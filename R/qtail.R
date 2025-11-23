# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fit qtail model
#'
#' @param x Feature matrix
#' @param y Target vector
#' @param taus Quantile levels to fit
#' @param tail Tail type: "upper" or "lower"
#' @param threshold_tau Threshold quantile for EVT
#' @param params Parameters passed to fit_qboost
#' @param verbose Logical; if TRUE, print progress messages
#' @param ... Additional arguments passed to fit_qboost
#'
#' @return An object of class "qtail"
#' @export
qtail <- function(x, y,
                  taus = NULL,
                  tail = c("upper", "lower"),
                  threshold_tau = NULL,
                  params = list(),
                  verbose = FALSE,
                  ...) {
  
  tail <- match.arg(tail)
  
  # Set defaults based on tail
  if (is.null(taus)) {
    if (tail == "upper") {
      taus <- c(.95, .975, .99, .995, .997, .9985, .999, .9993, .9998)
    } else {
      taus <- c(.05, .025, .01, .005, .003, .0015, .001, .0007, .0002)
    }
  }
  
  if (is.null(threshold_tau)) {
    if (tail == "upper") {
      threshold_tau <- 0.997
    } else {
      threshold_tau <- 0.01
    }
  }
  
  # Check threshold_tau is in taus
  if (!threshold_tau %in% taus) {
    stop("threshold_tau must be one of the values in taus")
  }
  
  # Set tau_target
  if (tail == "upper") {
    tau_target <- max(taus)
  } else {
    tau_target <- min(taus)
  }
  
  # Calculate total steps for progress reporting
  K <- length(taus)
  total_steps <- K + 4  # preprocessing + K models + stacking matrix + ridge + EVT
  current_step <- 0
  
  # Track overall start time
  if (verbose){
    overall_start <- Sys.time()
  }
  
  # Step 1: Preprocessing
  current_step <- current_step + 1
  if (verbose) {
    message(sprintf("[%d/%d] Preprocessing data...", current_step, total_steps))
    step_start <- Sys.time()
  }
  
  # Remove NA rows
  if (is.matrix(x) || is.data.frame(x)) {
    complete_rows <- complete.cases(x, y)
  } else {
    complete_rows <- !is.na(y)
  }
  
  if (!all(complete_rows)) {
    n_removed <- sum(!complete_rows)
    warning(sprintf("Removed %d rows with NA values", n_removed))
    x <- x[complete_rows, , drop = FALSE]
    y <- y[complete_rows]
  }
  
  if (verbose) {
    elapsed <- round(as.numeric(difftime(Sys.time(), step_start, units = "secs")), 2)
    message(sprintf("  → Completed in %.2fs", elapsed))
  }
  
  # Step A: Fit qboost models for each tau
  models <- list()
  for (j in seq_along(taus)) {
    tau <- taus[j]
    current_step <- current_step + 1
    if (verbose) {
      message(sprintf("[%d/%d] Fitting qboost for tau=%g...", current_step, total_steps, tau))
      step_start <- Sys.time()
    }
    
    models[[as.character(tau)]] <- qboost(
      x,
      y,
      tau = tau,
      nrounds = params$nrounds %||% 500,
      nfolds = params$nfolds %||% 5, 
      early_stopping_rounds = params$early_stopping_rounds %||% 50,
      params = params, 
      ...
    )
    
    if (verbose) {
      elapsed <- round(as.numeric(difftime(Sys.time(), step_start, units = "secs")), 2)
      message(sprintf("  → Completed in %.2fs", elapsed))
    }
  }
  
  # Step B: Build stacking design matrix
  current_step <- current_step + 1
  if (verbose) {
    message(sprintf("[%d/%d] Building stacking design matrix...", current_step, total_steps))
    step_start <- Sys.time()
  }
  n <- length(y)
  K <- length(taus)
  Z_raw <- matrix(0, nrow = n, ncol = K)
  colnames(Z_raw) <- as.character(taus)
  
  for (j in seq_along(taus)) {
    tau <- taus[j]
    Z_raw[, j] <- predict(models[[as.character(tau)]], x)
  }
  
  Z <- apply_pava_monotonicity(Z_raw, taus)
  
  if (verbose) {
    elapsed <- round(as.numeric(difftime(Sys.time(), step_start, units = "secs")), 2)
    message(sprintf("  → Completed in %.2fs", elapsed))
  }
  
  # Step C: Fit elastic net stacking model
  current_step <- current_step + 1
  if (verbose) {
    message(sprintf("[%d/%d] Fitting elastic net (stacking layer)...", current_step, total_steps))
    step_start <- Sys.time()
  }
  
  lambda_stack <- 0.01
  alpha_stack <- 0
  enet_fit <- glmnet::glmnet(
    Z,
    y,
    alpha = alpha_stack,
    lambda = lambda_stack,
    family = "gaussian",
    intercept = FALSE
  )
  coef_obj <- c(0, as.vector(enet_fit$beta[, 1]))
  
  stack <- list(
    coef = as.vector(coef_obj),
    lambda = lambda_stack,
    alpha = alpha_stack,
    glmnet_fit = enet_fit
  )
  names(stack$coef) <- c("(Intercept)", colnames(Z))
  
  if (verbose) {
    elapsed <- round(as.numeric(difftime(Sys.time(), step_start, units = "secs")), 2)
    message(sprintf("  → Completed in %.2fs", elapsed))
  }
  
  # Step D: EVT (GPD) tail fitting
  current_step <- current_step + 1
  if (verbose) {
    message(sprintf("[%d/%d] Fitting GPD tail model...", current_step, total_steps))
    step_start <- Sys.time()
  }
  q_thresh_hat <- predict(models[[as.character(threshold_tau)]], x)
  r <- y - q_thresh_hat
  
  if (tail == "upper") {
    e <- pmax(r, 0)
  } else {
    e <- pmax(-r, 0)
  }
  
  # Filter exceedances and remove any NA/Inf values
  e_exc <- e[e > 0 & is.finite(e)]
  
  # Default GPD parameters
  xi <- 0.1
  beta <- 1.0
  evfit <- NULL
  
  if (length(e_exc) < 10) {
    warning("Very few exceedances for EVT fitting (", length(e_exc), " < 10), using default GPD parameters")
    beta <- ifelse(length(e_exc) > 0, stats::sd(e_exc), 1.0)
  } else {
    # Try to fit GPD, fall back to defaults on error
    fit_success <- tryCatch({
      # Create clean numeric vector
      exc_vec <- as.numeric(e_exc)
      
      # Validate data
      if (length(exc_vec) < 10 || any(!is.finite(exc_vec)) || any(exc_vec <= 0)) {
        stop("Invalid exceedances data")
      }
      
      # Create proper data frame for evgam
      evt_data <- data.frame(y = exc_vec)
      
      # Fit evgam with simpler formula syntax
      evfit <<- evgam::evgam(y ~ 1, data = evt_data, family = "gpd")
      
      # Extract xi (shape) and beta (scale)
      coef_evt <- stats::coef(evfit)
      xi <<- coef_evt[1]
      beta <<- exp(coef_evt[2])
      
      TRUE
    }, error = function(err) {
      if (verbose) {
        message("  Warning: EVT fitting failed (", err$message, "), using default GPD parameters")
      } else {
        warning("EVT fitting failed, using default GPD parameters")
      }
      # Keep defaults
      xi <<- 0.1
      beta <<- ifelse(length(e_exc) > 0 && is.finite(stats::sd(e_exc)), stats::sd(e_exc), 1.0)
      evfit <<- NULL
      FALSE
    })
  }
  
  if (verbose) {
    elapsed <- round(as.numeric(difftime(Sys.time(), step_start, units = "secs")), 2)
    message(sprintf("  → Completed in %.2fs", elapsed))
  }
  
  evt <- list(
    xi = xi,
    beta = beta,
    threshold_tau = threshold_tau,
    tail = tail,
    n_exceedances = length(e_exc),
    evfit = evfit
  )
  
  # Build return object
  object <- list(
    taus = taus,
    tau_target = tau_target,
    threshold_tau = threshold_tau,
    tail = tail,
    models = models,
    stack = stack,
    evt = evt,
    call = match.call(),
    x = x,
    y = y,
    training_y = y,
    n = n
  )
  
  class(object) <- "qtail"
  
  # Completion message
  if (verbose) {
    total_elapsed <- round(as.numeric(difftime(Sys.time(), overall_start, units = "secs")), 2)
    message(
      sprintf(
        "✓ qtail model complete (%s tail, target tau=%g, %d exceedances) [Total: %.2fs]", 
        tail,
        tau_target,
        length(e_exc),
        total_elapsed
      )
    )
  }
  
  return(object)
}
