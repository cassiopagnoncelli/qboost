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
      taus <- c(0.95, 0.97, 0.99, 0.993, 0.999)
    } else {
      taus <- c(0.05, 0.03, 0.01, 0.007, 0.001)
    }
  }
  
  if (is.null(threshold_tau)) {
    if (tail == "upper") {
      threshold_tau <- 0.99
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
  
  # Step 1: Preprocessing
  current_step <- current_step + 1
  if (verbose) message(sprintf("[%d/%d] Preprocessing data...", current_step, total_steps))
  
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
  
  # Step A: Fit qboost models for each tau
  models <- list()
  for (j in seq_along(taus)) {
    tau <- taus[j]
    current_step <- current_step + 1
    if (verbose) message(sprintf("[%d/%d] Fitting qboost for tau=%g...", current_step, total_steps, tau))
    
    models[[as.character(tau)]] <- qboost(x, y, tau = tau, nrounds = params$nrounds %||% 500, 
                                          nfolds = params$nfolds %||% 5, 
                                          early_stopping_rounds = params$early_stopping_rounds %||% 50,
                                          params = params, ...)
  }
  
  # Step B: Build stacking design matrix
  current_step <- current_step + 1
  if (verbose) message(sprintf("[%d/%d] Building stacking design matrix...", current_step, total_steps))
  n <- length(y)
  K <- length(taus)
  Z <- matrix(0, nrow = n, ncol = K)
  colnames(Z) <- as.character(taus)
  
  for (j in seq_along(taus)) {
    tau <- taus[j]
    Z[, j] <- predict(models[[as.character(tau)]], x)
  }
  
  # Step C: Fit ridge regression (stacking model)
  current_step <- current_step + 1
  if (verbose) message(sprintf("[%d/%d] Fitting ridge regression (stacking layer)...", current_step, total_steps))
  
  cv <- glmnet::cv.glmnet(Z, y, alpha = 0, family = "gaussian")
  coef_obj <- coef(cv, s = "lambda.min")
  
  stack <- list(
    coef = as.vector(coef_obj),
    lambda = cv$lambda.min,
    glmnet_fit = cv
  )
  names(stack$coef) <- c("(Intercept)", colnames(Z))
  
  # Step D: EVT (GPD) tail fitting
  current_step <- current_step + 1
  if (verbose) message(sprintf("[%d/%d] Fitting GPD tail model...", current_step, total_steps))
  q_thresh_hat <- predict(models[[as.character(threshold_tau)]], x)
  r <- y - q_thresh_hat
  
  if (tail == "upper") {
    e <- pmax(r, 0)
  } else {
    e <- pmax(-r, 0)
  }
  
  # Filter exceedances
  e_exc <- e[e > 0]
  
  if (length(e_exc) < 10) {
    warning("Very few exceedances for EVT fitting, using default GPD parameters")
    # Use default/simple GPD parameters when insufficient data
    xi <- 0.1
    beta <- stats::sd(e_exc)
    evfit <- NULL
  } else {
    evfit <- evgam::evgam(list(e ~ 1),
                          data = data.frame(e = e_exc),
                          family = "gpd")
    
    # Extract xi (shape) and beta (scale)
    coef_evt <- coef(evfit)
    xi <- coef_evt[1]
    beta <- exp(coef_evt[2])
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
    message(sprintf("✓ qtail model complete (%s tail, target tau=%g, %d exceedances)", 
                    tail, tau_target, length(e_exc)))
  }
  
  return(object)
}
