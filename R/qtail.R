# Null-coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Fit qtail model
#'
#' Provides a clean interface for extreme quantile tail modeling using
#' LightGBM quantile regression combined with Extreme Value Theory (EVT).
#' Accepts either a formula + data or an `x`/`y` pair.
#'
#' @param ... Either a formula and optional `data` argument or an `x`/`y` pair
#'   followed by additional arguments.
#' @param taus Quantile levels to fit. If NULL, defaults based on tail type.
#' @param tail Tail type: "upper" or "lower"
#' @param threshold_tau Threshold quantile for EVT. If NULL, defaults based on tail type.
#' @param params Parameters passed to qboost (nrounds, nfolds, etc.)
#' @param verbose Logical; if TRUE, print progress messages
#'
#' @return An object of class "qtail"
#' @export
#' 
#' @examples
#' \dontrun{
#' # Using formula interface
#' set.seed(123)
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200)
#' )
#' df$y <- df$x1 * 2 + df$x2 * 0.5 + rt(200, df = 3) * 2
#' 
#' fit <- qtail(y ~ x1 + x2, data = df, tail = "upper", 
#'              params = list(nrounds = 50, nfolds = 3))
#' 
#' # Using x/y interface
#' x <- as.matrix(df[, c("x1", "x2")])
#' y <- df$y
#' fit <- qtail(x, y, tail = "upper", params = list(nrounds = 50, nfolds = 3))
#' }
qtail <- function(...,
                  taus = NULL,
                  tail = c("upper", "lower"),
                  threshold_tau = NULL,
                  params = list(),
                  verbose = FALSE) {
  
  tail <- match.arg(tail)
  
  # Parse inputs (formula or x/y)
  dots <- list(...)
  parsed <- .parse_qtail_inputs(dots)
  
  x <- parsed$x
  y <- as.numeric(parsed$y)
  extra_args <- parsed$extra_args
  
  if (!is.matrix(x)) {
    x <- data.matrix(x)
  }
  
  if (nrow(x) != length(y)) {
    stop("`x` and `y` must have compatible dimensions.", call. = FALSE)
  }
  
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
      params = params
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
    preds <- .lgb_predict(models[[as.character(tau)]]$model, data.matrix(x))
    # Replace NA/Inf values with the tau-th quantile of y as fallback
    if (any(!is.finite(preds))) {
      fallback <- stats::quantile(y, probs = tau, na.rm = TRUE)
      preds[!is.finite(preds)] <- fallback
    }
    Z_raw[, j] <- preds
  }
  
  Z <- apply_pava_monotonicity(Z_raw, taus)
  
  # Final check for any remaining NA/Inf values after monotonicity
  if (any(!is.finite(Z))) {
    for (j in seq_len(ncol(Z))) {
      if (any(!is.finite(Z[, j]))) {
        fallback <- stats::quantile(y, probs = taus[j], na.rm = TRUE)
        Z[!is.finite(Z[, j]), j] <- fallback
      }
    }
  }
  
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
  
  # Try to fit glmnet, but fall back to equal weights if zero variance
  enet_fit <- NULL
  coef_obj <- c(0, rep(1 / K, K))  # Initialize with equal weights
  
  fit_success <- tryCatch({
    enet_fit <- glmnet::glmnet(
      Z,
      y,
      alpha = alpha_stack,
      lambda = lambda_stack,
      family = "gaussian",
      intercept = FALSE
    )
    coef_obj <- c(0, as.vector(enet_fit$beta[, 1]))
    TRUE
  }, error = function(e) {
    if (grepl("zero variance", e$message, ignore.case = TRUE)) {
      # Use equal weights as fallback (already initialized)
      if (verbose) {
        message("  Note: Using equal weights (zero variance detected)")
      }
      FALSE
    } else {
      stop(e)
    }
  })
  
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
  q_thresh_hat <- .lgb_predict(models[[as.character(threshold_tau)]]$model, data.matrix(x))
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
    n = n,
    preprocess = parsed$preprocess
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

# Parse qtail inputs (formula or x/y) - similar to qboost
.parse_qtail_inputs <- function(dots) {
  if (length(dots) == 0) {
    stop("Provide either a formula or an `x`/`y` pair.", call. = FALSE)
  }

  nm <- names(dots)
  if (is.null(nm)) {
    nm <- rep("", length(dots))
  }
  nm[is.na(nm)] <- ""

  is_formula <- vapply(dots, inherits, logical(1), what = "formula")
  if (any(is_formula)) {
    formula_idx <- which(is_formula)[1]
    formula <- dots[[formula_idx]]

    data_idx <- integer(0)
    if ("data" %in% nm) {
      data_idx <- which(nm == "data")[1]
    } else {
      unnamed <- which(!nzchar(nm))
      unnamed <- unnamed[unnamed != formula_idx]
      if (length(unnamed) > 0) {
        data_idx <- unnamed[1]
      }
    }

    data <- if (length(data_idx) > 0) dots[[data_idx]] else NULL

    mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
    y <- stats::model.response(mf)
    mm <- stats::model.matrix(formula, mf)
    terms_obj <- stats::terms(mf)

    preprocess <- list(
      type = "formula",
      terms = terms_obj,
      xlevels = stats::.getXlevels(terms_obj, mf),
      contrasts = attr(mm, "contrasts"),
      formula = formula,
      feature_names = colnames(mm)
    )

    extra_idx <- setdiff(seq_along(dots), c(formula_idx, data_idx))
    extra_args <- if (length(extra_idx) > 0) dots[extra_idx] else list()

    return(list(
      x = mm,
      y = y,
      extra_args = extra_args,
      preprocess = preprocess
    ))
  }

  x_idx <- if ("x" %in% nm) which(nm == "x")[1] else 1L
  remaining <- setdiff(seq_along(dots), x_idx)
  if (length(remaining) == 0) {
    stop("`y` must be provided when using `x`/`y` inputs.", call. = FALSE)
  }
  y_idx <- if ("y" %in% nm) which(nm == "y")[1] else remaining[1]

  x <- dots[[x_idx]]
  y <- dots[[y_idx]]

  extra_idx <- setdiff(seq_along(dots), c(x_idx, y_idx))
  extra_args <- if (length(extra_idx) > 0) dots[extra_idx] else list()

  preprocess <- list(
    type = "xy",
    feature_names = colnames(x)
  )

  list(
    x = x,
    y = y,
    extra_args = extra_args,
    preprocess = preprocess
  )
}
