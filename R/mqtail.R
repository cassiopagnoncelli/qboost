#' Fit multi-group extreme tail quantile models
#'
#' Trains multiple \code{\link{mqbm}} quantile models (one per tau), then fits
#' GPD tail models per group. This enables extreme quantile prediction with
#' heterogeneous tail behavior across groups.
#'
#' @param ... Passed to \code{\link{mqbm}}.
#' @param multiplexer Character string specifying the column name for grouping.
#'   Passed directly to \code{\link{mqbm}}. Required parameter.
#' @param taus Numeric vector of quantile levels. If \code{NULL}, defaults
#'   based on \code{tail} direction.
#' @param tail Character string: \code{"upper"} or \code{"lower"}. Default is \code{"upper"}.
#' @param threshold_tau Quantile level for EVT threshold. Must be in \code{taus}.
#'   If \code{NULL}, defaults to 0.99 for upper tail or 0.01 for lower tail.
#' @param nrounds Maximum number of boosting rounds. Default is 500.
#' @param nfolds Number of CV folds for early stopping. Default is 5.
#' @param early_stopping_rounds Early stopping rounds. Default is 50.
#' @param seed Random seed. Default is 1.
#' @param params Additional named list of parameters for \code{\link{lightgbm}}.
#' @param verbose Logical; print progress messages. Default is \code{FALSE}.
#' @param train_idx Optional training indices. Passed to \code{\link{mqbm}}.
#' @param val_idx Optional validation indices. Passed to \code{\link{mqbm}}.
#' @param folds Optional fold structure. Passed to \code{\link{mqbm}}.
#'
#' @return Object of class \code{mqtail} containing:
#'   \item{mqbm_models}{Named list of \code{\link{mqbm}} objects, one per tau}
#'   \item{multiplexer_values}{Unique group identifiers from first mqbm model}
#'   \item{multiplexer_info}{Info per group from first mqbm model}
#'   \item{evt_models}{GPD parameters per group}
#'   \item{taus}{Fitted quantile levels}
#'   \item{tau_target}{Target extreme quantile}
#'   \item{threshold_tau}{EVT threshold level}
#'   \item{tail}{Tail direction}
#'   \item{multiplexer}{Grouping column name}
#'   \item{data_info}{Dataset dimensions}
#'
#' @export
mqtail <- function(...,
                   multiplexer,
                   taus = NULL,
                   tail = c("upper", "lower"),
                   threshold_tau = NULL,
                   nrounds = 500,
                   nfolds = 5,
                   early_stopping_rounds = 50,
                   seed = 1,
                   params = list(),
                   verbose = FALSE,
                   train_idx = NULL,
                   val_idx = NULL,
                   folds = NULL) {
  start_time <- Sys.time()
  tail <- match.arg(tail)

  # Set defaults
  taus <- taus %||% if (tail == "upper") {
    c(0.95, 0.97, 0.99, 0.995)
  } else {
    c(0.05, 0.03, 0.01, 0.005)
  }

  threshold_tau <- threshold_tau %||% if (tail == "upper") 0.99 else 0.01
  
  if (!threshold_tau %in% taus) {
    stop("`threshold_tau` must be one of the values in `taus`", call. = FALSE)
  }

  tau_target <- if (tail == "upper") max(taus) else min(taus)

  if (verbose) {
    message("=== Starting mqtail Model Fitting ===")
    message(sprintf("Tail: %s", tail))
    message(sprintf("Taus: %s", paste(taus, collapse = ", ")))
    message(sprintf("Threshold tau: %.4f", threshold_tau))
    message(sprintf("Target tau: %.4f", tau_target))
  }

  # Train one mqbm per tau
  mqbm_models <- list()
  
  if (verbose) {
    message(sprintf("\n=== Fitting %d mqbm Models (one per tau) ===", length(taus)))
  }
  
  for (j in seq_along(taus)) {
    tau <- taus[j]
    
    if (verbose) {
      message(sprintf("[%d/%d] Fitting mqbm for tau=%.4f...", j, length(taus), tau))
    }

    # Call mqbm with all original arguments plus current tau
    mqbm_models[[as.character(tau)]] <- mqbm(
      ...,
      multiplexer = multiplexer,
      tau = tau,
      nrounds = nrounds,
      nfolds = nfolds,
      early_stopping_rounds = early_stopping_rounds,
      seed = seed,
      params = params,
      train_idx = train_idx,
      val_idx = val_idx,
      folds = folds
    )
    
    if (verbose) {
      message(sprintf("  Completed tau=%.4f", tau))
    }
  }

  # Get multiplexer values from first mqbm model
  first_mqbm <- mqbm_models[[1]]
  multiplexer_values <- first_mqbm$multiplexer_values
  multiplexer_info <- first_mqbm$multiplexer_info
  
  # Fit GPD per multiplexer value
  if (verbose) {
    message(sprintf("\n=== Fitting GPD Models for %d Multiplexer Gates ===", length(multiplexer_values)))
    message(sprintf("Using threshold tau: %.4f", threshold_tau))
  }
  
  evt_models <- list()
  thresh_mqbm <- mqbm_models[[as.character(threshold_tau)]]
  
  for (val_idx in seq_along(multiplexer_values)) {
    val <- multiplexer_values[val_idx]
    
    if (verbose) {
      message(sprintf("[%d/%d] Processing multiplexer gate: %s", val_idx, length(multiplexer_values), val))
    }
    
    # Get training data for this value
    idx <- thresh_mqbm$multiplexer_info[[val]]$indices
    y_grp <- thresh_mqbm$training$y[idx]
    
    # Get threshold predictions
    fitted_thresh <- fitted(thresh_mqbm$models[[val]])
    
    # Compute exceedances
    r <- y_grp - fitted_thresh
    e <- if (tail == "upper") pmax(r, 0) else pmax(-r, 0)
    e_exc <- e[e > 0 & is.finite(e)]
    
    # Fit GPD
    xi <- 0.1
    beta <- 1.0
    
    if (length(e_exc) < 10) {
      if (verbose) {
        message(sprintf("  WARNING: Only %d exceedances, using default GPD parameters", length(e_exc)))
      }
      beta <- ifelse(length(e_exc) > 0, stats::sd(e_exc), 1.0)
    } else {
      if (verbose) {
        message(sprintf("  Fitting GPD with %d exceedances", length(e_exc)))
      }
      tryCatch({
        evt_data <- data.frame(y = as.numeric(e_exc))
        evfit <- evgam::evgam(y ~ 1, data = evt_data, family = "gpd")
        coef_evt <- stats::coef(evfit)
        xi <- coef_evt[1]
        beta <- exp(coef_evt[2])
        if (verbose) {
          message(sprintf("  GPD fit: xi=%.4f, beta=%.4f", xi, beta))
        }
      }, error = function(e) {
        if (verbose) message(sprintf("  GPD fit failed, using defaults (xi=%.4f, beta=%.4f)", xi, beta))
        beta <<- stats::sd(e_exc)
      })
    }
    
    evt_models[[val]] <- list(
      xi = xi,
      beta = beta,
      n_exceedances = length(e_exc)
    )
  }

  end_time <- Sys.time()
  elapsed <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  if (verbose) {
    message(sprintf("\n=== mqtail Fitting Complete ==="))
    message(sprintf("Total time: %.2f seconds", elapsed))
    message(sprintf("Total models fit: %d mqbm models + %d GPD models", length(taus), length(multiplexer_values)))
  }

  out <- list(
    mqbm_models = mqbm_models,
    multiplexer_values = multiplexer_values,
    multiplexer_info = multiplexer_info,
    evt_models = evt_models,
    taus = taus,
    tau_target = tau_target,
    threshold_tau = threshold_tau,
    tail = tail,
    multiplexer = multiplexer,
    data_info = first_mqbm$data_info,
    timings = list(
      start = start_time,
      end = end_time,
      elapsed = as.numeric(difftime(end_time, start_time, units = "secs"))
    )
  )

  class(out) <- "mqtail"
  out
}
