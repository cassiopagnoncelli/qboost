#' Out-of-sample Validation for qtail
#'
#' @param object A qtail object
#' @param x_new New feature matrix
#' @param y_new New response vector
#' @param taus Optional vector of quantile levels to evaluate
#' @param ... Additional arguments (not used)
#'
#' @return Data frame with validation metrics
#' @export
qtail_validate <- function(object, x_new, y_new, taus = NULL, ...) {
  if (!inherits(object, "qtail")) {
    stop("`object` must be a qtail model.", call. = FALSE)
  }

  if (is.null(taus)) {
    taus <- object$taus
  }

  n_new <- length(y_new)

  # Prepare result data frame
  result <- data.frame(
    tau = numeric(length(taus)),
    observed = numeric(length(taus)),
    qce = numeric(length(taus))
  )

  # Compute metrics for each tau
  for (i in seq_along(taus)) {
    tau <- taus[i]

    # Predict for this tau
    q_hat_new <- predict.qtail(object, x_new, type = "final", tau_override = tau)

    # Compute empirical coverage
    if (object$tail == "upper") {
      obs <- mean(y_new >= q_hat_new)
    } else {
      obs <- mean(y_new <= q_hat_new)
    }

    # QCE
    qce <- abs(obs - tau)

    result$tau[i] <- tau
    result$observed[i] <- obs
    result$qce[i] <- qce
  }

  # Compute PIT for new data
  pit <- numeric(n_new)

  for (i in seq_len(n_new)) {
    # Predict quantiles for this observation
    qgrid <- numeric(length(taus))
    for (j in seq_along(taus)) {
      qgrid[j] <- predict.qtail(object, x_new[i, , drop = FALSE],
        type = "final", tau_override = taus[j]
      )
    }

    # Monotonic interpolation
    if (object$tail == "lower") {
      qgrid <- rev(qgrid)
      taus_interp <- rev(taus)
    } else {
      taus_interp <- taus
    }

    # Interpolate with error handling
    pit_val <- tryCatch(
      stats::approx(qgrid, taus_interp, xout = y_new[i], rule = 2)$y,
      error = function(e) {
        # If interpolation fails, use NA
        NA_real_
      }
    )

    # Ensure PIT is within [0, 1]
    pit[i] <- if (!is.na(pit_val)) pmax(0, pmin(1, pit_val)) else NA_real_
  }

  # Add PIT statistics to result
  result$pit_mean <- mean(pit)
  result$pit_sd <- stats::sd(pit)

  return(result)
}
