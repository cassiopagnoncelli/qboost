#' Probability Integral Transform for qevt
#'
#' @param object A qevt object
#' @param ... Additional arguments (not used)
#'
#' @return Vector of PIT values
#' @keywords internal
pit_qevt <- function(object, ...) {
  if (!inherits(object, "qevt")) {
    stop("`object` must be a qevt model.", call. = FALSE)
  }
  
  if (is.null(object$train_x) || is.null(object$train_y)) {
    stop("Training data not available in model object.", call. = FALSE)
  }
  
  # Get predictions
  preds <- predict(object, object$train_x)
  n <- length(object$train_y)
  taus <- preds$taus
  pit <- numeric(n)
  
  for (i in seq_len(n)) {
    qgrid <- preds$monotone[i, ]
    y_obs <- object$train_y[i]
    
    # Monotonic interpolation to get PIT value
    pit_val <- tryCatch(
      stats::approx(qgrid, taus, xout = y_obs, rule = 2)$y,
      error = function(e) NA_real_
    )
    
    # Ensure PIT is within [0, 1]
    pit[i] <- if (!is.na(pit_val)) pmax(0, pmin(1, pit_val)) else NA_real_
  }
  
  return(pit)
}

#' PIT Plot for qevt
#'
#' @param object A qevt object
#' @param breaks Number of histogram breaks (default 40)
#' @param ... Additional arguments passed to hist
#'
#' @return NULL (invisibly)
#' @keywords internal
pit_plot_qevt <- function(object, breaks = 40, ...) {
  if (!inherits(object, "qevt")) {
    stop("`object` must be a qevt model.", call. = FALSE)
  }
  
  # Compute PIT values
  pit <- pit_qevt(object)
  
  # Remove NAs for plotting
  pit <- pit[!is.na(pit)]
  
  if (length(pit) < 2) {
    warning("Insufficient valid PIT values for plotting")
    return(invisible(NULL))
  }
  
  # Create histogram
  hist(pit,
       breaks = breaks,
       main = "PIT Histogram (Calibration Check)",
       xlab = "PIT Value",
       ylab = "Density",
       freq = FALSE,
       col = "lightblue",
       border = "white",
       ...
  )
  
  # Add uniform reference line
  abline(h = 1, col = "red", lwd = 2, lty = 2)
  text(0.5, 1.1, "Uniform (ideal)", col = "red", cex = 0.8)
  
  invisible(NULL)
}

#' QQ Plot for qevt
#'
#' @param object A qevt object
#' @param probs Probability grid for QQ plot (default seq(0.05, 0.95, 0.05))
#' @param ... Additional arguments (not used)
#'
#' @return NULL (invisibly)
#' @keywords internal
qq_plot_qevt <- function(object, probs = seq(0.05, 0.95, by = 0.05), ...) {
  if (!inherits(object, "qevt")) {
    stop("`object` must be a qevt model.", call. = FALSE)
  }
  
  if (is.null(object$train_x) || is.null(object$train_y)) {
    stop("Training data not available in model object.", call. = FALSE)
  }
  
  # Get median predictions
  preds <- predict(object, object$train_x)
  median_idx <- which.min(abs(preds$taus - 0.5))
  y_pred <- preds$monotone[, median_idx]
  y_obs <- object$train_y
  
  # Compute QQ data
  qq_data <- data.frame(
    prob = probs,
    empirical = stats::quantile(y_obs, probs, na.rm = TRUE, names = FALSE),
    predicted = stats::quantile(y_pred, probs, na.rm = TRUE, names = FALSE)
  )
  
  # Create QQ plot
  plot(qq_data$empirical, qq_data$predicted,
       main = "QQ Plot (Observed vs Predicted Quantiles)",
       xlab = "Observed Quantiles",
       ylab = "Predicted Quantiles",
       pch = 16,
       col = "steelblue",
       cex = 1.2
  )
  
  # Add 1:1 reference line
  abline(0, 1, col = "red", lwd = 2, lty = 2)
  grid()
  
  # Add correlation
  cor_val <- stats::cor(qq_data$empirical, qq_data$predicted, use = "complete.obs")
  text(min(qq_data$empirical, na.rm = TRUE), 
       max(qq_data$predicted, na.rm = TRUE),
       sprintf("r = %.3f", cor_val),
       adj = c(0, 1), cex = 0.9)
  
  invisible(NULL)
}
