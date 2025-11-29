#' Residuals method for qevt
#'
#' @param object A qevt object
#' @param tau Quantile level at which to compute residuals (default 0.5 for median).
#'   If the specified tau is not in taus_full, the closest available tau is used.
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of residuals (observed - predicted at specified quantile)
#' @export
residuals.qevt <- function(object, tau = 0.5, ...) {
  if (is.null(object$train_x) || is.null(object$train_y)) {
    stop("Training data not available in model object.", call. = FALSE)
  }
  
  # Get fitted values
  preds <- predict(object, object$train_x)
  
  # Find closest tau if specified tau not available
  if (!(tau %in% preds$taus)) {
    tau_idx <- which.min(abs(preds$taus - tau))
    tau_actual <- preds$taus[tau_idx]
    message(sprintf("Specified tau=%.4f not available, using closest tau=%.4f", 
                    tau, tau_actual))
    tau <- tau_actual
  }
  
  # Get predictions at specified quantile
  tau_col <- which(preds$taus == tau)
  y_pred <- preds$monotone[, tau_col]
  
  # Compute residuals
  return(object$train_y - y_pred)
}
