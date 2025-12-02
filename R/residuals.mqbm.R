#' Residuals method for mqbm
#'
#' Extracts residuals from a fitted mqbm model. Residuals are computed as
#' the difference between observed values and fitted quantile predictions
#' for each symbol-specific model.
#'
#' @param object A mqbm object
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of residuals (y - fitted) in the original training data order
#' @export
residuals.mqbm <- function(object, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  
  # Initialize residuals vector with same length as training data
  residuals_vals <- numeric(object$data_info$n)
  
  # Get residuals from each symbol-specific model
  for (sym in object$symbols) {
    idx <- object$symbol_info[[sym]]$indices
    # Each qbm model stores y and fitted values in training
    y_sym <- object$models[[sym]]$training$y
    fitted_sym <- object$models[[sym]]$training$fitted
    residuals_vals[idx] <- y_sym - fitted_sym
  }
  
  residuals_vals
}
