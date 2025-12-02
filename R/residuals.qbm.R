#' Residuals method for qbm
#'
#' Extracts residuals from a fitted qbm model. Residuals are computed as
#' the difference between observed values and fitted quantile predictions.
#'
#' @param object A qbm object
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of residuals (y - fitted)
#' @export
residuals.qbm <- function(object, ...) {
  if (!inherits(object, "qbm")) {
    stop("`object` must be a qbm model.", call. = FALSE)
  }
  
  object$training$y - object$training$fitted
}
