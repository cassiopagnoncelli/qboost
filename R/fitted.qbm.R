#' Fitted values method for qbm
#'
#' Returns the fitted values from training for the qbm model.
#'
#' @param object A qbm object
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of fitted values
#' @export
fitted.qbm <- function(object, ...) {
  if (!inherits(object, "qbm")) {
    stop("`object` must be a qbm model.", call. = FALSE)
  }
  
  object$training$fitted
}
