#' Coefficients (feature importance) for `qrb`
#'
#' @param object A fitted `qrb` model.
#' @param ... Additional arguments passed to `importance.qrb()`.
#'
#' @return A tibble of feature importances.
#' @export
#' @method coef qrb
coef.qrb <- function(object, ...) {
  importance.qrb(object, ...)
}
