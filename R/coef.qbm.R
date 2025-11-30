#' Coefficients (feature importance) for `qbm`
#'
#' @param object A fitted `qbm` model.
#' @param ... Additional arguments passed to `importance.qbm()`.
#'
#' @return A tibble of feature importances.
#' @export
#' @method coef qbm
coef.qbm <- function(object, ...) {
  importance.qbm(object, ...)
}
