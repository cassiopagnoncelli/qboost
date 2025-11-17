#' Coefficients (feature importance) for `qboost`
#'
#' @param object A fitted `qboost` model.
#' @param ... Additional arguments passed to `importance.qboost()`.
#'
#' @return A tibble of feature importances.
#' @export
#' @method coef qboost
coef.qboost <- function(object, ...) {
  importance.qboost(object, ...)
}
