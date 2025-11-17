#' Feature importance for a `qboost` model
#'
#' @param object A fitted `qboost` model.
#' @param ... Additional arguments passed to `lightgbm::lgb.importance()`.
#'
#' @return A data frame of feature importances.
#' @export
importance.qboost <- function(object, ...) {
  if (!inherits(object, "qboost")) {
    stop("`object` must be a qboost model.", call. = FALSE)
  }
  lightgbm::lgb.importance(object$model, ...)
}
