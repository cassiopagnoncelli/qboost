#' Feature importance for a `qboost` model
#'
#' @param object A fitted `qboost` model.
#' @param ... Additional arguments passed to `lightgbm::lgb.importance()`.
#'
#' @return A tibble of feature importances with columns `feature`, `gain`, `cover`, `freq`.
#' @export
importance.qboost <- function(object, ...) {
  if (!inherits(object, "qboost")) {
    stop("`object` must be a qboost model.", call. = FALSE)
  }
  tidy_importance(object$model, ...)
}
