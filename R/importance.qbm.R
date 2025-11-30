#' Feature importance for a `qbm` model
#'
#' @param object A fitted `qbm` model.
#' @param ... Additional arguments passed to `lightgbm::lgb.importance()`.
#'
#' @return A tibble of feature importances with columns `feature`, `gain`, `cover`, `freq`.
#' @export
importance.qbm <- function(object, ...) {
  if (!inherits(object, "qbm")) {
    stop("`object` must be a qbm model.", call. = FALSE)
  }
  tidy_importance(object$model, ...)
}
