#' Feature importance for a `qrb` model
#'
#' @param object A fitted `qrb` model.
#' @param ... Additional arguments passed to `lightgbm::lgb.importance()`.
#'
#' @return A tibble of feature importances with columns `feature`, `gain`, `cover`, `freq`.
#' @export
importance.qrb <- function(object, ...) {
  if (!inherits(object, "qrb")) {
    stop("`object` must be a qrb model.", call. = FALSE)
  }
  tidy_importance(object$model, ...)
}
