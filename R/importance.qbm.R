#' Extract feature importance from a model
#'
#' Generic function for extracting feature importance from fitted models.
#'
#' @param object A fitted model object.
#' @param ... Additional arguments passed to methods.
#'
#' @return A tibble of feature importances.
#' @export
importance <- function(object, ...) {
  UseMethod("importance")
}

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
