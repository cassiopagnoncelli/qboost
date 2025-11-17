#' Predict from a `qboost` model
#'
#' @param object A fitted `qboost` object.
#' @param newdata New data.frame or matrix of predictors.
#' @param ... Additional arguments passed to `predict.lgb.Booster()`.
#'
#' @return Numeric vector of predicted quantiles.
#' @export
#' @method predict qboost
predict.qboost <- function(object, newdata, ...) {
  if (!inherits(object, "qboost")) {
    stop("`object` must be a qboost model.", call. = FALSE)
  }
  if (missing(newdata)) {
    stop("`newdata` is required for prediction.", call. = FALSE)
  }
  if (!is.matrix(newdata)) {
    newdata <- data.matrix(newdata)
  }
  # Call the S3 `predict` method for `lgb.Booster` with positional `data`.
  stats::predict(object$model, newdata, ...)
}
