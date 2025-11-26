#' Predict from a `qboost` model
#'
#' @param object A fitted `qboost` object.
#' @param newdata New data.frame or matrix of predictors. For models fit with a
#'   formula, a data.frame with the same predictor columns used at training
#'   time is expected.
#' @param ... Additional arguments passed to `stats::predict()`.
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

  if (!is.null(object$preprocess) && identical(object$preprocess$type, "formula")) {
    terms_obj <- object$preprocess$terms
    if (is.null(terms_obj)) {
      stop("Terms information missing; cannot build design matrix for prediction.", call. = FALSE)
    }
    terms_noy <- stats::delete.response(terms_obj)
    nd <- newdata
    if (!is.data.frame(nd)) {
      nd <- as.data.frame(nd)
    }
    mf <- stats::model.frame(
      terms_noy,
      nd,
      na.action = stats::na.pass,
      xlev = object$preprocess$xlevels
    )
    newdata <- stats::model.matrix(
      terms_noy,
      mf,
      contrasts.arg = object$preprocess$contrasts
    )
  } else if (!is.matrix(newdata)) {
    newdata <- data.matrix(newdata)
  }

  .lgb_predict(object$model, newdata)
}
