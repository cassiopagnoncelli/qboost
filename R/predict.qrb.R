#' Predict from a `qrb` model
#'
#' @param object A fitted `qrb` object.
#' @param newdata New data.frame or matrix of predictors. For models fit with a
#'   formula, a data.frame with the same predictor columns used at training
#'   time is expected.
#' @param ... Additional arguments passed to `stats::predict()`.
#'
#' @return Numeric vector of predicted quantiles.
#' @export
#' @method predict qrb
predict.qrb <- function(object, newdata, ...) {
  if (!inherits(object, "qrb")) {
    stop("`object` must be a qrb model.", call. = FALSE)
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
    # Remove intercept column if present (consistent with training)
    if ("(Intercept)" %in% colnames(newdata)) {
      newdata <- newdata[, colnames(newdata) != "(Intercept)", drop = FALSE]
    }
  } else if (!is.matrix(newdata)) {
    newdata <- data.matrix(newdata)
  }

  # Ensure plain numeric matrix
  if (is.matrix(newdata)) {
    feature_names <- colnames(newdata)
    newdata <- matrix(as.numeric(newdata), nrow = nrow(newdata), ncol = ncol(newdata))
    colnames(newdata) <- feature_names
  }

  .lgb_predict(object$model, newdata)
}
