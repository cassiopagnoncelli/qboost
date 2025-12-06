#' Predict quantiles from a qbm model
#'
#' Generates quantile predictions at the specified tau level for new observations.
#' Automatically handles formula preprocessing and ensures feature compatibility
#' with the training data.
#'
#' @param object A fitted qbm model object returned by \code{\link{qbm}}.
#' @param newdata A data.frame or matrix of predictor variables. For models fit
#'   with a formula interface, \code{newdata} must be a data.frame containing
#'   all predictor variables used in training (factor levels, transformations,
#'   and interactions are handled automatically). For models fit with the matrix
#'   interface, \code{newdata} should have the same number and order of columns
#'   as the training data.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of length \code{nrow(newdata)} containing predicted
#'   quantile values at the tau level specified during model training.
#'
#' @details
#' The prediction process preserves the preprocessing applied during training:
#' \itemize{
#'   \item For formula models: factor encoding, interactions, and transformations
#'     are automatically applied to \code{newdata}
#'   \item For matrix models: features are used as-is but coerced to numeric matrix
#'   \item Missing values are preserved in predictions (result in \code{NA})
#' }
#'
#' @seealso \code{\link{qbm}}, \code{\link{fitted.qbm}}, \code{\link{residuals.qbm}}
#'
#' @examples
#' \dontrun{
#' # Train model
#' df <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
#' df$y <- df$x1 * 0.5 + rnorm(100)
#' fit <- qbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50)
#'
#' # Predict on new data
#' newdata <- data.frame(x1 = c(0, 1, -1), x2 = c(0, 0, 1))
#' predictions <- predict(fit, newdata)
#' print(predictions)
#'
#' # Compare multiple quantiles
#' fit_lower <- qbm(y ~ x1 + x2, data = df, tau = 0.1, nrounds = 50)
#' fit_upper <- qbm(y ~ x1 + x2, data = df, tau = 0.9, nrounds = 50)
#'
#' data.frame(
#'   lower = predict(fit_lower, newdata),
#'   median = predict(fit, newdata),
#'   upper = predict(fit_upper, newdata)
#' )
#' }
#'
#' @export
#' @method predict qbm
predict.qbm <- function(object, newdata, ...) {
  if (!inherits(object, "qbm")) {
    stop("`object` must be a qbm model.", call. = FALSE)
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
