#' Fitted values method for qevt
#'
#' @param object A qevt object
#' @param type Type of fitted values to return: "monotone" (default) for PAVA-adjusted
#'   quantiles, or "raw" for unadjusted quantiles
#' @param ... Additional arguments (not used)
#'
#' @return Matrix of fitted quantile values with rows corresponding to training
#'   observations and columns corresponding to quantile levels (taus_full)
#' @export
fitted.qevt <- function(object, type = c("monotone", "raw"), ...) {
  type <- match.arg(type)

  if (is.null(object$train_x)) {
    stop("Training data not available in model object.", call. = FALSE)
  }

  # Get predictions for training data
  preds <- predict(object, object$train_x)

  # Return requested type
  if (type == "monotone") {
    return(preds$monotone)
  } else {
    return(preds$raw)
  }
}
