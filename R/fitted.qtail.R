#' Fitted values method for qtail
#'
#' @param object A qtail object
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of fitted values
#' @export
fitted.qtail <- function(object, ...) {
  # Final EVT-adjusted predictions for training data
  return(predict.qtail(object, object$x, type = "final"))
}
