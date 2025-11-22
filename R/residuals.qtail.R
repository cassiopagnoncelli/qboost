#' Residuals method for qtail
#'
#' @param object A qtail object
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of residuals
#' @export
residuals.qtail <- function(object, ...) {
  # Residuals relative to stacked prediction (before EVT)
  q_stack <- predict.qtail(object, object$x, type = "stack")
  return(object$y - q_stack)
}
