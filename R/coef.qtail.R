#' Coefficients method for qtail
#'
#' @param object A qtail object
#' @param ... Additional arguments (not used)
#'
#' @return Named numeric vector of stacking coefficients
#' @export
coef.qtail <- function(object, ...) {
  return(object$stack$coef)
}
