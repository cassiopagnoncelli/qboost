#' Coefficients method for qevt
#'
#' @param object A qevt object
#' @param ... Additional arguments (not used)
#'
#' @return Named numeric vector of GPD parameters (xi and beta)
#' @export
coef.qevt <- function(object, ...) {
  # Return GPD shape and scale parameters
  c(
    xi = object$gpd$xi,
    beta = object$gpd$beta
  )
}
