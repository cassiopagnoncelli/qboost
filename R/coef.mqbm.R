#' Extract coefficients from a mqbm model
#'
#' Returns feature importance for each symbol-specific model within the mqbm object.
#'
#' @param object A fitted mqbm model object returned by \code{\link{mqbm}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A named list of tibbles, one per symbol, containing feature importances
#'   for each symbol-specific model. Returns \code{NULL} if feature importance
#'   is not available.
#'
#' @seealso \code{\link{mqbm}}, \code{\link{coef.qbm}}
#' @export
#' @method coef mqbm
coef.mqbm <- function(object, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  
  # Extract feature importance from each symbol-specific model
  coefs <- lapply(object$symbols, function(sym) {
    coef(object$models[[sym]], ...)
  })
  names(coefs) <- object$symbols
  
  coefs
}
