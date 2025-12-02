#' Print method for qbm models
#'
#' Displays a summary of the fitted qbm model by calling \code{\link{summary.qbm}}.
#'
#' @param x A fitted qbm model object returned by \code{\link{qbm}}.
#' @param ... Additional arguments passed to \code{\link{summary.qbm}}.
#'
#' @return The input object \code{x}, invisibly.
#'
#' @seealso \code{\link{qbm}}, \code{\link{summary.qbm}}
#' @export
#' @method print qbm
print.qbm <- function(x, ...) {
  summary(x, ...)
  invisible(x)
}
