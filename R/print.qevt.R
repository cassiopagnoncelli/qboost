#' Print method for qevt models
#'
#' Displays a summary of the fitted qevt model by calling \code{\link{summary.qevt}}.
#'
#' @param x A fitted qevt model object returned by \code{\link{qevt}}.
#' @param ... Additional arguments passed to \code{\link{summary.qevt}}.
#'
#' @return The input object \code{x}, invisibly.
#'
#' @seealso \code{\link{qevt}}, \code{\link{summary.qevt}}
#' @export
#' @method print qevt
print.qevt <- function(x, ...) {
  summary(x, ...)
  invisible(x)
}
