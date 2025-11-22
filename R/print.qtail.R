#' Print method for qtail
#'
#' @param x A qtail object
#' @param ... Additional arguments passed to summary
#'
#' @return The object (invisibly)
#' @export
print.qtail <- function(x, ...) {
  s <- summary(x, ...)
  print(s)
  invisible(x)
}
