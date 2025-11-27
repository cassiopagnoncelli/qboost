#' PIT Plot for qtail
#'
#' @param object A qtail object
#' @param breaks Number of histogram breaks
#' @param ... Additional arguments passed to hist
#'
#' @return NULL (invisibly)
#' @export
qtail_pit_plot <- function(object, breaks = 40, ...) {
  if (!inherits(object, "qtail")) {
    stop("`object` must be a qtail model.", call. = FALSE)
  }

  # Compute PIT values
  pit <- qtail_pit(object)

  # Remove NAs for plotting
  pit <- pit[!is.na(pit)]

  if (length(pit) < 2) {
    warning("Insufficient valid PIT values for plotting")
    return(invisible(NULL))
  }

  # Create histogram
  hist(pit,
    breaks = breaks,
    main = "PIT Histogram",
    xlab = "PIT Value",
    ylab = "Density",
    freq = FALSE,
    col = "lightblue",
    border = "white",
    ...
  )

  # Add uniform reference line
  abline(h = 1, col = "red", lwd = 2, lty = 2)

  invisible(NULL)
}
