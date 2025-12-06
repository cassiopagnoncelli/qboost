#' Print method for `mqbm`
#'
#' @param x A `mqbm` object.
#' @param ... Additional arguments (unused).
#'
#' @return The input object, invisibly.
#' @export
print.mqbm <- function(x, ...) {
  if (!inherits(x, "mqbm")) {
    stop("`x` must be a mqbm model.", call. = FALSE)
  }

  multiplexer_name <- if (!is.null(x$multiplexer)) x$multiplexer else "multiplexer"

  cat("Multiplexed Quantile Gradient Boosting Model\n")
  cat(" Data:             ", x$data_info$n, " rows, ", x$data_info$p, " cols\n", sep = "")
  cat(" Elapsed (s):      ", format(x$timings$elapsed, digits = 4), "\n", sep = "")
  cat(" Tau:              ", format(x$tau, digits = 3), "\n", sep = "")
  cat(" Multiplexer:      ", multiplexer_name, "\n", sep = "")
  cat(
    " Values:           ", x$data_info$n_multiplexer,
    " (", paste(x$multiplexer_values, collapse = ", "), ")\n",
    sep = ""
  )

  cat("\nModels per value:\n")
  for (val in x$multiplexer_values) {
    n_val <- x$multiplexer_info[[val]]$n
    trees <- x$models[[val]]$best_iter
    cat("  ", val, ": n=", n_val, ", trees=", trees, "\n", sep = "")
  }

  invisible(x)
}
