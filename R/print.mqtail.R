#' Print method for mqtail``
#'
#' @param x A mqtail object.
#' @param ... Additional arguments (unused).
#'
#' @return The input object, invisibly.
#' @export
print.mqtail <- function(x, ...) {
  if (!inherits(x, "mqtail")) {
    stop("`x` must be a mqtail model.", call. = FALSE)
  }

  multiplexer_name <- if (!is.null(x$multiplexer)) x$multiplexer else "multiplexer"

  cat("Multiplexed Extreme Tail Quantile Model\n")
  cat(" Data:             ", x$data_info$n, " rows, ", x$data_info$p, " cols\n", sep = "")
  cat(" Elapsed (s):      ", format(x$timings$elapsed, digits = 4), "\n", sep = "")
  cat(" Tail:             ", x$tail, "\n", sep = "")
  cat(" Target tau:       ", format(x$tau_target, digits = 4), "\n", sep = "")
  cat(" Threshold tau:    ", format(x$threshold_tau, digits = 4), "\n", sep = "")
  cat(" Taus:             ", paste(format(x$taus, digits = 3), collapse = ", "), "\n", sep = "")
  cat(" Multiplexer:      ", multiplexer_name, "\n", sep = "")
  cat(
    " Values:           ", length(x$multiplexer_values),
    " (", paste(x$multiplexer_values, collapse = ", "), ")\n",
    sep = ""
  )

  cat("\nModels per multiplexer value:\n")
  for (val in x$multiplexer_values) {
    n_val <- x$multiplexer_info[[val]]$n
    n_exc <- x$evt_models[[val]]$n_exceedances
    cat("  ", val, ": n=", n_val, ", exceedances=", n_exc, "\n", sep = "")
  }

  invisible(x)
}
