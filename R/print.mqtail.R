#' Print method for mqtail
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

  multi_name <- if (!is.null(x$multi)) x$multi else "symbol"

  cat("Symbol-based Extreme Tail Quantile Model\n")
  cat(" Data:             ", x$data_info$n, " rows, ", x$data_info$p, " cols\n", sep = "")
  cat(" Elapsed (s):      ", format(x$timings$elapsed, digits = 4), "\n", sep = "")
  cat(" Tail:             ", x$tail, "\n", sep = "")
  cat(" Target tau:       ", format(x$tau_target, digits = 4), "\n", sep = "")
  cat(" Threshold tau:    ", format(x$threshold_tau, digits = 4), "\n", sep = "")
  cat(" Taus:             ", paste(format(x$taus, digits = 3), collapse = ", "), "\n", sep = "")
  cat(" Multi:            ", multi_name, "\n", sep = "")
  cat(" Symbols:          ", x$data_info$n_symbols, " (", paste(x$symbols, collapse = ", "), ")\n", sep = "")

  cat("\nModels per symbol:\n")
  for (sym in x$symbols) {
    n_sym <- x$symbol_info[[sym]]$n
    n_exc <- x$models[[sym]]$evt$n_exceedances
    cat("  ", sym, ": n=", n_sym, ", exceedances=", n_exc, "\n", sep = "")
  }

  invisible(x)
}
