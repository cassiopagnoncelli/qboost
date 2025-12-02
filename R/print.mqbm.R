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

  multi_name <- if (!is.null(x$multi)) x$multi else "symbol"
  
  cat("Symbol-based Quantile Gradient Boosting Model\n")
  cat(" Data:             ", x$data_info$n, " rows, ", x$data_info$p, " cols\n", sep = "")
  cat(" Elapsed (s):      ", format(x$timings$elapsed, digits = 4), "\n", sep = "")
  cat(" Tau:              ", format(x$tau, digits = 3), "\n", sep = "")
  cat(" Multi:            ", multi_name, "\n", sep = "")
  cat(" Symbols:          ", x$data_info$n_symbols, " (", paste(x$symbols, collapse = ", "), ")\n", sep = "")
  
  cat("\nModels per symbol:\n")
  for (sym in x$symbols) {
    n_sym <- x$symbol_info[[sym]]$n
    trees <- x$models[[sym]]$best_iter
    cat("  ", sym, ": n=", n_sym, ", trees=", trees, "\n", sep = "")
  }
  
  invisible(x)
}
