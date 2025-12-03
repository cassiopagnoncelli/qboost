#' Summarize an mqtail model
#'
#' Aggregates performance metrics across all symbol-specific models.
#'
#' @param object A fitted mqtail model object returned by \code{\link{mqtail}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return An object of class \code{mqtail_summary}.
#'
#' @seealso \code{\link{mqtail}}, \code{\link{summary.qtail}}
#'
#' @export
#' @method summary mqtail
summary.mqtail <- function(object, ...) {
  if (!inherits(object, "mqtail")) {
    stop("`object` must be an mqtail model.", call. = FALSE)
  }

  # Aggregate symbol-specific summaries
  symbol_summaries <- lapply(object$symbols, function(sym) {
    summ <- summary(object$models[[sym]])
    list(
      symbol = sym,
      n = object$symbol_info[[sym]]$n,
      n_exceedances = object$models[[sym]]$evt$n_exceedances,
      gpd_xi = object$models[[sym]]$evt$xi,
      gpd_beta = object$models[[sym]]$evt$beta
    )
  })
  names(symbol_summaries) <- object$symbols

  out <- list(
    tail = object$tail,
    tau_target = object$tau_target,
    threshold_tau = object$threshold_tau,
    taus = object$taus,
    multi = if (!is.null(object$multi)) object$multi else "symbol",
    n_symbols = length(object$symbols),
    symbols = object$symbols,
    data_info = object$data_info,
    symbol_summaries = symbol_summaries,
    timings = object$timings
  )

  class(out) <- "mqtail_summary"
  out
}

#' Print method for mqtail_summary
#'
#' @param x An mqtail_summary object
#' @param ... Additional arguments (unused)
#'
#' @return The input object, invisibly
#' @export
print.mqtail_summary <- function(x, ...) {
  if (!inherits(x, "mqtail_summary")) {
    stop("`x` must be an mqtail_summary object.", call. = FALSE)
  }

  cat("Multi Symbol Extreme Tail Quantile Model\n")
  cat(" Data:             ", x$data_info$n, " rows, ", x$data_info$p, " cols\n", sep = "")
  cat(" Multi:            ", x$multi, "\n", sep = "")
  cat(" Symbols:          ", x$n_symbols, " (", paste(x$symbols, collapse = ", "), ")\n", sep = "")
  cat(" Tail:             ", x$tail, "\n", sep = "")
  cat(" Target tau:       ", format(x$tau_target, digits = 4), "\n", sep = "")
  cat(" Threshold tau:    ", format(x$threshold_tau, digits = 4), "\n", sep = "")
  cat(" Taus:             ", paste(format(x$taus, digits = 3), collapse = ", "), "\n", sep = "")
  cat(" Elapsed (s):      ", format(x$timings$elapsed, digits = 4), "\n\n", sep = "")

  cat("Per-Symbol GPD Parameters:\n")
  for (sym in x$symbols) {
    summ <- x$symbol_summaries[[sym]]
    cat(sprintf("  %s: n=%d, exc=%d, xi=%.3f, beta=%.3f\n",
                sym, summ$n, summ$n_exceedances, summ$gpd_xi, summ$gpd_beta))
  }

  invisible(x)
}
