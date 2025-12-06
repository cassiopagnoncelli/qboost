#' Summarize an mqtail model
#'
#' Aggregates performance metrics per multiplexer gate.
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

  # Aggregate summaries per multiplexer value
  value_summaries <- lapply(object$multiplexer_values, function(val) {
    list(
      value = val,
      n_exceedances = object$evt_models[[val]]$n_exceedances,
      gpd_xi = object$evt_models[[val]]$xi,
      gpd_beta = object$evt_models[[val]]$beta
    )
  })
  names(value_summaries) <- object$multiplexer_values

  out <- list(
    tail = object$tail,
    tau_target = object$tau_target,
    threshold_tau = object$threshold_tau,
    taus = object$taus,
    multiplexer = if (!is.null(object$multiplexer)) object$multiplexer else "multiplexer",
    n_values = length(object$multiplexer_values),
    multiplexer_values = object$multiplexer_values,
    value_summaries = value_summaries,
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

  cat("Multiplexed Extreme Tail Quantile Model\n")
  cat(" Multiplexer:      ", x$multiplexer, "\n", sep = "")
  cat(" Values:           ", x$n_values, " (", paste(x$multiplexer_values, collapse = ", "), ")\n", sep = "")
  cat(" Tail:             ", x$tail, "\n", sep = "")
  cat(" Target tau:       ", format(x$tau_target, digits = 4), "\n", sep = "")
  cat(" Threshold tau:    ", format(x$threshold_tau, digits = 4), "\n", sep = "")
  cat(" Taus:             ", paste(format(x$taus, digits = 3), collapse = ", "), "\n", sep = "")
  cat(" Elapsed (s):      ", format(x$timings$elapsed, digits = 4), "\n\n", sep = "")

  cat("Per-Value GPD Parameters:\n")
  for (val in x$multiplexer_values) {
    summ <- x$value_summaries[[val]]
    cat(sprintf(
      "  %s: exc=%d, xi=%.3f, beta=%.3f\n",
      val, summ$n_exceedances, summ$gpd_xi, summ$gpd_beta
    ))
  }

  invisible(x)
}
