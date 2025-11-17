#' Summarise a `qboost` model
#'
#' @param object A fitted `qboost` object.
#' @param ... Unused, included for compatibility.
#'
#' @return An object of class `qboost_summary`.
#' @export
#' @method summary qboost
summary.qboost <- function(object, ...) {
  if (!inherits(object, "qboost")) {
    stop("`object` must be a qboost model.", call. = FALSE)
  }

  stability <- compute_stability_score(
    qce = object$calibration$qce,
    pseudo_r2 = object$metrics$pseudo_r2,
    overfit_gap = object$metrics$overfit_gap
  )

  out <- list(
    tau = object$tau,
    best_iter = object$best_iter,
    metrics = object$metrics,
    calibration = object$calibration,
    tails = object$tails,
    complexity = object$complexity,
    importance = importance.qboost(object),
    params_used = object$params_used,
    stability = stability
  )

  class(out) <- "qboost_summary"
  out
}

#' Print method for `qboost_summary`
#'
#' @param x A `qboost_summary` object.
#' @param ... Additional arguments (unused).
#'
#' @return The input object, invisibly.
#' @export
print.qboost_summary <- function(x, ...) {
  if (!inherits(x, "qboost_summary")) {
    stop("`x` must be a qboost_summary object.", call. = FALSE)
  }

  cat("Quantile Gradient Boosting Model\n")
  cat(" Tau:              ", format(x$tau, digits = 3), "\n", sep = "")
  cat(" Trees:            ", x$best_iter, "\n", sep = "")
  cat(" Leaves total:     ", format(x$complexity$total_leaves, big.mark = ","), "\n", sep = "")
  cat(" Avg leaves/tree:  ", format(x$complexity$avg_leaves_per_tree, digits = 3), "\n", sep = "")
  cat(" Gain/leaf:        ", format(x$complexity$gain_per_leaf, digits = 4), "\n", sep = "")
  cat(" Importance entropy:", format(x$complexity$importance_entropy, digits = 4), "\n\n", sep = "")

  cat("Training metrics\n")
  cat(" Pinball loss:     ", format(x$metrics$pinball_loss, digits = 4), "\n", sep = "")
  cat(" MAE:              ", format(x$metrics$mae, digits = 4), "\n", sep = "")
  cat(" Pseudo-R2:        ", format(x$metrics$pseudo_r2, digits = 4), "\n\n", sep = "")

  cat("Cross-validation\n")
  cat(" Best iteration:   ", x$metrics$best_iter_cv, "\n", sep = "")
  cat(" CV pinball loss:  ", format(x$metrics$cv_pinball, digits = 4), "\n", sep = "")
  cat(" Overfit gap:      ", format(x$metrics$overfit_gap, digits = 4), "\n\n", sep = "")

  cat("Calibration\n")
  cat(" Coverage:         ", format(x$calibration$coverage, digits = 4), "\n", sep = "")
  cat(" Target tau:       ", format(x$calibration$tau, digits = 3), "\n", sep = "")
  cat(" QCE:              ", format(x$calibration$qce, digits = 4), "\n\n", sep = "")

  cat("Tail statistics\n")
  cat(" Tail spread:      ", format(x$tails$tail_spread, digits = 4), "\n\n", sep = "")

  cat("Stability score:   ", format(x$stability, digits = 4), "\n\n", sep = "")

  if (!is.null(x$importance) && nrow(x$importance) > 0) {
    cat("Top features (gain):\n")
    top <- utils::head(x$importance, 10)
    print(top[, c("feature", "gain"), drop = FALSE], row.names = FALSE)
  } else {
    cat("No feature importance available.\n")
  }

  invisible(x)
}

#' Print method for `qboost`
#'
#' @param x A `qboost` object.
#' @param ... Additional arguments (unused).
#'
#' @return The input object, invisibly.
#' @export
print.qboost <- function(x, ...) {
  summary(x)
  invisible(x)
}
