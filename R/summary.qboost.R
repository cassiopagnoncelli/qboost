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

  out <- list(
    tau = object$tau,
    best_iter = object$best_iter,
    metrics = object$metrics,
    importance = importance.qboost(object),
    params_used = object$params_used
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
  cat(" Tau:          ", format(x$tau, digits = 3), "\n", sep = "")
  cat(" Trees:        ", x$best_iter, "\n", sep = "")
  cat(" Pinball loss: ", format(x$metrics$pinball_loss, digits = 4), "\n", sep = "")
  cat(" MAE:          ", format(x$metrics$mae, digits = 4), "\n", sep = "")
  cat(" Pseudo-R2:    ", format(x$metrics$pseudo_r2, digits = 4), "\n\n", sep = "")

  if (!is.null(x$metrics$cv) && nrow(x$metrics$cv) > 0) {
    best_cv <- x$metrics$cv[which.min(x$metrics$cv$quantile), , drop = FALSE]
    cat("Cross-validation\n")
    cat(" Best iteration: ", best_cv$iteration, "\n", sep = "")
    cat(" CV quantile:    ", format(best_cv$quantile, digits = 4), "\n\n", sep = "")
  }

  if (!is.null(x$importance) && nrow(x$importance) > 0) {
    cat("Top features (gain):\n")
    top <- head(x$importance, 10)
    print(top[, c("Feature", "Gain"), drop = FALSE], row.names = FALSE)
  } else {
    cat("No feature importance available.\n")
  }

  invisible(x)
}
