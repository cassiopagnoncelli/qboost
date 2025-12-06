#' Summarise a `qbm` model
#'
#' @param object A fitted `qbm` object.
#' @param detailed Logical; if TRUE, print extended diagnostics.
#' @param newdata Optional new data for out-of-sample summary.
#' @param y_new Optional observed outcomes aligned with `newdata`.
#' @param top_features Integer; number of features to show in importance tables.
#' @param ... Unused, included for compatibility.
#'
#' @return An object of class `qbm_summary`.
#' @export
#' @method summary qbm
summary.qbm <- function(object, detailed = TRUE, newdata = NULL, y_new = NULL, top_features = 15, ...) {
  if (!inherits(object, "qbm")) {
    stop("`object` must be a qbm model.", call. = FALSE)
  }

  stability <- compute_stability_score(
    qce = object$calibration$qce,
    pseudo_r2 = object$metrics$pseudo_r2,
    overfit_gap = object$metrics$overfit_gap
  )

  new_section <- NULL
  if (!is.null(newdata)) {
    nd <- if (!is.matrix(newdata)) data.matrix(newdata) else newdata
    preds_new <- stats::predict(object, nd)

    metrics_new <- NULL
    if (!is.null(y_new)) {
      y_new <- as.numeric(y_new)
      if (length(y_new) != nrow(nd)) {
        stop("`y_new` length must match number of rows in `newdata`.", call. = FALSE)
      }
      metrics_new <- compute_qbm_metrics(
        y = y_new,
        yhat = preds_new,
        tau = object$tau,
        cv_result = NULL,
        model = NULL
      )
    }

    new_section <- list(
      predictions = preds_new,
      metrics = if (!is.null(metrics_new)) metrics_new$metrics else NULL,
      calibration = if (!is.null(metrics_new)) metrics_new$calibration else NULL,
      residuals = if (!is.null(metrics_new)) metrics_new$residuals else NULL,
      tails = if (!is.null(metrics_new)) metrics_new$tails else NULL,
      y = y_new
    )
  }

  # default top_features: Inf for detailed, 15 for compact
  if (is.null(top_features) || !is.numeric(top_features) ||
        length(top_features) != 1 || top_features < 1) {
    top_features_final <- if (isTRUE(detailed)) Inf else 15
  } else {
    top_features_final <- top_features
  }

  out <- list(
    tau = object$tau,
    best_iter = object$best_iter,
    metrics = object$metrics,
    calibration = object$calibration,
    tails = object$tails,
    complexity = object$complexity,
    residuals = object$residuals,
    importance = importance.qbm(object),
    validation = object$validation,
    data_info = object$data_info,
    timings = object$timings,
    cv_settings = object$cv_settings,
    params_used = object$params_used,
    stability = stability,
    detailed = detailed,
    newdata = new_section,
    top_features = top_features_final
  )

  class(out) <- "qbm_summary"
  out
}

#' Print method for `qbm_summary`
#'
#' @param x A `qbm_summary` object.
#' @param ... Additional arguments (unused).
#'
#' @return The input object, invisibly.
#' @export
print.qbm_summary <- function(x, ...) {
  if (!inherits(x, "qbm_summary")) {
    stop("`x` must be a qbm_summary object.", call. = FALSE)
  }

  if (isTRUE(x$detailed)) {
    cat("Quantile Gradient Boosting Model\n")
    cat(" Data:             ", x$data_info$n, " rows, ", x$data_info$p, " cols\n", sep = "")
    cat(" Elapsed (s):      ", format(x$timings$elapsed, digits = 4), "\n", sep = "")
    cat(" Tau:              ", format(x$tau, digits = 3), "\n", sep = "")
    cat(" Trees:            ", x$best_iter, "\n", sep = "")
    cat(" Leaves total:     ", format(x$complexity$total_leaves, big.mark = ","), "\n", sep = "")
    cat(" Avg leaves/tree:  ", format(x$complexity$avg_leaves_per_tree, digits = 3), "\n", sep = "")
    cat(" Gain/leaf:        ", format(x$complexity$gain_per_leaf, digits = 4), "\n", sep = "")
    cat(" Importance entropy:", format(x$complexity$importance_entropy, digits = 4), "\n\n", sep = "")

    cat("Hyperparameters\n")
    hp <- x$params_used
    hp_fields <- c("learning_rate", "num_leaves", "max_depth", "min_data_in_leaf", "feature_fraction", "boosting")
    for (nm in hp_fields) {
      if (!is.null(hp[[nm]])) {
        cat(" ", nm, ": ", hp[[nm]], "\n", sep = "")
      }
    }
    cat("\n")

    cat("Training metrics")
    if (!is.null(x$data_info$n_train)) {
      cat(" (n=", x$data_info$n_train, ")", sep = "")
    }
    cat("\n")
    cat(" Pinball loss:     ", format(x$metrics$pinball_loss, digits = 4), "\n", sep = "")
    cat(" MAE:              ", format(x$metrics$mae, digits = 4), "\n", sep = "")
    cat(" Pseudo-R2:        ", format(x$metrics$pseudo_r2, digits = 4), "\n", sep = "")
    cat(" Coverage:         ", format(x$calibration$coverage, digits = 4), "\n", sep = "")
    cat(" QCE:              ", format(x$calibration$qce, digits = 4), "\n", sep = "")

    if (!is.null(x$validation)) {
      cat("\nValidation metrics")
      if (!is.null(x$data_info$n_val) && x$data_info$n_val > 0) {
        cat(" (n=", x$data_info$n_val, ")", sep = "")
      }
      cat("\n")
      cat(" Pinball loss:     ", format(x$validation$metrics$pinball_loss, digits = 4), "\n", sep = "")
      cat(" MAE:              ", format(x$validation$metrics$mae, digits = 4), "\n", sep = "")
      cat(" Pseudo-R2:        ", format(x$validation$metrics$pseudo_r2, digits = 4), "\n", sep = "")
      cat(" Coverage:         ", format(x$validation$calibration$coverage, digits = 4), "\n", sep = "")
      cat(" QCE:              ", format(x$validation$calibration$qce, digits = 4), "\n", sep = "")
    }
    cat("\n")

    cat("Cross-validation\n")
    cat(" Best iteration:   ", x$metrics$best_iter_cv, "\n", sep = "")
    cat(" CV pinball loss:  ", format(x$metrics$cv_pinball, digits = 4), "\n", sep = "")
    cat(" Overfit gap:      ", format(x$metrics$overfit_gap, digits = 4), "\n", sep = "")
    cat(" Early stopping:   ", x$cv_settings$early_stopping_rounds, " (nfolds=", x$cv_settings$nfolds,
      ", nrounds=", x$cv_settings$nrounds, ")\n\n",
      sep = ""
    )

    cat("Residuals\n")
    cat(" Median:           ", format(x$residuals$median, digits = 4), "\n", sep = "")
    cat(" IQR:              ", format(x$residuals$iqr, digits = 4), "\n", sep = "")
    cat(" Skewness:         ", format(x$residuals$skewness, digits = 4), "\n\n", sep = "")

    cat("Calibration\n")
    cat(" Coverage:         ", format(x$calibration$coverage, digits = 4), "\n", sep = "")
    cat(" Target tau:       ", format(x$calibration$tau, digits = 3), "\n", sep = "")
    cat(" QCE:              ", format(x$calibration$qce, digits = 4), "\n", sep = "")
    cat(" Slope/Intercept:  ",
      format(x$calibration$slope, digits = 4), " / ",
      format(x$calibration$intercept, digits = 4), "\n",
      sep = ""
    )
    curve_preview <- utils::head(x$calibration$curve, 5)
    if (!is.null(curve_preview) && nrow(curve_preview) > 0) {
      cat(" Nominal vs observed (first 5):\n")
      print(curve_preview, row.names = FALSE)
    }
    cat("\n")

    cat("Tail statistics\n")
    cat(" Tail spread:      ", format(x$tails$tail_spread, digits = 4), "\n\n", sep = "")

    cat("Stability score:   ", format(x$stability, digits = 4), "\n", sep = "")
    comp_qce <- 1 - x$calibration$qce
    comp_overfit <- exp(-x$metrics$overfit_gap)
    cat("  components: (1-QCE)=", format(comp_qce, digits = 4),
      " pseudoR2=", format(x$metrics$pseudo_r2, digits = 4),
      " exp(-gap)=", format(comp_overfit, digits = 4), "\n\n",
      sep = ""
    )

    if (!is.null(x$newdata)) {
      cat("New data\n")
      cat(" Observations:     ", length(x$newdata$predictions), "\n", sep = "")
      if (!is.null(x$newdata$metrics)) {
        cat(" Pinball loss:     ", format(x$newdata$metrics$pinball_loss, digits = 4), "\n", sep = "")
        cat(" MAE:              ", format(x$newdata$metrics$mae, digits = 4), "\n", sep = "")
        cat(" Pseudo-R2:        ", format(x$newdata$metrics$pseudo_r2, digits = 4), "\n", sep = "")
        cat(" Coverage:         ", format(x$newdata$calibration$coverage, digits = 4),
          " (target ", format(x$newdata$calibration$tau, digits = 3), ")\n",
          sep = ""
        )
        cat(" QCE:              ", format(x$newdata$calibration$qce, digits = 4), "\n", sep = "")
        cat(" Tail spread:      ", format(x$newdata$tails$tail_spread, digits = 4), "\n\n", sep = "")
      } else {
        cat(" Metrics require `y_new`; only predictions computed.\n\n")
      }
    }

    if (!is.null(x$importance) && nrow(x$importance) > 0) {
      cat("Top features (gain):\n")
      top <- utils::head(x$importance, if (is.finite(x$top_features)) x$top_features else nrow(x$importance))
      top$share_gain <- top$share_gain * 100
      tbl <- tibble::as_tibble(top[, c("feature", "gain", "share_gain", "cover", "freq"), drop = FALSE])
      print(tbl, n = Inf)
    } else {
      cat("No feature importance available.\n")
    }
  } else {
    cat("Quantile Gradient Boosting Model\n")
    cat(" Tau:              ", format(x$tau, digits = 3), "\n", sep = "")
    cat(" Trees:            ", x$best_iter, "\n", sep = "")
    cat(" Pinball loss:     ", format(x$metrics$pinball_loss, digits = 4), "\n", sep = "")
    cat(" MAE:              ", format(x$metrics$mae, digits = 4), "\n", sep = "")
    cat(" Pseudo-R2:        ", format(x$metrics$pseudo_r2, digits = 4), "\n", sep = "")
    cat(" Coverage:         ", format(x$calibration$coverage, digits = 4),
      " (target ", format(x$calibration$tau, digits = 3), ")\n",
      sep = ""
    )
    cat(" QCE:              ", format(x$calibration$qce, digits = 4), "\n", sep = "")
    cat(" CV pinball:       ", format(x$metrics$cv_pinball, digits = 4),
      " | gap: ", format(x$metrics$overfit_gap, digits = 4), "\n",
      sep = ""
    )
    if (!is.null(x$newdata)) {
      cat(" New data: ")
      cat("n=", length(x$newdata$predictions))
      if (!is.null(x$newdata$metrics)) {
        cat(", pinball=", format(x$newdata$metrics$pinball_loss, digits = 4),
          ", MAE=", format(x$newdata$metrics$mae, digits = 4),
          ", QCE=", format(x$newdata$calibration$qce, digits = 4),
          sep = ""
        )
      } else {
        cat(" (predictions only; provide y_new for metrics)")
      }
      cat("\n")
    }
    if (!is.null(x$importance) && nrow(x$importance) > 0) {
      top <- utils::head(x$importance, if (is.finite(x$top_features)) x$top_features else nrow(x$importance))
      top$share_gain <- top$share_gain * 100
      cat(" Top features:\n")
      tbl <- tibble::as_tibble(top[, c("feature", "gain", "share_gain"), drop = FALSE])
      print(tbl, n = Inf)
    }
    cat("\n(detailed = TRUE for full report)\n")
  }

  invisible(x)
}
