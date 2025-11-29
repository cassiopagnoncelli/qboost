#' Summarise a qtail model
#'
#' @param object A fitted qtail object
#' @param detailed Logical; if TRUE, print extended diagnostics
#' @param newdata Optional new data for out-of-sample summary
#' @param y_new Optional observed outcomes aligned with newdata
#' @param ... Unused, included for compatibility
#'
#' @return An object of class qtail_summary
#' @export
summary.qtail <- function(object, detailed = TRUE, newdata = NULL, y_new = NULL, ...) {
  if (!inherits(object, "qtail")) {
    stop("`object` must be a qtail model.", call. = FALSE)
  }

  # Compute training predictions
  fitted_vals <- fitted.qtail(object)
  resids <- residuals.qtail(object)

  # Training calibration
  cal_curve <- calibration_curve.qtail(object)

  # Compute training coverage at target
  if (object$tail == "upper") {
    coverage_target <- mean(object$y <= fitted_vals)
  } else {
    coverage_target <- mean(object$y >= fitted_vals)
  }

  # New data section
  new_section <- NULL
  if (!is.null(newdata)) {
    nd <- if (!is.matrix(newdata)) data.matrix(newdata) else newdata
    preds_new <- predict.qtail(object, nd, type = "final")

    metrics_new <- NULL
    if (!is.null(y_new)) {
      y_new <- as.numeric(y_new)
      if (length(y_new) != nrow(nd)) {
        stop("`y_new` length must match number of rows in `newdata`.", call. = FALSE)
      }

      if (object$tail == "upper") {
        cov_new <- mean(y_new <= preds_new)
      } else {
        cov_new <- mean(y_new >= preds_new)
      }

      metrics_new <- list(
        coverage = cov_new,
        mae = mean(abs(y_new - preds_new)),
        n = length(y_new)
      )
    }

    new_section <- list(
      predictions = preds_new,
      metrics = metrics_new,
      y = y_new
    )
  }

  out <- list(
    tail = object$tail,
    taus = object$taus,
    tau_target = object$tau_target,
    threshold_tau = object$threshold_tau,
    n = object$n,
    p = ncol(object$x),
    stack_coef = object$stack$coef,
    stack_lambda = object$stack$lambda,
    evt = object$evt,
    calibration = list(
      curve = cal_curve,
      coverage_target = coverage_target
    ),
    residuals = list(
      median = stats::median(resids),
      iqr = stats::IQR(resids),
      mad = stats::mad(resids),
      skewness = mean(((resids - mean(resids)) / stats::sd(resids))^3)
    ),
    newdata = new_section,
    detailed = detailed
  )

  class(out) <- "qtail_summary"
  out
}

#' Print method for qtail_summary
#'
#' @param x A qtail_summary object
#' @param ... Additional arguments (unused)
#'
#' @return The input object, invisibly
#' @export
print.qtail_summary <- function(x, ...) {
  if (!inherits(x, "qtail_summary")) {
    stop("`x` must be a qtail_summary object.", call. = FALSE)
  }

  if (isTRUE(x$detailed)) {
    cat("Extreme Quantile Tail Model (qtail)\n")
    cat(" Data:             ", x$n, " rows, ", x$p, " cols\n", sep = "")
    cat(" Tail:             ", x$tail, "\n", sep = "")
    cat(" Target quantile:  ", format(x$tau_target, digits = 4), "\n", sep = "")
    cat(" Threshold (EVT):  ", format(x$threshold_tau, digits = 4), "\n", sep = "")
    cat(" Quantile grid:    ", paste(format(x$taus, digits = 4), collapse = ", "), "\n\n", sep = "")

    cat("Stacking Architecture\n")
    cat(" Base models:      ", length(x$taus), " qrb quantile models\n", sep = "")
    cat(" Meta-learner:     Ridge regression (alpha=0)\n")
    cat(" Lambda (CV):      ", format(x$stack_lambda, digits = 6), "\n", sep = "")
    cat(" Coefficients:\n")
    for (i in seq_along(x$stack_coef)) {
      nm <- names(x$stack_coef)[i]
      val <- x$stack_coef[i]
      cat("   ", format(nm, width = 15), ": ", format(val, digits = 4, width = 8), "\n", sep = "")
    }
    cat("\n")

    cat("Extreme Value Theory (GPD)\n")
    cat(" Shape (xi):       ", format(x$evt$xi, digits = 4), "\n", sep = "")
    cat(" Scale (beta):     ", format(x$evt$beta, digits = 4), "\n", sep = "")
    cat(" Exceedances:      ", x$evt$n_exceedances, " (at threshold tau=",
      format(x$threshold_tau, digits = 4), ")\n",
      sep = ""
    )

    if (x$evt$n_exceedances < 20) {
      cat(" Warning:          Few exceedances may affect EVT reliability\n")
    }
    cat("\n")

    cat("Training Performance\n")
    cat(" Coverage @ target:", format(x$calibration$coverage_target, digits = 4),
      " (nominal ", format(x$tau_target, digits = 4), ")\n",
      sep = ""
    )
    cat(" Deviation:        ",
      format(abs(x$calibration$coverage_target - x$tau_target), digits = 4), "\n\n",
      sep = ""
    )

    cat("Residuals (vs stacked model)\n")
    cat(" Median:           ", format(x$residuals$median, digits = 4), "\n", sep = "")
    cat(" IQR:              ", format(x$residuals$iqr, digits = 4), "\n", sep = "")
    cat(" MAD:              ", format(x$residuals$mad, digits = 4), "\n", sep = "")
    cat(" Skewness:         ", format(x$residuals$skewness, digits = 4), "\n\n", sep = "")

    cat("Calibration Curve (nominal vs observed):\n")
    cal_tbl <- x$calibration$curve
    cal_tbl$deviation <- abs(cal_tbl$observed - cal_tbl$nominal)
    print(cal_tbl, row.names = FALSE)
    cat("\n")

    if (!is.null(x$newdata)) {
      cat("New Data Evaluation\n")
      cat(" Observations:     ", length(x$newdata$predictions), "\n", sep = "")
      if (!is.null(x$newdata$metrics)) {
        cat(" Coverage @ target:", format(x$newdata$metrics$coverage, digits = 4),
          " (nominal ", format(x$tau_target, digits = 4), ")\n",
          sep = ""
        )
        cat(" Deviation:        ",
          format(abs(x$newdata$metrics$coverage - x$tau_target), digits = 4), "\n",
          sep = ""
        )
        cat(" MAE:              ", format(x$newdata$metrics$mae, digits = 4), "\n\n", sep = "")
      } else {
        cat(" Metrics require `y_new`; only predictions computed.\n\n")
      }
    }

    cat("Model Interpretation:\n")
    if (x$tail == "upper") {
      cat(" * Predicts extreme high values (", format(x$tau_target, digits = 4),
        " quantile)\n",
        sep = ""
      )
      cat(" * ", x$evt$n_exceedances, " training points exceed threshold\n", sep = "")
      cat(" * GPD shape (xi=", format(x$evt$xi, digits = 3),
        ") indicates tail heaviness\n",
        sep = ""
      )
    } else {
      cat(" * Predicts extreme low values (", format(x$tau_target, digits = 4),
        " quantile)\n",
        sep = ""
      )
      cat(" * ", x$evt$n_exceedances, " training points below threshold\n", sep = "")
      cat(" * GPD shape (xi=", format(x$evt$xi, digits = 3),
        ") indicates tail heaviness\n",
        sep = ""
      )
    }
  } else {
    # Compact summary
    cat("Extreme Quantile Tail Model (qtail)\n")
    cat(" Tail:             ", x$tail, " (target tau=", format(x$tau_target, digits = 4), ")\n", sep = "")
    cat(" Data:             ", x$n, " rows, ", x$p, " cols\n", sep = "")
    cat(" Models:           ", length(x$taus), " base + stack + EVT\n", sep = "")
    cat(" Coverage:         ", format(x$calibration$coverage_target, digits = 4),
      " | deviation: ", format(abs(x$calibration$coverage_target - x$tau_target), digits = 4), "\n",
      sep = ""
    )
    cat(" EVT params:       xi=", format(x$evt$xi, digits = 3),
      ", beta=", format(x$evt$beta, digits = 3),
      " (", x$evt$n_exceedances, " exc.)\n",
      sep = ""
    )

    if (!is.null(x$newdata)) {
      cat(" New data:         n=", length(x$newdata$predictions), sep = "")
      if (!is.null(x$newdata$metrics)) {
        cat(", coverage=", format(x$newdata$metrics$coverage, digits = 4),
          ", MAE=", format(x$newdata$metrics$mae, digits = 4),
          sep = ""
        )
      }
      cat("\n")
    }

    cat("\n(detailed = TRUE for full report)\n")
  }

  invisible(x)
}
