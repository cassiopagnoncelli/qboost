#' Plot method for qtail
#'
#' @param x A qtail object
#' @param type Type of plot to produce. If "all" (default), produces all diagnostic plots.
#' @param plot Logical; if TRUE, plots are printed. If FALSE, returns list of plots.
#' @param ... Additional arguments passed to plotting functions
#'
#' @return If plot=FALSE, a list of ggplot objects. Otherwise NULL (invisibly).
#' @export
plot.qtail <- function(x,
                       type = c(
                         "all", "calibration", "stack_coeffs", "evt_tail",
                         "gpd_qq", "residuals", "coverage"
                       ),
                       plot = TRUE,
                       ...) {
  type <- match.arg(type)

  # If type is "all", create all plots
  if (type == "all") {
    plots <- list(
      calibration = .plot_qtail_calibration(x),
      coverage = .plot_qtail_coverage(x),
      pit = .plot_qtail_pit(x),
      evt_tail = .plot_qtail_evt_tail(x),
      stack_coeffs = .plot_qtail_stack_coeffs(x),
      residuals = .plot_qtail_residuals(x),
      gpd_qq = .plot_qtail_gpd_qq(x)
    )

    if (isTRUE(plot)) {
      for (p in plots) {
        if (!is.null(p)) print(p)
      }
    }

    return(invisible(plots))
  }

  # Single plot type requested - use original base R plotting
  type <- match.arg(type, choices = c("calibration", "stack_coeffs", "evt_tail", "gpd_qq", "residuals"))

  if (type == "calibration") {
    # Calibration plot: nominal vs observed
    cal <- calibration_curve.qtail(x)

    plot(cal$nominal, cal$observed,
      xlab = "Nominal Coverage",
      ylab = "Observed Coverage",
      main = "Calibration Curve",
      pch = 19,
      col = "blue",
      xlim = range(c(cal$nominal, cal$observed)),
      ylim = range(c(cal$nominal, cal$observed)),
      ...
    )

    # Add 45-degree reference line
    abline(0, 1, col = "red", lty = 2)

    # Add grid
    grid()
  } else if (type == "stack_coeffs") {
    # Barplot of stacking coefficients
    coefs <- coef.qtail(x)

    barplot(abs(coefs),
      names.arg = names(coefs),
      las = 2,
      main = "Stacking Coefficients (Absolute Values)",
      ylab = "|Coefficient|",
      col = "steelblue",
      ...
    )
  } else if (type == "evt_tail") {
    # EVT tail extrapolation plot
    # Generate a sequence of tail probabilities
    if (x$tail == "upper") {
      taus_plot <- seq(x$threshold_tau, max(x$taus), length.out = 50)
    } else {
      taus_plot <- seq(min(x$taus), x$threshold_tau, length.out = 50)
    }

    # Compute stack and final predictions for each tau
    q_stack_vals <- numeric(length(taus_plot))
    q_final_vals <- numeric(length(taus_plot))

    for (i in seq_along(taus_plot)) {
      q_stack_vals[i] <- mean(predict.qtail(x, x$x,
        type = "stack",
        tau_override = taus_plot[i]
      ))
      q_final_vals[i] <- mean(predict.qtail(x, x$x,
        type = "final",
        tau_override = taus_plot[i]
      ))
    }

    # Plot
    plot(taus_plot, q_stack_vals,
      type = "l",
      col = "blue",
      lwd = 2,
      xlab = "Quantile Level (tau)",
      ylab = "Predicted Quantile",
      main = "EVT Tail Extrapolation",
      ...
    )

    lines(taus_plot, q_final_vals, col = "red", lwd = 2, lty = 1)

    # Add threshold line
    abline(v = x$threshold_tau, col = "gray", lty = 3)

    legend("topleft",
      legend = c("Stacked", "EVT-adjusted", "Threshold"),
      col = c("blue", "red", "gray"),
      lty = c(1, 1, 3),
      lwd = c(2, 2, 1)
    )

    grid()
  } else if (type == "gpd_qq") {
    # GPD QQ plot
    # Get exceedances
    q_thresh_hat <- predict(x$models[[as.character(x$threshold_tau)]], x$x)
    r <- x$y - q_thresh_hat

    if (x$tail == "upper") {
      e <- pmax(r, 0)
    } else {
      e <- pmax(-r, 0)
    }

    e_exc <- e[e > 0]

    if (length(e_exc) < 5) {
      plot.new()
      text(0.5, 0.5, "Insufficient exceedances for QQ plot", cex = 1.5)
      return(invisible(NULL))
    }

    # Sort empirical exceedances
    e_sorted <- sort(e_exc)
    n <- length(e_sorted)

    # Theoretical quantiles from GPD(xi, beta)
    p <- (1:n) / (n + 1)
    xi <- x$evt$xi
    beta <- x$evt$beta

    # GPD quantile function: Q(p) = (beta/xi) * ((1-p)^(-xi) - 1)
    if (abs(xi) < 1e-6) {
      # xi ≈ 0, use exponential limit
      q_theoretical <- -beta * log(1 - p)
    } else {
      q_theoretical <- (beta / xi) * ((1 - p)^(-xi) - 1)
    }

    plot(q_theoretical, e_sorted,
      xlab = "Theoretical GPD Quantiles",
      ylab = "Empirical Exceedances",
      main = "GPD QQ-Plot",
      pch = 19,
      col = "darkgreen",
      ...
    )

    abline(0, 1, col = "red", lty = 2)
    grid()
  } else if (type == "residuals") {
    # Histogram of residuals
    resids <- residuals.qtail(x)

    hist(resids,
      breaks = 50,
      main = "Residuals (vs Stacked Model)",
      xlab = "Residual",
      ylab = "Frequency",
      col = "lightblue",
      border = "white",
      ...
    )

    # Add vertical line at zero
    abline(v = 0, col = "red", lwd = 2, lty = 2)
  }

  invisible(NULL)
}

# Helper functions for ggplot-based diagnostics

.plot_qtail_calibration <- function(x) {
  cal <- calibration_curve.qtail(x)

  ggplot2::ggplot(cal, ggplot2::aes(x = nominal, y = observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_line(color = "#6a3d9a", linewidth = 1) +
    ggplot2::geom_point(color = "#6a3d9a", size = 2) +
    ggplot2::labs(
      title = "Calibration Curve",
      x = "Nominal Coverage",
      y = "Observed Coverage"
    ) +
    ggplot2::theme_minimal()
}

.plot_qtail_coverage <- function(x) {
  # Coverage at each fitted tau
  coverage_df <- data.frame(
    tau = x$taus,
    coverage = sapply(x$taus, function(tau) {
      preds <- predict.qtail(x, x$x, tau = tau, type = "final")
      mean(x$y <= preds, na.rm = TRUE)
    })
  )
  coverage_df$error <- abs(coverage_df$coverage - coverage_df$tau)

  ggplot2::ggplot(coverage_df, ggplot2::aes(x = tau, y = coverage)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_point(color = "#e31a1c", size = 3) +
    ggplot2::geom_line(color = "#e31a1c") +
    ggplot2::labs(
      title = "Coverage at Fitted Quantiles",
      x = "Target Quantile (tau)",
      y = "Observed Coverage"
    ) +
    ggplot2::theme_minimal()
}

.plot_qtail_evt_tail <- function(x) {
  # Generate sequence of tail probabilities
  if (x$tail == "upper") {
    taus_plot <- seq(x$threshold_tau, max(x$taus), length.out = 50)
  } else {
    taus_plot <- seq(min(x$taus), x$threshold_tau, length.out = 50)
  }

  # Compute predictions
  df_plot <- data.frame(
    tau = rep(taus_plot, 2),
    quantile = c(
      sapply(taus_plot, function(tau) mean(predict.qtail(x, x$x, type = "stack", tau_override = tau))),
      sapply(taus_plot, function(tau) mean(predict.qtail(x, x$x, type = "final", tau_override = tau)))
    ),
    type = rep(c("Stacked", "EVT-adjusted"), each = length(taus_plot))
  )

  ggplot2::ggplot(df_plot, ggplot2::aes(x = tau, y = quantile, color = type)) +
    ggplot2::geom_line(linewidth = 1.2) +
    ggplot2::geom_vline(xintercept = x$threshold_tau, linetype = "dotted", color = "gray50") +
    ggplot2::scale_color_manual(values = c("Stacked" = "#1f78b4", "EVT-adjusted" = "#e31a1c")) +
    ggplot2::labs(
      title = "EVT Tail Extrapolation",
      x = "Quantile Level (tau)",
      y = "Predicted Quantile",
      color = "Method"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top")
}

.plot_qtail_stack_coeffs <- function(x) {
  coefs <- coef.qtail(x)
  coef_df <- data.frame(
    variable = factor(names(coefs), levels = names(coefs)),
    coefficient = as.numeric(coefs)
  )

  ggplot2::ggplot(coef_df, ggplot2::aes(x = variable, y = abs(coefficient))) +
    ggplot2::geom_col(fill = "#33a02c") +
    ggplot2::labs(
      title = "Stacking Coefficients (Absolute Values)",
      x = "",
      y = "|Coefficient|"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

.plot_qtail_residuals <- function(x) {
  resids <- residuals.qtail(x)

  ggplot2::ggplot(data.frame(residuals = resids), ggplot2::aes(x = residuals)) +
    ggplot2::geom_histogram(bins = 50, fill = "#b2df8a", color = "white") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", color = "#e31a1c", linewidth = 1) +
    ggplot2::labs(
      title = "Residuals (vs Stacked Model)",
      x = "Residual",
      y = "Frequency"
    ) +
    ggplot2::theme_minimal()
}

.plot_qtail_pit <- function(x) {
  # Compute PIT values
  pit_vals <- qtail_pit(x)
  pit_vals <- pit_vals[is.finite(pit_vals)]

  if (length(pit_vals) < 10) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text",
        x = 0.5, y = 0.5,
        label = "Insufficient PIT values",
        size = 6
      ) +
      ggplot2::theme_void())
  }

  # Create histogram
  ggplot2::ggplot(data.frame(pit = pit_vals), ggplot2::aes(x = pit)) +
    ggplot2::geom_histogram(
      bins = 30, fill = "#ff7f00", color = "white",
      boundary = 0, closed = "left"
    ) +
    ggplot2::geom_hline(
      yintercept = length(pit_vals) / 30,
      linetype = "dashed", color = "gray60"
    ) +
    ggplot2::labs(
      title = "Probability Integral Transform (PIT)",
      subtitle = "Should be uniform if well-calibrated",
      x = "PIT Value",
      y = "Frequency"
    ) +
    ggplot2::scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    ggplot2::theme_minimal()
}

.plot_qtail_gpd_qq <- function(x) {
  # Get exceedances
  q_thresh_hat <- predict(x$models[[as.character(x$threshold_tau)]], x$x)
  r <- x$y - q_thresh_hat

  e <- if (x$tail == "upper") pmax(r, 0) else pmax(-r, 0)
  e_exc <- e[e > 0 & is.finite(e)]

  if (length(e_exc) < 5) {
    return(ggplot2::ggplot() +
      ggplot2::annotate("text",
        x = 0.5, y = 0.5,
        label = "Insufficient exceedances for QQ plot",
        size = 6
      ) +
      ggplot2::theme_void())
  }

  # Sort empirical exceedances
  e_sorted <- sort(e_exc)
  n <- length(e_sorted)

  # Theoretical quantiles from GPD
  p <- (1:n) / (n + 1)
  xi <- x$evt$xi
  beta <- x$evt$beta

  if (abs(xi) < 1e-6) {
    q_theoretical <- -beta * log(1 - p)
  } else {
    q_theoretical <- (beta / xi) * ((1 - p)^(-xi) - 1)
  }

  qq_df <- data.frame(
    theoretical = q_theoretical,
    empirical = e_sorted
  )

  ggplot2::ggplot(qq_df, ggplot2::aes(x = theoretical, y = empirical)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_point(color = "#33a02c", size = 2, alpha = 0.7) +
    ggplot2::labs(
      title = "GPD QQ-Plot",
      x = "Theoretical GPD Quantiles",
      y = "Empirical Exceedances"
    ) +
    ggplot2::theme_minimal()
}
