#' Plot method for qtail
#'
#' @param x A qtail object
#' @param type Type of plot to produce
#' @param ... Additional arguments passed to plotting functions
#'
#' @return NULL (invisibly)
#' @export
plot.qtail <- function(x,
                       type = c("calibration", "stack_coeffs", "evt_tail", 
                                "gpd_qq", "residuals"),
                       ...) {
  
  type <- match.arg(type)
  
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
         ...)
    
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
            ...)
    
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
      q_stack_vals[i] <- mean(predict.qtail(x, x$x, type = "stack", 
                                             tau_override = taus_plot[i]))
      q_final_vals[i] <- mean(predict.qtail(x, x$x, type = "final", 
                                             tau_override = taus_plot[i]))
    }
    
    # Plot
    plot(taus_plot, q_stack_vals,
         type = "l",
         col = "blue",
         lwd = 2,
         xlab = "Quantile Level (tau)",
         ylab = "Predicted Quantile",
         main = "EVT Tail Extrapolation",
         ...)
    
    lines(taus_plot, q_final_vals, col = "red", lwd = 2, lty = 1)
    
    # Add threshold line
    abline(v = x$threshold_tau, col = "gray", lty = 3)
    
    legend("topleft",
           legend = c("Stacked", "EVT-adjusted", "Threshold"),
           col = c("blue", "red", "gray"),
           lty = c(1, 1, 3),
           lwd = c(2, 2, 1))
    
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
         ...)
    
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
         ...)
    
    # Add vertical line at zero
    abline(v = 0, col = "red", lwd = 2, lty = 2)
    
  }
  
  invisible(NULL)
}
