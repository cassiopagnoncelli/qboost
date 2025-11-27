#' Plot diagnostics for qevt
#' @param x qevt model
#' @param newdata Feature matrix/data.frame; defaults to training data stored in model
#' @param obs integer row index to plot (default 1)
#' @param ... unused
#' @export
plot.qevt <- function(x, newdata = NULL, obs = 1, ...) {
  data_use <- newdata
  if (is.null(data_use)) {
    if (!is.null(x$train_x)) {
      data_use <- x$train_x
    } else {
      stop("newdata is required")
    }
  }
  preds <- predict(x, data_use)
  if (obs < 1 || obs > nrow(preds$raw)) stop("obs out of range")
  q_raw <- preds$raw[obs, ]
  q_mono <- preds$monotone[obs, ]
  taus <- preds$taus
  tail_idx <- seq.int(length(taus) - 4, length(taus))
  p_exc_vec <- predict_exceedance(x$exceed_model, data_use)
  p_exc <- p_exc_vec[obs]
  severity_pred <- NA_real_
  if (!is.null(x$severity_model)) {
    pred_fun <- NULL
    if (exists("predict.lgb.Booster", envir = asNamespace("lightgbm"), inherits = FALSE)) {
      pred_fun <- get("predict.lgb.Booster", envir = asNamespace("lightgbm"))
    }
    if (is.null(pred_fun)) {
      pred_fun <- predict
    }
    severity_pred <- as.numeric(pred_fun(x$severity_model, data.matrix(data_use[obs, , drop = FALSE])))
  }

  op <- par(no.readonly = TRUE)
  on.exit(par(op))
  par(mfrow = c(2, 2))

  # Raw vs monotone quantiles
  plot(taus, q_raw,
    type = "b",
    col = "gray50",
    pch = 16,
    xlab = "tau",
    ylab = "quantile",
    main = sprintf("Quantiles (obs %d)", obs)
  )
  lines(taus, q_mono, type = "b", col = "blue", pch = 17)
  legend("topleft", legend = c("raw", "monotone"), col = c("gray50", "blue"), pch = c(16, 17), bty = "n")
  # Annotate coverage on this obs for the target tau
  target_tau <- x$tau_target
  q_target <- q_mono[which(taus == target_tau)]
  covered <- as.numeric(x$train_y[obs] <= q_target)
  mtext(sprintf("cover@%.4f=%d", target_tau, covered), side = 3, line = -2, adj = 1, cex = 0.8)

  # Tail focus
  plot(taus[tail_idx], q_mono[tail_idx],
    type = "b",
    col = "red",
    pch = 17,
    xlab = "tau (tail)",
    ylab = "quantile",
    main = "Tail quantiles"
  )
  points(taus[tail_idx], q_raw[tail_idx], col = "gray60", pch = 16)
  legend("topleft", legend = c("raw", "monotone"), col = c("gray60", "red"), pch = c(16, 17), bty = "n")

  # Exceedance probability gauge
  plot(0, 0,
    type = "n",
    xlim = c(0, 1), ylim = c(0, 1),
    axes = FALSE, xlab = "", ylab = "",
    main = "Exceedance prob (tau0)"
  )
  segments(0, 0.5, 1, 0.5, lwd = 6, col = "lightgray")
  segments(0, 0.5, p_exc, 0.5, lwd = 6, col = "darkgreen")
  axis(1, at = c(0, 0.5, 1), labels = c("0", "0.5", "1"))
  text(p_exc, 0.7, labels = sprintf("p=%.3f", p_exc))

  # EVT/GPD tail curve (fallback to severity if needed)
  tail_taus <- seq(x$tau0, x$tau_target, length.out = 100)
  tau_cond <- (tail_taus - x$tau0) / (1 - x$tau0)
  tau_cond <- pmin(pmax(tau_cond, 0), 1 - 1e-12)
  use_gpd <- isTRUE(x$gpd$converged) && is.finite(x$gpd$xi) && is.finite(x$gpd$beta) && x$gpd$beta > 0
  if (use_gpd) {
    q_tail <- x$u + gpd_quantile(tau_cond, x$gpd$xi, x$gpd$beta)
  } else if (is.finite(severity_pred)) {
    q_tail <- x$u + severity_pred
  } else {
    q_tail <- rep(x$u, length(tail_taus))
  }
  plot(tail_taus, q_tail,
    type = "l",
    col = if (use_gpd) "purple" else "darkorange",
    lwd = 2,
    xlab = "tau (tail)",
    ylab = "quantile",
    main = if (use_gpd) "GPD tail curve" else "Tail curve (fallback)"
  )
  abline(v = x$tau0, col = "gray", lty = 3)

  invisible(NULL)
}
