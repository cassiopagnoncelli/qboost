#' Summary for qevt model
#' @param object qevt
#' @param ... unused
#' @return invisible list with key stats
#' @export
summary.qevt <- function(object, ...) {
  cat("qevt: Extreme Quantile Model\n")
  cat(" Threshold tau0:", sprintf("%.4f", object$tau0), "\n")
  cat(" u:", format(object$u, digits = 6), "\n")
  cat(" Tail taus:", paste(format(c(object$tau1, object$tau2, object$tau3, object$tau_target), digits = 6), collapse = ", "), "\n")
  cat(" Sub-quantile grid:", paste(format(object$tau_grid_sub, digits = 4), collapse = ", "), "\n")
  gpd_status <- if (isTRUE(object$gpd$converged)) "converged" else "not converged"
  cat(" GPD: xi=", format(object$gpd$xi, digits = 6),
      " beta=", format(object$gpd$beta, digits = 6),
      " n=", object$gpd$n,
      " status=", gpd_status, "\n", sep = "")
  cat(" Exceedance model: LightGBM binary\n")
  cat(" Sub-quantile models: ", length(object$sub_models), " LightGBM quantile models\n", sep = "")
  # If held-out data stored, compute quick tail diagnostics
  if (!is.null(object$train_x) && !is.null(object$train_y)) {
    preds <- predict(object, object$train_x)
    q999 <- preds$monotone[, which(preds$taus == 0.999)]
    y <- object$train_y
    kendall_idx <- q999 > stats::quantile(q999, 0.999, na.rm = TRUE)
    if (any(kendall_idx)) {
      kendall <- suppressWarnings(stats::cor(y[kendall_idx], q999[kendall_idx], method = "kendall", use = "pairwise.complete.obs"))
      cat(" Kendall@0.999 (train): ", format(kendall, digits = 4), "\n", sep = "")
    }
    cover <- mean(y <= q999, na.rm = TRUE)
    cat(" Coverage@0.999 (train): ", format(cover, digits = 4), "\n", sep = "")
  }
  invisible(list(
    tau0 = object$tau0,
    u = object$u,
    taus_full = object$taus_full,
    gpd = object$gpd
  ))
}
