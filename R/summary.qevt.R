#' Summary for qevt model
#' @param object qevt
#' @param newdata Optional feature matrix for diagnostics (defaults to training data if available)
#' @param y Optional outcomes aligned with `newdata` for diagnostics
#' @param ... unused
#' @return invisible list with key stats
#' @export
summary.qevt <- function(object, newdata = NULL, y = NULL, ...) {
  cat("qevt: Extreme Quantile Model\n")
  cat(" Threshold tau0:", sprintf("%.4f", object$tau0), "\n")
  cat(" u:", format(object$u, digits = 6), "\n")
  cat(" Tail taus:", paste(format(c(object$tau1, object$tau2, object$tau3, object$tau_target), digits = 6), collapse = ", "), "\n")
  cat(" Sub-quantile grid:", paste(format(object$tau_grid_sub, digits = 4), collapse = ", "), "\n")
  gpd_status <- if (isTRUE(object$gpd$converged)) "converged" else "not converged"
  cat(" GPD: xi=", format(object$gpd$xi, digits = 6),
    " beta=", format(object$gpd$beta, digits = 6),
    " n=", object$gpd$n,
    " status=", gpd_status, "\n",
    sep = ""
  )
  cat(" Exceedance model: LightGBM binary\n")
  cat(" Sub-quantile models: ", length(object$sub_models), " LightGBM quantile models\n", sep = "")
  # Diagnostics: train by default, or supplied newdata/y
  data_use <- newdata
  y_use <- y
  label <- "train"
  if (is.null(data_use) && !is.null(object$train_x)) {
    data_use <- object$train_x
    if (is.null(y_use) && !is.null(object$train_y)) {
      y_use <- object$train_y
    }
  } else if (!is.null(data_use)) {
    label <- "newdata"
  }
  diag_block <- function(data_mat, y_vec, label_txt) {
    preds <- predict(object, data_mat)
    if (!is.null(y_vec)) {
      q999 <- preds$monotone[, which(preds$taus == 0.999)]
      kendall_idx <- q999 > stats::quantile(q999, 0.999, na.rm = TRUE)
      kendall <- NA_real_
      if (any(kendall_idx)) {
        kendall <- suppressWarnings(stats::cor(y_vec[kendall_idx], q999[kendall_idx], method = "kendall", use = "pairwise.complete.obs"))
      }
      cover <- mean(y_vec <= q999, na.rm = TRUE)
      cat(" Kendall@0.999 (", label_txt, "): ", format(kendall, digits = 4), "\n", sep = "")
      cat(" Coverage@0.999 (", label_txt, "): ", format(cover, digits = 4), "\n", sep = "")
    }
  }
  if (!is.null(data_use)) {
    diag_block(data_use, y_use, label)
  }
  invisible(list(
    tau0 = object$tau0,
    u = object$u,
    taus_full = object$taus_full,
    gpd = object$gpd
  ))
}
