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
  invisible(list(
    tau0 = object$tau0,
    u = object$u,
    taus_full = object$taus_full,
    gpd = object$gpd
  ))
}
