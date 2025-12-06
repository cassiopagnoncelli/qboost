#' Fitted values from mqtail model
#'
#' @param object A fitted mqtail model
#' @param type Either "surface" (default) or "quantile"
#' @param ... Additional arguments (not used)
#'
#' @export
fitted.mqtail <- function(object, type = c("surface", "quantile"), ...) {
  type <- match.arg(type)
  
  # Get fitted values from each tau's mqbm model
  K <- length(object$taus)
  first_mqbm <- object$mqbm_models[[1]]
  n <- first_mqbm$data_info$n
  Z_raw <- matrix(0, nrow = n, ncol = K)
  
  for (j in seq_along(object$taus)) {
    tau <- object$taus[j]
    mqbm_model <- object$mqbm_models[[as.character(tau)]]
    Z_raw[, j] <- fitted(mqbm_model, type = type)
  }
  
  # Apply PAVA monotonicity
  Z <- apply_pava_monotonicity(Z_raw, object$taus)
  
  # Return stacked fitted values (mean across taus)
  rowMeans(Z)
}
