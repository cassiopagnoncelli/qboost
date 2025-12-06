#' Predict from mqtail model
#'
#' @param object A fitted mqtail model
#' @param newdata Data for prediction
#' @param type Either "surface" (default) or "quantile"
#' @param ... Passed to predict.mqbm
#'
#' @export
#' @method predict mqtail
predict.mqtail <- function(object, newdata, type = c("surface", "quantile"), ...) {
  type <- match.arg(type)
  
  # Get predictions from each tau's mqbm model
  K <- length(object$taus)
  n_rows <- if (is.data.frame(newdata)) nrow(newdata) else nrow(as.matrix(newdata))
  Z_raw <- matrix(0, nrow = n_rows, ncol = K)
  
  for (j in seq_along(object$taus)) {
    tau <- object$taus[j]
    mqbm_model <- object$mqbm_models[[as.character(tau)]]
    # Get predictions - mqbm.predict handles the multi column
    Z_raw[, j] <- predict(mqbm_model, newdata, type = type, ...)
  }
  
  # Apply PAVA monotonicity
  Z <- apply_pava_monotonicity(Z_raw, object$taus)
  
  # Return stacked predictions (mean across taus)
  rowMeans(Z)
}
