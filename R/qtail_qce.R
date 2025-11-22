#' Quantile Calibration Error for qtail
#'
#' @param object A qtail object
#' @param taus Optional vector of quantile levels to evaluate
#' @param ... Additional arguments (not used)
#'
#' @return Data frame with tau, observed, and qce columns
#' @export
qtail_qce <- function(object, taus = NULL, ...) {
  if (!inherits(object, "qtail")) {
    stop("`object` must be a qtail model.", call. = FALSE)
  }
  
  if (is.null(taus)) {
    taus <- object$taus
  }
  
  result <- data.frame(
    tau = numeric(length(taus)),
    observed = numeric(length(taus)),
    qce = numeric(length(taus))
  )
  
  for (i in seq_along(taus)) {
    tau <- taus[i]
    
    # Predict for this tau
    q_hat <- predict.qtail(object, object$x, type = "final", tau_override = tau)
    
    # Compute empirical coverage
    if (object$tail == "upper") {
      obs <- mean(object$y >= q_hat)
    } else {
      obs <- mean(object$y <= q_hat)
    }
    
    # QCE component
    qce <- abs(obs - tau)
    
    result$tau[i] <- tau
    result$observed[i] <- obs
    result$qce[i] <- qce
  }
  
  return(result)
}
