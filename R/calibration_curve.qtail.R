#' Calibration curve for qtail
#'
#' @param object A qtail object
#' @param ... Additional arguments (not used)
#'
#' @return Data frame with nominal, observed, and count columns
#' @export
calibration_curve.qtail <- function(object, ...) {
  
  # Set nominals based on tail type
  if (object$tail == "upper") {
    nominals <- c(0.95, 0.97, 0.99, 0.997, 0.999)
  } else {
    nominals <- c(0.05, 0.03, 0.01, 0.003, 0.001)
  }
  
  observed <- numeric(length(nominals))
  
  for (i in seq_along(nominals)) {
    tau <- nominals[i]
    
    # Predict for this tau
    q_hat <- predict.qtail(object, object$x, type = "final", tau_override = tau)
    
    # Compute empirical frequency
    if (object$tail == "upper") {
      observed[i] <- mean(object$y <= q_hat)
    } else {
      observed[i] <- mean(object$y >= q_hat)
    }
  }
  
  return(data.frame(
    nominal = nominals,
    observed = observed,
    count = length(object$y)
  ))
}
