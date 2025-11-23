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
    nominals <- c(.95, .975, .99, .995, .997, .9985, .999, .9993, .9998)
    
  } else {
    nominals <- c(.05, .025, .01, .005, .003, .0015, .001, .0007, .0002)
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
