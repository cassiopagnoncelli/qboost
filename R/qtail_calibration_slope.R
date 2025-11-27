#' Quantile Calibration Slope for qtail
#'
#' @param object A qtail object
#' @param tau Quantile level to evaluate (default: object$tau_target)
#' @param ... Additional arguments (not used)
#'
#' @return List with slope, intercept, and tau
#' @export
qtail_calibration_slope <- function(object, tau = object$tau_target, ...) {
  if (!inherits(object, "qtail")) {
    stop("`object` must be a qtail model.", call. = FALSE)
  }

  # Predict for this tau
  q_hat <- predict.qtail(object, object$x, type = "final", tau_override = tau)

  # Create binary indicator
  if (object$tail == "upper") {
    binary <- as.numeric(object$y >= q_hat)
  } else {
    binary <- as.numeric(object$y <= q_hat)
  }

  # Fit logistic regression with error handling
  fit_result <- tryCatch(
    {
      fit <- stats::glm(binary ~ q_hat, family = stats::binomial())
      coefs <- stats::coef(fit)
      list(
        slope = coefs[2],
        intercept = coefs[1],
        tau = tau
      )
    },
    error = function(e) {
      # If glm fails, return NA values
      list(
        slope = NA_real_,
        intercept = NA_real_,
        tau = tau
      )
    }
  )

  return(fit_result)
}
