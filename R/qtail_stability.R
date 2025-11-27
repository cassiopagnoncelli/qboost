#' Stability Metric for qtail
#'
#' @param object A qtail object
#' @param ... Additional arguments (not used)
#'
#' @return List with stability score and components
#' @export
qtail_stability <- function(object, ...) {
  if (!inherits(object, "qtail")) {
    stop("`object` must be a qtail model.", call. = FALSE)
  }

  # Compute QCE at target tau
  qce_data <- qtail_qce(object, taus = object$tau_target)
  qce_target <- qce_data$qce[1]

  # Compute calibration slope (may produce glm warnings)
  slope_data <- suppressWarnings(qtail_calibration_slope(object, tau = object$tau_target))
  coverage_slope <- slope_data$slope

  # Overfit gap (default to 0 if not available)
  # Check if base models have metrics available
  overfit_gap <- 0
  if (!is.null(object$models[[1]]$metrics)) {
    # Try to compute from first model as proxy
    model1 <- object$models[[1]]
    if (!is.null(model1$metrics$pinball_loss) && !is.null(model1$metrics$cv_pinball)) {
      overfit_gap <- abs(model1$metrics$pinball_loss - model1$metrics$cv_pinball)
    }
  }

  # Compute stability
  stability <- exp(-qce_target) * coverage_slope * (1 - overfit_gap)

  return(list(
    stability = stability,
    qce = qce_target,
    slope = coverage_slope,
    overfit_gap = overfit_gap
  ))
}
