#' Extract residuals from a mqtail model
#'
#' Computes residuals as the difference between observed values and fitted
#' predictions from the training data.
#'
#' @param object A fitted mqtail model object returned by \code{\link{mqtail}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of residuals with length equal to the number of
#'   training observations.
#'
#' @seealso \code{\link{mqtail}}, \code{\link{fitted.mqtail}}, \code{\link{predict.mqtail}}
#'
#' @export
residuals.mqtail <- function(object, ...) {
  if (!inherits(object, "mqtail")) {
    stop("`object` must be a mqtail model.", call. = FALSE)
  }

  # Get fitted values and observed y from the threshold model
  thresh_mqbm <- object$mqbm_models[[as.character(object$threshold_tau)]]
  fitted_vals <- fitted(thresh_mqbm, type = "surface")
  y_vals <- thresh_mqbm$training$y

  # Compute residuals
  y_vals - fitted_vals
}
