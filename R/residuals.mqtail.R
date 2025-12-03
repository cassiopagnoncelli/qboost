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

  # Initialize residuals vector with same length as training data
  residuals_vals <- numeric(object$data_info$n)

  # Get residuals from each symbol-specific model
  for (sym in object$symbols) {
    idx <- object$symbol_info[[sym]]$indices
    # Get residuals from qtail model
    residuals_vals[idx] <- residuals(object$models[[sym]])
  }

  residuals_vals
}
