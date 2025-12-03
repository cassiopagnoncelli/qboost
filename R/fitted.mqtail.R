#' Fitted values method for mqtail
#'
#' Returns the fitted values from training for each symbol-specific model.
#' Can return either raw quantile predictions or ECDF-transformed probabilities.
#'
#' @param object A mqtail object
#' @param type Character string specifying the type of fitted values. Either \code{"surface"}
#'   for raw quantile predictions (default) or \code{"quantile"} for ECDF-transformed
#'   probabilities.
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of fitted values in the original training data order.
#'
#' @export
fitted.mqtail <- function(object, type = c("surface", "quantile"), ...) {
  if (!inherits(object, "mqtail")) {
    stop("`object` must be a mqtail model.", call. = FALSE)
  }

  # Match and validate type argument
  type <- match.arg(type)

  # Initialize fitted values vector with same length as training data
  fitted_vals <- numeric(object$data_info$n)

  # Get fitted values from each symbol-specific model
  for (sym in object$symbols) {
    idx <- object$symbol_info[[sym]]$indices
    # Get raw fitted values from qtail model
    raw_fitted <- fitted(object$models[[sym]])

    # Apply transformation based on type
    if (type == "quantile") {
      # Transform through symbol-specific ECDF
      fitted_vals[idx] <- object$ecdf_funs[[sym]](raw_fitted)
    } else {
      # Return raw surface fitted values
      fitted_vals[idx] <- raw_fitted
    }
  }

  fitted_vals
}
