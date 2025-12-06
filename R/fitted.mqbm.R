#' Fitted values method for mqbm
#'
#' Returns the fitted values from training for each group-specific model.
#' Can return either raw quantile predictions or ECDF-transformed probabilities.
#'
#' @param object A mqbm object
#' @param type Character string specifying the type of fitted values. Either \code{"surface"}
#'   for raw quantile predictions (default) or \code{"quantile"} for ECDF-transformed
#'   probabilities.
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of fitted values in the original training data order.
#'   If \code{type = "surface"}, returns raw quantile values. If \code{type = "quantile"},
#'   returns ECDF-transformed probabilities (0-1 range).
#' @export
fitted.mqbm <- function(object, type = c("surface", "quantile"), ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }

  # Match and validate type argument
  type <- match.arg(type)

  # Initialize fitted values vector with same length as training data
  fitted_vals <- numeric(object$data_info$n)

  # Get fitted values from each group-specific model
  for (val in object$multiplexer_values) {
    idx <- object$multiplexer_info[[val]]$indices
    # Get raw fitted values from qbm model
    raw_fitted <- object$models[[val]]$training$fitted

    # Apply transformation based on type
    if (type == "quantile") {
      # Transform through group-specific ECDF
      fitted_vals[idx] <- object$ecdf_funs[[val]](raw_fitted)
    } else {
      # Return raw surface fitted values
      fitted_vals[idx] <- raw_fitted
    }
  }

  fitted_vals
}
