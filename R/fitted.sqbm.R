#' Fitted values method for mqbm
#'
#' Returns the fitted values from training for each symbol-specific model.
#'
#' @param object A mqbm object
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of fitted values in the original training data order
#' @export
fitted.mqbm <- function(object, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  
  # Initialize fitted values vector with same length as training data
  fitted_vals <- numeric(object$data_info$n)
  
  # Get fitted values from each symbol-specific model
  for (sym in object$symbols) {
    idx <- object$symbol_info[[sym]]$indices
    # Each qbm model stores its fitted values in training$fitted
    fitted_vals[idx] <- object$models[[sym]]$training$fitted
  }
  
  fitted_vals
}
