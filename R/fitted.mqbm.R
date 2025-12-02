#' Fitted values method for mqbm
#'
#' Returns the fitted values from training for each symbol-specific model,
#' transformed through the empirical cumulative distribution function (ECDF)
#' built from each symbol's training y values.
#'
#' @param object A mqbm object
#' @param ... Additional arguments (not used)
#'
#' @return Numeric vector of ECDF-transformed fitted values (probabilities)
#'   in the original training data order
#' @export
fitted.mqbm <- function(object, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  
  # Initialize fitted values vector with same length as training data
  fitted_vals <- numeric(object$data_info$n)
  
  # Get fitted values from each symbol-specific model and transform through ECDF
  for (sym in object$symbols) {
    idx <- object$symbol_info[[sym]]$indices
    # Get raw fitted values from qbm model
    raw_fitted <- object$models[[sym]]$training$fitted
    # Transform through symbol-specific ECDF
    fitted_vals[idx] <- object$ecdf_funs[[sym]](raw_fitted)
  }
  
  fitted_vals
}
