#' Extract residuals from a mqbm model
#'
#' Computes residuals as the difference between observed values and fitted
#' quantile predictions from the training data. For multi-symbol quantile
#' regression, residuals are computed separately for each symbol-specific
#' model and returned in the original training data order.
#'
#' @param object A fitted mqbm model object returned by \code{\link{mqbm}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of residuals with length equal to the number of
#'   training observations. Each element is computed as \code{y - fitted},
#'   where \code{fitted} is the predicted quantile from the appropriate
#'   symbol-specific model. Residuals are returned in the same order as the
#'   original training data.
#'
#' @seealso \code{\link{mqbm}}, \code{\link{fitted.mqbm}}, \code{\link{predict.mqbm}}
#'
#' @examples
#' \dontrun{
#' # Fit a mqbm model with multiple symbols
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
#' )
#' df$y <- df$x1 * 0.5 + rnorm(200)
#' fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50)
#'
#' # Extract residuals
#' resid <- residuals(fit)
#' summary(resid)
#'
#' # Residuals by symbol
#' tapply(resid, df$symbol, summary)
#' }
#'
#' @export
residuals.mqbm <- function(object, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  
  # Initialize residuals vector with same length as training data
  residuals_vals <- numeric(object$data_info$n)
  
  # Get residuals from each symbol-specific model
  for (sym in object$symbols) {
    idx <- object$symbol_info[[sym]]$indices
    # Each qbm model stores y and fitted values in training
    y_sym <- object$models[[sym]]$training$y
    fitted_sym <- object$models[[sym]]$training$fitted
    residuals_vals[idx] <- y_sym - fitted_sym
  }
  
  residuals_vals
}
