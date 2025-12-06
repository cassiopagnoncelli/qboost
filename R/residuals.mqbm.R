#' Extract residuals from a mqbm model
#'
#' Computes residuals as the difference between observed values and fitted
#' quantile predictions from the training data. For multiplexed quantile
#' regression, residuals are computed separately for each group-specific
#' model and returned in the original training data order.
#'
#' @param object A fitted mqbm model object returned by \code{\link{mqbm}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of residuals with length equal to the number of
#'   training observations. Each element is computed as \code{y - fitted},
#'   where \code{fitted} is the predicted quantile from the appropriate
#'   group-specific model. Residuals are returned in the same order as the
#'   original training data.
#'
#' @seealso \code{\link{mqbm}}, \code{\link{fitted.mqbm}}, \code{\link{predict.mqbm}}
#'
#' @examples
#' \dontrun{
#' # Fit a mqbm model with multiple groups
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   cluster = sample(c("A", "B", "C"), 200, replace = TRUE)
#' )
#' df$y <- df$x1 * 0.5 + rnorm(200)
#' fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 50)
#'
#' # Extract residuals
#' resid <- residuals(fit)
#' summary(resid)
#'
#' # Residuals by group
#' tapply(resid, df$cluster, summary)
#' }
#'
#' @export
residuals.mqbm <- function(object, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  
  # Initialize residuals vector with same length as training data
  residuals_vals <- numeric(object$data_info$n)
  
  # Get residuals from each group-specific model
  for (val in object$multiplexer_values) {
    idx <- object$multiplexer_info[[val]]$indices
    # Each qbm model stores y and fitted values in training
    y_val <- object$models[[val]]$training$y
    fitted_val <- object$models[[val]]$training$fitted
    residuals_vals[idx] <- y_val - fitted_val
  }
  
  residuals_vals
}
