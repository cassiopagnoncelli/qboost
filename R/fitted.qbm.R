#' Extract fitted values from a qbm model
#'
#' Returns the fitted quantile values from the training data. These are the
#' in-sample predictions at the specified quantile level (tau) used to train
#' the model.
#'
#' @param object A fitted qbm model object returned by \code{\link{qbm}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of fitted values with length equal to the number of
#'   training observations. Each value represents the predicted quantile at the
#'   model's specified tau level for the corresponding training observation.
#'
#' @seealso \code{\link{qbm}}, \code{\link{residuals.qbm}}, \code{\link{predict.qbm}}
#'
#' @examples
#' \dontrun{
#' # Fit a qbm model
#' df <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
#' df$y <- df$x1 * 0.5 + rnorm(100)
#' fit <- qbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50)
#'
#' # Extract fitted values
#' fitted_vals <- fitted(fit)
#' summary(fitted_vals)
#'
#' # Compare with observed values
#' plot(df$y, fitted_vals, main = "Observed vs Fitted")
#' abline(0, 1, col = "red", lty = 2)
#' }
#'
#' @export
fitted.qbm <- function(object, ...) {
  if (!inherits(object, "qbm")) {
    stop("`object` must be a qbm model.", call. = FALSE)
  }
  
  object$training$fitted
}
