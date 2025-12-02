#' Extract residuals from a qbm model
#'
#' Computes residuals as the difference between observed values and fitted
#' quantile predictions from the training data. For quantile regression,
#' residuals provide insight into model fit and can be used to assess
#' calibration and identify outliers.
#'
#' @param object A fitted qbm model object returned by \code{\link{qbm}}.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of residuals with length equal to the number of
#'   training observations. Each element is computed as \code{y - fitted},
#'   where \code{fitted} is the predicted quantile from the training data.
#'
#' @seealso \code{\link{qbm}}, \code{\link{fitted.qbm}}, \code{\link{predict.qbm}}
#'
#' @examples
#' \dontrun{
#' # Fit a qbm model
#' df <- data.frame(x1 = rnorm(100), x2 = rnorm(100))
#' df$y <- df$x1 * 0.5 + rnorm(100)
#' fit <- qbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50)
#'
#' # Extract residuals
#' resid <- residuals(fit)
#' summary(resid)
#'
#' # Plot residuals
#' plot(fitted(fit), resid, main = "Residuals vs Fitted")
#' abline(h = 0, col = "red", lty = 2)
#' }
#'
#' @export
residuals.qbm <- function(object, ...) {
  if (!inherits(object, "qbm")) {
    stop("`object` must be a qbm model.", call. = FALSE)
  }
  
  object$training$y - object$training$fitted
}
