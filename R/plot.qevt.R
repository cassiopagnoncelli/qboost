#' Plot quantile curve for a single observation
#' @param x qevt model
#' @param newdata Feature matrix/data.frame with at least one row
#' @param monotone logical; if TRUE use PAVA-adjusted quantiles
#' @param obs integer row index to plot (default 1)
#' @param ... unused
#' @export
plot.qevt <- function(x, newdata, monotone = TRUE, obs = 1, ...) {
  preds <- predict(x, newdata)
  mat <- if (monotone) preds$monotone else preds$raw
  if (obs < 1 || obs > nrow(mat)) {
    stop("obs out of range")
  }
  qs <- mat[obs, ]
  taus <- preds$taus
  plot(taus, qs,
       type = "b",
       xlab = "tau",
       ylab = "quantile",
       main = sprintf("Quantile curve (obs %d, monotone=%s)", obs, monotone),
       pch = 16)
}
