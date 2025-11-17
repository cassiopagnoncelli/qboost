#' Diagnostic plots for `qboost` models
#'
#' Produces a list of ggplot objects: pinball loss vs residuals, QQ plot,
#' calibration curve, and feature importance.
#'
#' @param x A fitted `qboost` object.
#' @param plot Logical; if `TRUE`, plots are printed.
#' @param features_n Maximum number of features to show in the importance plot.
#' @param ... Unused, included for compatibility.
#'
#' @return A list of `ggplot` objects (invisible if printed).
#' @export
#' @method plot qboost
plot.qboost <- function(x, plot = TRUE, features_n = 25, ...) {
  if (!inherits(x, "qboost")) {
    stop("`x` must be a qboost model.", call. = FALSE)
  }
  if (is.null(x$training) || is.null(x$training$y) || is.null(x$training$fitted)) {
    stop("Training predictions are required for plotting diagnostics.", call. = FALSE)
  }

  truth <- x$training$y
  fitted <- x$training$fitted

  pin_df <- pinball_plot_data(truth, fitted, x$tau)
  qq_df <- qq_plot_data(truth, fitted)
  calib_df <- calibration_curve(truth, fitted)
  imp_df <- importance.qboost(x)

  plots <- list(
    pinball = plot_pinball_loss(pin_df),
    qq = plot_qq_quantiles(qq_df),
    calibration = plot_calibration_curve(calib_df),
    importance = plot_feature_importance(imp_df, top_n = features_n)
  )

  if (isTRUE(plot)) {
    for (p in plots) print(p)
  }

  invisible(plots)
}
