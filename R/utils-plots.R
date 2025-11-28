#' Pinball loss plot
#'
#' @param df Data frame from `pinball_plot_data()`.
#' @return A `ggplot` object.
#' @keywords internal
plot_pinball_loss <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = predicted, y = residual)) +
    ggplot2::geom_point(alpha = 0.6, color = "#1f78b4") +
    suppressWarnings(ggplot2::geom_smooth(method = "loess", se = FALSE, color = "#b2df8a")) +
    ggplot2::labs(
      title = "Pinball residuals",
      x = "Predicted quantile",
      y = "Residual (y - yhat)"
    ) +
    ggplot2::theme_minimal()
}

#' QQ plot between predicted and empirical quantiles
#'
#' @param df Data frame from `qq_plot_data()`.
#' @return A `ggplot` object.
#' @keywords internal
plot_qq_quantiles <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = empirical, y = predicted)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, color = "gray60", linetype = "dashed") +
    ggplot2::geom_point(color = "#e31a1c") +
    ggplot2::labs(
      title = "QQ plot: predicted vs empirical quantiles",
      x = "Empirical quantile of y",
      y = "Predicted quantile"
    ) +
    ggplot2::theme_minimal()
}

#' Calibration curve plot
#'
#' @param df Data frame from `calibration_curve()`.
#' @return A `ggplot` object.
#' @keywords internal
plot_calibration_curve <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = nominal, y = observed)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
    ggplot2::geom_line(color = "#6a3d9a") +
    ggplot2::geom_point(color = "#6a3d9a") +
    ggplot2::labs(
      title = "Calibration curve",
      x = "Nominal probability",
      y = "Observed coverage"
    ) +
    ggplot2::theme_minimal()
}

#' Feature importance plot
#'
#' @param df Data frame returned by `importance.qboost()`.
#' @param top_n Maximum number of features to display.
#' @return A `ggplot` object.
#' @keywords internal
plot_feature_importance <- function(df, top_n = 25) {
  if (is.null(df) || nrow(df) == 0) {
    return(
      ggplot2::ggplot() +
        ggplot2::labs(title = "No feature importance available") +
        ggplot2::theme_void()
    )
  }

  df <- df[order(-df$gain), , drop = FALSE]
  df <- utils::head(df, top_n)
  df$feature <- stats::reorder(df$feature, df$gain)

  ggplot2::ggplot(df, ggplot2::aes(x = feature, y = gain)) +
    ggplot2::geom_col(fill = "#33a02c") +
    ggplot2::coord_flip() +
    ggplot2::labs(
      title = "Feature importance (Gain)",
      x = "",
      y = "Gain"
    ) +
    ggplot2::theme_minimal()
}
