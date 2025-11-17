#' Pinball loss for quantile regression
#'
#' @param truth Observed numeric vector.
#' @param estimate Predicted numeric vector.
#' @param tau Target quantile.
#'
#' @return Numeric vector of losses.
#' @keywords internal
pinball_loss <- function(truth, estimate, tau) {
  truth <- as.numeric(truth)
  estimate <- as.numeric(estimate)
  pos <- pmax(truth - estimate, 0)
  neg <- pmax(estimate - truth, 0)
  tau * pos + (1 - tau) * neg
}

#' Mean pinball loss
#'
#' @inheritParams pinball_loss
#' @return Scalar mean loss.
#' @keywords internal
pinball_loss_mean <- function(truth, estimate, tau) {
  mean(pinball_loss(truth, estimate, tau), na.rm = TRUE)
}

#' Mean absolute error
#'
#' @inheritParams pinball_loss
#' @return Scalar MAE.
#' @keywords internal
mae <- function(truth, estimate) {
  mean(abs(truth - estimate), na.rm = TRUE)
}

#' Pseudo-R2 for quantile regression
#'
#' Compares the model pinball loss against a constant-quantile baseline.
#'
#' @inheritParams pinball_loss
#' @return Scalar pseudo-R2.
#' @keywords internal
quantile_pseudo_r2 <- function(truth, estimate, tau) {
  baseline <- stats::quantile(truth, tau, na.rm = TRUE, names = FALSE)
  baseline_loss <- pinball_loss_mean(truth, rep(baseline, length(truth)), tau)
  model_loss <- pinball_loss_mean(truth, estimate, tau)
  if (isTRUE(all.equal(baseline_loss, 0))) {
    return(NA_real_)
  }
  1 - (model_loss / baseline_loss)
}

#' Extract a CV table from lightgbm::lgb.cv()
#'
#' @param cv_result Object returned by `lightgbm::lgb.cv()`.
#' @return Data frame with iteration and quantile metric when available.
#' @keywords internal
extract_cv_table <- function(cv_result) {
  if (is.null(cv_result) || is.null(cv_result$record_evals)) {
    return(data.frame())
  }

  valid <- cv_result$record_evals$valid
  if (is.null(valid) || is.null(valid$quantile) || is.null(valid$quantile$eval)) {
    return(data.frame())
  }

  data.frame(
    iteration = seq_along(valid$quantile$eval),
    quantile = unlist(valid$quantile$eval, use.names = FALSE)
  )
}

#' Compute evaluation metrics for qboost models
#'
#' @param y Observed numeric vector.
#' @param yhat Predicted numeric vector.
#' @param tau Target quantile.
#' @param cv_result Optional `lgb.cv` result.
#'
#' @return List with pinball loss, pseudo-R2, MAE, baseline, and CV table.
#' @keywords internal
compute_qboost_metrics <- function(y, yhat, tau, cv_result = NULL) {
  pinball_mean <- pinball_loss_mean(y, yhat, tau)
  list(
    pinball_loss = pinball_mean,
    mae = mae(y, yhat),
    pseudo_r2 = quantile_pseudo_r2(y, yhat, tau),
    baseline = stats::quantile(y, tau, na.rm = TRUE, names = FALSE),
    cv = extract_cv_table(cv_result)
  )
}

#' Data for pinball loss plot
#'
#' @inheritParams pinball_loss
#' @return Data frame with residuals and loss.
#' @keywords internal
pinball_plot_data <- function(truth, estimate, tau) {
  data.frame(
    residual = truth - estimate,
    loss = pinball_loss(truth, estimate, tau)
  )
}

#' QQ plot data between predicted quantile and empirical distribution
#'
#' @param truth Observed numeric vector.
#' @param estimate Predicted numeric vector.
#' @param probs Probability grid.
#'
#' @return Data frame with empirical and predicted quantiles.
#' @keywords internal
qq_plot_data <- function(truth, estimate, probs = seq(0.05, 0.95, by = 0.05)) {
  data.frame(
    prob = probs,
    empirical = stats::quantile(truth, probs, na.rm = TRUE, names = FALSE),
    predicted = stats::quantile(estimate, probs, na.rm = TRUE, names = FALSE)
  )
}

#' Calibration curve data for quantile predictions
#'
#' Uses quantiles of the predicted distribution as thresholds and compares
#' observed coverage against nominal probabilities.
#'
#' @param truth Observed numeric vector.
#' @param estimate Predicted numeric vector.
#' @param probs Probability grid.
#'
#' @return Data frame with nominal and observed coverage.
#' @keywords internal
calibration_curve <- function(truth, estimate, probs = seq(0.05, 0.95, by = 0.05)) {
  thresholds <- stats::quantile(estimate, probs, na.rm = TRUE, names = FALSE)
  observed <- vapply(
    thresholds,
    function(t) mean(truth <= t, na.rm = TRUE),
    numeric(1)
  )
  data.frame(
    nominal = probs,
    observed = observed
  )
}
