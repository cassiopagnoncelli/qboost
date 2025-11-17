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
#' Compares model pinball loss to a constant-quantile baseline.
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

#' Coverage and quantile coverage error
#'
#' @inheritParams pinball_loss
#' @return List with `coverage` and `qce`.
#' @keywords internal
coverage_qce <- function(truth, estimate, tau) {
  coverage <- mean(truth <= estimate, na.rm = TRUE)
  list(
    coverage = coverage,
    qce = abs(coverage - tau)
  )
}

#' Calibration curve data for quantile predictions
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

#' Tail spread metric (robust tail dispersion)
#'
#' Uses interdecile range of absolute residuals.
#'
#' @inheritParams pinball_loss
#' @return Numeric tail spread.
#' @keywords internal
tail_spread_metric <- function(truth, estimate) {
  res <- abs(truth - estimate)
  q90 <- stats::quantile(res, 0.90, na.rm = TRUE, names = FALSE)
  q10 <- stats::quantile(res, 0.10, na.rm = TRUE, names = FALSE)
  q90 - q10
}

#' Extract leaves statistics from LightGBM model
#'
#' @param model An `lgb.Booster` object.
#'
#' @return List with total leaves and average leaves per tree.
#' @keywords internal
leaf_stats <- function(model) {
  dump <- tryCatch(lightgbm::lgb.dump(model, num_iteration = NULL), error = function(e) NULL)
  tree_info <- model$dump_model()$tree_info %||% list()

  if (!is.null(dump) && length(dump) > 0) {
    # fallback to dump_model structure
    leaves <- vapply(tree_info, function(t) t$num_leaves %||% length(t$leaf_value %||% list()), numeric(1))
  } else {
    leaves <- vapply(tree_info, function(t) t$num_leaves %||% length(t$leaf_value %||% list()), numeric(1))
  }

  total <- sum(leaves)
  avg <- if (length(leaves) > 0) mean(leaves) else NA_real_

  list(
    total_leaves = total,
    avg_leaves_per_tree = avg
  )
}

#' Gain per leaf metric
#'
#' @param importance_df Importance tibble.
#' @param total_leaves Total number of leaves.
#'
#' @return Gain per leaf.
#' @keywords internal
gain_per_leaf_metric <- function(importance_df, total_leaves) {
  if (is.null(importance_df) || nrow(importance_df) == 0 || is.na(total_leaves) || total_leaves == 0) {
    return(NA_real_)
  }
  sum(importance_df$gain, na.rm = TRUE) / total_leaves
}

#' Importance entropy (Shannon entropy of normalized gains)
#'
#' @param importance_df Importance tibble.
#'
#' @return Entropy value.
#' @keywords internal
importance_entropy <- function(importance_df) {
  if (is.null(importance_df) || nrow(importance_df) == 0) {
    return(NA_real_)
  }
  p <- importance_df$gain / sum(importance_df$gain, na.rm = TRUE)
  p <- p[p > 0]
  -sum(p * log(p))
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
#' @param model An `lgb.Booster` model.
#'
#' @return List with metrics, calibration, tails, and complexity.
#' @keywords internal
compute_qboost_metrics <- function(y, yhat, tau, cv_result = NULL, model = NULL) {
  pinball_mean <- pinball_loss_mean(y, yhat, tau)
  mae_val <- mae(y, yhat)
  pseudo <- quantile_pseudo_r2(y, yhat, tau)
  cov_qce <- coverage_qce(y, yhat, tau)
  calib_curve <- calibration_curve(y, yhat)
  tails <- tail_spread_metric(y, yhat)

  cv_pin <- cv_result$best_score %||% NA_real_
  overfit_gap <- pinball_mean - cv_pin

  importance_df <- tidy_importance(model)
  leaves <- leaf_stats(model)
  gain_leaf <- gain_per_leaf_metric(importance_df, leaves$total_leaves)
  entropy <- importance_entropy(importance_df)

  list(
    metrics = list(
      pinball_loss = pinball_mean,
      mae = mae_val,
      pseudo_r2 = pseudo,
      best_iter_cv = cv_result$best_iter %||% NA_integer_,
      cv_pinball = cv_pin,
      overfit_gap = overfit_gap
    ),
    calibration = list(
      coverage = cov_qce$coverage,
      tau = tau,
      qce = cov_qce$qce,
      curve = calib_curve
    ),
    tails = list(
      tail_spread = tails
    ),
    complexity = list(
      total_leaves = leaves$total_leaves,
      avg_leaves_per_tree = leaves$avg_leaves_per_tree,
      gain_per_leaf = gain_leaf,
      importance_entropy = entropy
    )
  )
}

#' Stability score computation
#'
#' @param qce Quantile coverage error.
#' @param pseudo_r2 Pseudo-R2 metric.
#' @param overfit_gap Overfitting gap (train - CV pinball).
#'
#' @return Stability score in [0, 1]-ish space.
#' @keywords internal
compute_stability_score <- function(qce, pseudo_r2, overfit_gap) {
  0.50 * (1 - qce) + 0.30 * pseudo_r2 + 0.20 * exp(-overfit_gap)
}

#' Tidy LightGBM feature importance
#'
#' @param model An `lgb.Booster` object.
#' @param ... Additional arguments to `lightgbm::lgb.importance()`.
#'
#' @return Tibble with feature, gain, cover, freq.
#' @keywords internal
tidy_importance <- function(model, ...) {
  if (is.null(model)) {
    return(tibble::tibble(feature = character(), gain = double(), cover = double(), freq = double()))
  }
  imp <- tryCatch(lightgbm::lgb.importance(model, ...), error = function(e) NULL)
  if (is.null(imp) || nrow(imp) == 0) {
    return(tibble::tibble(feature = character(), gain = double(), cover = double(), freq = double()))
  }
  tibble::tibble(
    feature = imp$Feature,
    gain = imp$Gain,
    cover = imp$Cover %||% NA_real_,
    freq = imp$Frequency
  ) |> dplyr::arrange(dplyr::desc(gain))
}

#' Data for pinball loss plot
#'
#' @inheritParams pinball_loss
#' @return Data frame with residuals and loss.
#' @keywords internal
pinball_plot_data <- function(truth, estimate, tau) {
  data.frame(
    residual = truth - estimate,
    loss = pinball_loss(truth, estimate, tau),
    predicted = estimate
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
