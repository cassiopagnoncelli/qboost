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

#' Pinball loss components
#'
#' Returns positive and negative parts separately.
#'
#' @inheritParams pinball_loss
#' @return Data frame with positive and negative components and total.
#' @keywords internal
pinball_loss_components <- function(truth, estimate, tau) {
  truth <- as.numeric(truth)
  estimate <- as.numeric(estimate)
  pos <- pmax(truth - estimate, 0)
  neg <- pmax(estimate - truth, 0)
  data.frame(
    positive = tau * pos,
    negative = (1 - tau) * neg,
    total = tau * pos + (1 - tau) * neg
  )
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

#' Calibration slope and intercept from coverage curve
#'
#' Fits a simple linear regression of observed coverage on nominal probability.
#'
#' @param calib_df Data frame with columns `nominal` and `observed`.
#'
#' @return List with slope and intercept.
#' @keywords internal
calibration_fit <- function(calib_df) {
  if (is.null(calib_df) || nrow(calib_df) < 2) {
    return(list(slope = NA_real_, intercept = NA_real_))
  }
  fit <- stats::lm(observed ~ nominal, data = calib_df)
  list(
    slope = unname(stats::coef(fit)[["nominal"]]),
    intercept = unname(stats::coef(fit)[["(Intercept)"]])
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

#' Extract leaf count from a single tree
#' @keywords internal
extract_tree_leaves <- function(tree) {
  if (!is.list(tree)) {
    return(NA_real_)
  }
  if (!is.null(tree$num_leaves)) {
    return(tree$num_leaves)
  }
  if (!is.null(tree$leaf_value)) {
    return(length(tree$leaf_value))
  }
  NA_real_
}

#' Get default leaves parameter from model
#' @keywords internal
get_default_leaves <- function(model) {
  num_leaves_param <- tryCatch(model$params$num_leaves, error = function(e) NULL)
  if (!is.null(num_leaves_param)) {
    return(num_leaves_param)
  }

  max_leaves_param <- tryCatch(model$params$max_leaves, error = function(e) NULL)
  if (!is.null(max_leaves_param)) {
    return(max_leaves_param)
  }

  31
}

#' Get tree count from model
#' @keywords internal
get_tree_count <- function(model, best_iter = NULL) {
  if (!is.null(best_iter)) {
    return(best_iter)
  }

  bi <- tryCatch(model$best_iter, error = function(e) NULL)
  if (!is.null(bi) && !is.na(bi) && bi > 0) {
    return(bi)
  }

  ci <- tryCatch(model$current_iter(), error = function(e) NA_real_)
  if (!is.na(ci) && ci > 0) {
    return(ci)
  }

  NA_real_
}

#' Extract leaves statistics from LightGBM model
#'
#' @param model An `lgb.Booster` object.
#'
#' @return List with total leaves and average leaves per tree.
#' @keywords internal
leaf_stats <- function(model, best_iter = NULL) {
  info <- tryCatch(model$dump_model(), error = function(e) NULL)
  tree_info <- if (is.list(info) && !is.null(info$tree_info)) info$tree_info else list()

  leaves <- if (length(tree_info) > 0) {
    vapply(tree_info, extract_tree_leaves, numeric(1))
  } else {
    numeric(0)
  }

  leaves_clean <- leaves[!is.na(leaves)]
  total <- if (length(leaves_clean) > 0) sum(leaves_clean) else NA_real_
  avg <- if (length(leaves_clean) > 0) mean(leaves_clean) else NA_real_

  # Fallback: estimate using model params when dump is unavailable
  if (is.na(total) || is.na(avg)) {
    leaves_default <- get_default_leaves(model)
    trees_count <- get_tree_count(model, best_iter)

    if (!is.na(leaves_default) && !is.na(trees_count)) {
      total <- leaves_default * trees_count
      avg <- leaves_default
    }
  }

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
compute_qboost_metrics <- function(y, yhat, tau, cv_result = NULL, model = NULL, best_iter = NULL) {
  pinball_mean <- pinball_loss_mean(y, yhat, tau)
  pinball_comp <- pinball_loss_components(y, yhat, tau)
  mae_val <- mae(y, yhat)
  pseudo <- quantile_pseudo_r2(y, yhat, tau)
  cov_qce <- coverage_qce(y, yhat, tau)
  calib_curve <- calibration_curve(y, yhat)
  tails <- tail_spread_metric(y, yhat)
  residuals <- y - yhat
  res_sd <- stats::sd(residuals, na.rm = TRUE)
  skew <- if (is.na(res_sd) || res_sd == 0) {
    NA_real_
  } else {
    mean((residuals - mean(residuals, na.rm = TRUE))^3, na.rm = TRUE) / (res_sd^3)
  }
  residual_stats <- list(
    median = stats::median(residuals, na.rm = TRUE),
    iqr = stats::quantile(residuals, 0.75, na.rm = TRUE, names = FALSE) -
      stats::quantile(residuals, 0.25, na.rm = TRUE, names = FALSE),
    skewness = skew,
    tail_spread = tails
  )

  cv_pin <- if (!is.null(cv_result$best_score)) cv_result$best_score else NA_real_
  overfit_gap <- pinball_mean - cv_pin

  if (is.null(model)) {
    importance_df <- tidy_importance(NULL)
    leaves <- list(total_leaves = NA_real_, avg_leaves_per_tree = NA_real_)
  } else {
    importance_df <- tidy_importance(model)
    leaves <- leaf_stats(model, best_iter = best_iter)
  }
  gain_leaf <- gain_per_leaf_metric(importance_df, leaves$total_leaves)
  entropy <- importance_entropy(importance_df)

  calib_fit <- calibration_fit(calib_curve)

  list(
    metrics = list(
      pinball_loss = pinball_mean,
      pinball_components = colMeans(pinball_comp, na.rm = TRUE),
      mae = mae_val,
      pseudo_r2 = pseudo,
      best_iter_cv = if (!is.null(cv_result$best_iter)) cv_result$best_iter else NA_integer_,
      cv_pinball = cv_pin,
      overfit_gap = overfit_gap
    ),
    calibration = list(
      coverage = cov_qce$coverage,
      tau = tau,
      qce = cov_qce$qce,
      curve = calib_curve,
      slope = calib_fit$slope,
      intercept = calib_fit$intercept
    ),
    tails = list(
      tail_spread = tails
    ),
    residuals = residual_stats,
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
#' @return Stability score roughly bounded between 0 and 1.
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
  total_gain <- sum(imp$Gain, na.rm = TRUE)
  tibble::tibble(
    feature = imp$Feature,
    gain = imp$Gain,
    cover = if (!is.null(imp$Cover)) imp$Cover else NA_real_,
    freq = imp$Frequency,
    share_gain = if (!is.null(total_gain) && total_gain != 0) imp$Gain / total_gain else NA_real_
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
