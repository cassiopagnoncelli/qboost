#' Fit independent quantile models
#' @param x Feature matrix/data.frame
#' @param y Numeric target
#' @param taus Numeric vector of quantile levels
#' @param params List of LightGBM parameters (per model)
#' @param ... Extra args to qbm
#' @return Named list of models keyed by tau
fit_quantile_models <- function(x, y, taus, params = list(), ...) {
  models <- vector("list", length(taus))
  for (i in seq_along(taus)) {
    tau <- taus[i]
    models[[i]] <- qbm(x, y, tau = tau, params = params, ...)
    names(models)[i] <- as.character(tau)
  }
  models
}

#' Raw predictions for all taus
#' @param models List from fit_quantile_models
#' @param newdata Feature matrix/data.frame
#' @param taus Quantile levels (in same order used for fitting)
#' @return Matrix n x K of raw quantile predictions
predict_quantiles_raw <- function(models, newdata, taus) {
  preds <- lapply(taus, function(tau) predict(models[[as.character(tau)]], newdata))
  preds_mat <- do.call(cbind, preds)
  colnames(preds_mat) <- as.character(taus)
  preds_mat
}

#' Apply PAVA per row to enforce monotone quantiles
#' @param pred_matrix n x K matrix of raw predictions (cols ordered by taus)
#' @param taus Quantile levels (increasing)
#' @return Matrix with isotonic-adjusted predictions
apply_pava_monotonicity <- function(pred_matrix, taus) {
  if (ncol(pred_matrix) != length(taus)) {
    stop("pred_matrix columns must match length of taus")
  }
  adjust_row <- function(row_vals) {
    iso <- stats::isoreg(x = taus, y = row_vals)
    # Interpolate back to original taus to align length/order
    stats::approx(x = iso$x, y = iso$yf, xout = taus, ties = "ordered")$y
  }
  adjusted <- t(apply(pred_matrix, 1, adjust_row))
  colnames(adjusted) <- colnames(pred_matrix)
  adjusted
}

#' Convenience: predict and enforce monotone
#' @param models List from fit_quantile_models
#' @param newdata Feature matrix/data.frame
#' @param taus Quantile levels (in same order used for fitting)
#' @return Matrix with isotonic-adjusted predictions
predict_quantiles_monotone <- function(models, newdata, taus) {
  raw <- predict_quantiles_raw(models, newdata, taus)
  apply_pava_monotonicity(raw, taus)
}

#' Stack monotone quantiles with ridge
#' @param preds_mono n x K matrix of monotone predictions
#' @param y Numeric target for stacking fit
#' @param lambda Ridge penalty
#' @return List with fit and coefficients
stack_quantiles <- function(preds_mono, y, lambda = 0.01) {
  fit <- glmnet::glmnet(
    preds_mono,
    y,
    alpha = 0,
    lambda = lambda,
    family = "gaussian",
    intercept = FALSE
  )
  coef_obj <- c(0, as.vector(fit$beta[, 1]))
  names(coef_obj) <- c("(Intercept)", colnames(preds_mono))
  list(
    fit = fit,
    coef = coef_obj,
    lambda = lambda
  )
}
