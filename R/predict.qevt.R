#' Predict method for qevt
#' @param object qevt model
#' @param newdata Feature matrix/data.frame
#' @param ... unused
#' @return list(raw, monotone, taus)
#' @export
predict.qevt <- function(object, newdata, ...) {
  total_steps <- 4
  step_idx <- 1
  t0 <- Sys.time()
  message(sprintf("[Predict %d/%d] Exceedance probabilities...", step_idx, total_steps))
  p_exc <- predict_exceedance(object$exceed_model, newdata)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  → done in %.2fs (ETA %.2fs)", elapsed, eta))

  step_idx <- step_idx + 1
  message(sprintf("[Predict %d/%d] Sub-quantile predictions...", step_idx, total_steps))
  sub_mat <- predict_subquantiles(object$sub_models, newdata, object$tau_grid_sub)
  n <- nrow(sub_mat)
  use_gpd <- isTRUE(object$gpd$converged) && is.finite(object$gpd$xi) &&
    is.finite(object$gpd$beta) && object$gpd$beta > 0
  severity_pred <- rep(0, n)
  if (!is.null(object$severity_model)) {
    booster <- object$severity_model
    severity_pred <- .lgb_predict(booster, data.matrix(newdata))
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  → done in %.2fs (ETA %.2fs)", elapsed, eta))

  step_idx <- step_idx + 1
  message(sprintf("[Predict %d/%d] EVT quantiles...", step_idx, total_steps))
  evt_with_fallback <- function(tau) {
    if (use_gpd) {
      q_evt <- predict_evt_quantile(newdata, p_exc, object$gpd$xi, object$gpd$beta, object$u, tau, object$tau0)
    } else {
      q_evt <- rep(NA_real_, n)
    }
    if (any(!is.finite(q_evt))) {
      q_evt <- object$u + severity_pred
      q_evt <- q_evt * p_exc + object$u * (1 - p_exc)
    }
    q_evt
  }
  q_evt_tau0 <- rep(object$u, n)
  q_evt_tau1 <- evt_with_fallback(object$tau1)
  q_evt_tau2 <- evt_with_fallback(object$tau2)
  q_evt_tau3 <- evt_with_fallback(object$tau3)
  q_evt_target <- evt_with_fallback(object$tau_target)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  → EVT quantiles done in %.2fs (ETA %.2fs)", elapsed, eta))

  step_idx <- step_idx + 1
  message(sprintf("[Predict %d/%d] PAVA monotonicity...", step_idx, total_steps))
  Q_raw <- cbind(
    sub_mat,
    q_evt_tau0,
    q_evt_tau1,
    q_evt_tau2,
    q_evt_tau3,
    q_evt_target
  )
  colnames(Q_raw) <- as.character(object$taus_full)
  Q_mono <- t(apply(Q_raw, 1, function(v) apply_pava(object$taus_full, v)))
  colnames(Q_mono) <- as.character(object$taus_full)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("  → done in %.2fs (complete)", elapsed))
  list(
    raw = Q_raw,
    monotone = Q_mono,
    taus = object$taus_full
  )
}

# Convenience wrapper
predict_all_quantiles <- function(X, model) {
  predict(model, X)
}
