#' Predict method for qevt
#' @param object qevt model
#' @param newdata New data.frame or matrix of predictors. For models fit with a
#'   formula, a data.frame with the same predictor columns used at training
#'   time is expected.
#' @param ... unused
#' @return list(raw, monotone, taus)
#' @export
predict.qevt <- function(object, newdata, ...) {
  if (!inherits(object, "qevt")) {
    stop("`object` must be a qevt model.", call. = FALSE)
  }
  if (missing(newdata)) {
    stop("`newdata` is required for prediction.", call. = FALSE)
  }

  # Handle formula preprocessing
  if (!is.null(object$preprocess) && identical(object$preprocess$type, "formula")) {
    terms_obj <- object$preprocess$terms
    if (is.null(terms_obj)) {
      stop("Terms information missing; cannot build design matrix for prediction.", call. = FALSE)
    }
    terms_noy <- stats::delete.response(terms_obj)
    nd <- newdata
    if (!is.data.frame(nd)) {
      nd <- as.data.frame(nd)
    }
    mf <- stats::model.frame(
      terms_noy,
      nd,
      na.action = stats::na.pass,
      xlev = object$preprocess$xlevels
    )
    newdata <- stats::model.matrix(
      terms_noy,
      mf,
      contrasts.arg = object$preprocess$contrasts
    )
    # Remove intercept column if present (consistent with training)
    if ("(Intercept)" %in% colnames(newdata)) {
      newdata <- newdata[, colnames(newdata) != "(Intercept)", drop = FALSE]
    }
  } else if (!is.matrix(newdata)) {
    newdata <- data.matrix(newdata)
  }

  # Ensure plain numeric matrix
  if (is.matrix(newdata)) {
    feature_names <- colnames(newdata)
    newdata <- matrix(as.numeric(newdata), nrow = nrow(newdata), ncol = ncol(newdata))
    colnames(newdata) <- feature_names
  }

  total_steps <- 4
  step_idx <- 1
  t0 <- Sys.time()
  message(sprintf("[Predict %d/%d] Exceedance probabilities...", step_idx, total_steps))
  p_exc <- predict_exceedance(object$exceed_model, newdata)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  -> done in %.2fs (ETA %.2fs)", elapsed, eta))

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
  message(sprintf("  -> done in %.2fs (ETA %.2fs)", elapsed, eta))

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
  q_evt_intermediate <- lapply(object$taus_evt, evt_with_fallback)
  q_evt_target <- evt_with_fallback(object$tau_target)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  -> EVT quantiles done in %.2fs (ETA %.2fs)", elapsed, eta))

  step_idx <- step_idx + 1
  message(sprintf("[Predict %d/%d] PAVA monotonicity...", step_idx, total_steps))
  Q_raw <- cbind(
    sub_mat,
    q_evt_tau0,
    do.call(cbind, q_evt_intermediate),
    q_evt_target
  )
  colnames(Q_raw) <- as.character(object$taus_full)
  Q_mono <- t(apply(Q_raw, 1, function(v) apply_pava(object$taus_full, v)))
  colnames(Q_mono) <- as.character(object$taus_full)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("  -> done in %.2fs (complete)", elapsed))
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
