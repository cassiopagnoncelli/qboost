#' Fit extreme quantile model with EVT and PAVA monotonicity
#' @param X Feature matrix/data.frame
#' @param y Numeric response
#' @param tau_target Final extreme tau (default 0.9999)
#' @param tau0 Threshold quantile for EVT (default 0.997)
#' @param tau1 Intermediate EVT quantile (default 0.9985)
#' @param tau2 Intermediate EVT quantile (default 0.999)
#' @param tau3 Intermediate EVT quantile (default 0.9993)
#' @return Object of class 'qevt'
#' @export
qevt <- function(
    X,
    y,
    tau_target = 0.9999,
    tau0 = 0.997,
    tau1 = 0.9985,
    tau2 = 0.999,
    tau3 = 0.9993) {
  total_steps <- 4
  step_idx <- 1
  t0 <- Sys.time()
  message(sprintf("[Step %d/%d] Fitting exceedance classifier (tau0=%.4f)...", step_idx, total_steps, tau0))
  tau_grid_sub <- c(0.95, 0.975, 0.99, 0.995)
  exceed_fit <- fit_exceedance_model(X, y, tau0 = tau0)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  → done in %.2fs (ETA %.2fs)", elapsed, eta))
  
  step_idx <- step_idx + 1
  message(sprintf("[Step %d/%d] Fitting GPD (MLE) and severity fallback...", step_idx, total_steps))
  u <- exceed_fit$u
  tail_idx <- which(y > u)
  y_tail <- y[tail_idx]
  X_tail <- X[tail_idx, , drop = FALSE]
  z <- y_tail - u
  gpd_fit <- fit_gpd(y_tail, u)
  severity_model <- NULL
  if (length(z) > 0) {
    severity_model <- fit_severity_lgbm(X_tail, z)
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  → done in %.2fs (ETA %.2fs)", elapsed, eta))
  
  step_idx <- step_idx + 1
  message(sprintf("[Step %d/%d] Training sub-quantile LightGBM models...", step_idx, total_steps))
  sub_models <- fit_subquantile_models(X, y, tau_grid_sub = tau_grid_sub)
  taus_full <- c(tau_grid_sub, tau0, tau1, tau2, tau3, tau_target)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  → done in %.2fs (ETA %.2fs)", elapsed, eta))
  
  step_idx <- step_idx + 1
  message(sprintf("[Step %d/%d] Assembling model object...", step_idx, total_steps))
  model <- list(
    exceed_model = exceed_fit,
    gpd = gpd_fit,
    severity_model = severity_model,
    sub_models = sub_models,
    tau_grid_sub = tau_grid_sub,
    tau0 = tau0,
    tau1 = tau1,
    tau2 = tau2,
    tau3 = tau3,
    tau_target = tau_target,
    u = u,
    taus_full = taus_full
  )
  class(model) <- "qevt"
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("  → done in %.2fs (complete)", elapsed))
  model
}
