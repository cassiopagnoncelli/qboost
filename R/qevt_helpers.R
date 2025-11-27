# Helpers for qevt extreme quantile engine

# Exceedance classifier y > u
fit_exceedance_model <- function(X, y, tau0 = 0.997, params = list(), nrounds = 200) {
  u <- as.numeric(stats::quantile(y, probs = tau0, na.rm = TRUE))
  label <- as.integer(y > u)
  dtrain <- lightgbm::lgb.Dataset(data = data.matrix(X), label = label)
  params_default <- list(
    objective = "binary",
    metric = "binary_logloss",
    learning_rate = 0.05,
    num_leaves = 127,
    max_depth = -1
  )
  params_full <- utils::modifyList(params_default, params)
  model <- lightgbm::lgb.train(
    params = params_full,
    data = dtrain,
    nrounds = nrounds,
    verbose = -1
  )
  list(model = model, u = u, tau0 = tau0)
}

# GPD MLE on exceedances
fit_gpd <- function(y_tail, u) {
  z <- y_tail - u
  z <- z[z > 0 & is.finite(z)]
  if (length(z) < 5) {
    return(list(xi = NA_real_, beta = NA_real_, n = length(z), converged = FALSE))
  }
  gpd_nll <- function(theta, z) {
    log_beta <- theta[1]
    xi <- theta[2]
    beta <- exp(log_beta)
    if (beta <= 0) {
      return(Inf)
    }
    t <- 1 + xi * z / beta
    if (any(t <= 0)) {
      return(Inf)
    }
    if (abs(xi) < 1e-8) {
      return(length(z) * log(beta) + sum(z) / beta)
    }
    length(z) * log(beta) + (1 + 1 / xi) * sum(log(t))
  }
  init <- c(log(stats::sd(z)), 0.1)
  opt <- stats::optim(
    par = init,
    fn = gpd_nll,
    z = z,
    method = "BFGS",
    control = list(maxit = 1000)
  )
  beta_hat <- exp(opt$par[1])
  xi_hat <- opt$par[2]
  list(xi = xi_hat, beta = beta_hat, n = length(z), converged = opt$convergence == 0)
}

gpd_quantile <- function(p, xi, beta) {
  if (abs(xi) < 1e-10) {
    return(-beta * log(1 - p))
  }
  beta * ((1 - p)^(-xi) - 1) / xi
}

# Severity regression fallback on excess sizes
fit_severity_lgbm <- function(X_tail, z, params = list(), nrounds = 200) {
  dtrain <- lightgbm::lgb.Dataset(data = data.matrix(X_tail), label = z)
  params_default <- list(
    objective = "regression",
    metric = "l2",
    learning_rate = 0.05,
    num_leaves = 127,
    max_depth = -1
  )
  params_full <- utils::modifyList(params_default, params)
  lightgbm::lgb.train(
    params = params_full,
    data = dtrain,
    nrounds = nrounds,
    verbose = -1
  )
}

# Independent quantile models below threshold
fit_subquantile_models <- function(X, y, tau_grid_sub, params = list(), nrounds = 400) {
  models <- vector("list", length(tau_grid_sub))
  for (i in seq_along(tau_grid_sub)) {
    tau <- tau_grid_sub[i]
    dtrain <- lightgbm::lgb.Dataset(data = data.matrix(X), label = y)
    params_default <- list(
      objective = "quantile",
      alpha = tau,
      metric = "quantile",
      learning_rate = 0.05,
      num_leaves = 200,
      max_depth = 14
    )
    params_full <- utils::modifyList(params_default, params)
    models[[i]] <- lightgbm::lgb.train(
      params = params_full,
      data = dtrain,
      nrounds = nrounds,
      verbose = -1
    )
    names(models)[i] <- as.character(tau)
  }
  models
}

predict_exceedance <- function(model, X) {
  booster <- model$model
  .lgb_predict(booster, data.matrix(X))
}

predict_subquantiles <- function(models, X, tau_grid_sub = NULL) {
  if (is.null(tau_grid_sub)) {
    tau_grid_sub <- as.numeric(names(models))
  }
  preds <- lapply(tau_grid_sub, function(tau) {
    booster <- models[[as.character(tau)]]
    .lgb_predict(booster, data.matrix(X))
  })
  mat <- do.call(cbind, preds)
  colnames(mat) <- as.character(tau_grid_sub)
  mat
}

# EVT quantile at tau blended with exceedance prob
predict_evt_quantile <- function(X, p_exceed, xi, beta, u, tau, tau0) {
  tau_cond <- (tau - tau0) / (1 - tau0)
  tau_cond <- min(max(tau_cond, 0), 1 - 1e-12)
  q_sev <- gpd_quantile(tau_cond, xi, beta)
  q_evt <- u + q_sev
  q_evt * p_exceed + u * (1 - p_exceed)
}

# Row-wise PAVA monotonicity
apply_pava <- function(taus_full, q_vec_raw) {
  iso <- stats::isoreg(x = taus_full, y = q_vec_raw)
  as.numeric(iso$yf)
}
