lgb_quantile_cv <- function(
    x, y, tau,
    nfold = 5,
    nrounds = 500,
    params = list(),
    verbose = 0
) {
  stopifnot(is.matrix(x))
  stopifnot(length(y) == nrow(x))

  # merge params with required quantile settings
  params_full <- modifyList(params, list(
    objective = "quantile",
    alpha = tau,
    metric = "quantile"
  ))

  # build dataset
  dtrain <- lightgbm::lgb.Dataset(x, label = y)

  # ---- 1. CV to get best iteration ----
  cv <- lightgbm::lgb.cv(
    params   = params_full,
    data     = dtrain,
    nfold    = nfold,
    nrounds  = nrounds,
    verbose  = verbose
  )

  best_iter <- cv$best_iter

  # ---- 2. Retrain final model on full data ----
  final_model <- lightgbm::lgb.train(
    params   = params_full,
    data     = dtrain,
    nrounds  = best_iter,
    verbose  = verbose
  )

  # ---- 3. Return a clean object with $predict() ----
  out <- list(
    model     = final_model,
    tau       = tau,
    best_iter = best_iter,

    predict = function(newx) {
      if (!is.matrix(newx)) newx <- as.matrix(newx)
      predict(final_model, newx)
    }
  )

  class(out) <- "lgb_quantile"
  out
}
