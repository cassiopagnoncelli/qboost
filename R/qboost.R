#' Fit a LightGBM quantile regression model
#'
#' Provides a clean, `lm`-style interface for quantile regression using LightGBM.
#' Cross-validates to find the best iteration, refits on full data, and computes
#' rich diagnostics and calibration metrics.
#'
#' @param x A matrix or data.frame of predictors.
#' @param y Numeric response vector.
#' @param tau Target quantile in (0, 1].
#' @param nrounds Maximum number of boosting iterations.
#' @param nfolds Number of cross-validation folds.
#' @param params Optional named list of extra LightGBM parameters.
#' @param early_stopping_rounds Early stopping patience in CV.
#' @param seed Random seed for reproducibility.
#' @param ... Additional arguments forwarded to `lightgbm::lgb.cv()` / `lgb.train()`.
#'
#' @return An object of class `qboost`.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' x <- matrix(rnorm(200), ncol = 2)
#' y <- x[, 1] * 0.5 + rnorm(100)
#' fit <- qboost(x, y, tau = 0.3, nrounds = 50, nfolds = 3)
#' predict(fit, x[1:5, ])
#' }
qboost <- function(
    x,
    y,
    tau = 0.5,
    nrounds = 500,
    nfolds = 5,
    params = list(),
    early_stopping_rounds = 50,
    seed = 1,
    ...
) {
  start_time <- Sys.time()

  if (!is.matrix(x)) {
    x <- data.matrix(x)
  }
  y <- as.numeric(y)
  if (nrow(x) != length(y)) {
    stop("`x` and `y` must have compatible dimensions.", call. = FALSE)
  }
  if (tau <= 0 || tau > 1) {
    stop("`tau` must be in (0, 1].", call. = FALSE)
  }

  params_default <- list(
    objective = "quantile",
    alpha = tau,
    metric = "quantile"
  )
  params_full <- utils::modifyList(params_default, params)

  dtrain <- lightgbm::lgb.Dataset(data = x, label = y)

  set.seed(seed)
  cv <- lightgbm::lgb.cv(
    params = params_full,
    data = dtrain,
    nrounds = nrounds,
    nfold = nfolds,
    early_stopping_rounds = early_stopping_rounds,
    verbose = -1,
    ...
  )

  best_iter <- cv$best_iter %||% nrounds

  set.seed(seed)
  final_model <- lightgbm::lgb.train(
    params = params_full,
    data = dtrain,
    nrounds = best_iter,
    verbose = -1,
    ...
  )

  fitted <- stats::predict(final_model, x)

  train_metrics <- compute_qboost_metrics(
    y = y,
    yhat = fitted,
    tau = tau,
    cv_result = cv,
    model = final_model
  )

  importance_df <- tidy_importance(final_model)

  end_time <- Sys.time()

  out <- list(
    model = final_model,
    tau = tau,
    best_iter = best_iter,
    metrics = train_metrics$metrics,
    calibration = train_metrics$calibration,
    tails = train_metrics$tails,
    complexity = train_metrics$complexity,
    importance = importance_df,
    timings = list(
      start = start_time,
      end = end_time,
      elapsed = as.numeric(difftime(end_time, start_time, units = "secs"))
    ),
    params_used = params_full,
    training = list(
      y = y,
      fitted = fitted
    )
  )

  class(out) <- "qboost"
  out
}

# internal helper mirroring `%||%` from rlang for lightweight use
`%||%` <- function(x, y) if (is.null(x)) y else x
