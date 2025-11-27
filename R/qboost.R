#' Fit a LightGBM quantile regression model
#'
#' Provides a clean, `lm`-style interface for quantile regression using
#' LightGBM. Accepts either a formula + data or an `x`/`y` pair, cross-validates
#' to find the best iteration, refits on full data, and computes rich
#' diagnostics and calibration metrics.
#'
#' @param ... Either a formula and optional `data` argument or an `x`/`y` pair
#'   followed by additional arguments forwarded to `lightgbm::lgb.cv()` /
#'   `lightgbm::lgb.train()`.
#' @param tau Target quantile in (0, 1].
#' @param nrounds Maximum number of boosting iterations.
#' @param nfolds Number of cross-validation folds.
#' @param params Optional named list of extra LightGBM parameters.
#' @param early_stopping_rounds Early stopping patience in CV.
#' @param seed Random seed for reproducibility.
#'
#' @return An object of class `qboost`.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200)
#' )
#' df$y <- df$x1 * 0.5 + rnorm(200)
#' fit <- qboost(y ~ x1 + x2, data = df, tau = 0.3, nrounds = 50, nfolds = 3)
#' predict(fit, head(df[, c("x1", "x2")]))
#' }
qboost <- function(
    ...,
    tau = 0.5,
    nrounds = 500,
    nfolds = 5,
    params = list(),
    early_stopping_rounds = 50,
    seed = 1) {
  start_time <- Sys.time()

  dots <- list(...)
  parsed <- .parse_qboost_inputs(dots)

  x <- parsed$x
  y <- as.numeric(parsed$y)
  extra_args <- parsed$extra_args

  if (!is.matrix(x)) {
    x <- data.matrix(x)
  }
  parsed$preprocess$feature_names <- colnames(x)
  if (nrow(x) != length(y)) {
    stop("`x` and `y` must have compatible dimensions.", call. = FALSE)
  }
  if (tau <= 0 || tau > 1) {
    stop("`tau` must be in (0, 1].", call. = FALSE)
  }

  params_default <- list(
    objective = "quantile",
    alpha = tau,
    metric = "quantile",
    num_leaves = 200,
    max_depth = 14
  )
  params_full <- utils::modifyList(params_default, params)

  dtrain <- lightgbm::lgb.Dataset(data = x, label = y)

  set.seed(seed)
  cv_args <- .merge_lgb_args(
    list(
      params = params_full,
      data = dtrain,
      nrounds = nrounds,
      nfold = nfolds,
      early_stopping_rounds = early_stopping_rounds,
      verbose = -1
    ),
    extra_args
  )
  cv <- do.call(lightgbm::lgb.cv, cv_args)

  best_iter <- if (!is.null(cv$best_iter)) cv$best_iter else nrounds

  set.seed(seed)
  train_args <- .merge_lgb_args(
    list(
      params = params_full,
      data = dtrain,
      nrounds = best_iter,
      verbose = -1
    ),
    extra_args
  )
  final_model <- do.call(lightgbm::lgb.train, train_args)

  fitted <- .lgb_predict(final_model, x)

  train_metrics <- compute_qboost_metrics(
    y = y,
    yhat = fitted,
    tau = tau,
    cv_result = cv,
    model = final_model,
    best_iter = best_iter
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
    residuals = train_metrics$residuals,
    timings = list(
      start = start_time,
      end = end_time,
      elapsed = as.numeric(difftime(end_time, start_time, units = "secs"))
    ),
    data_info = list(
      n = nrow(x),
      p = ncol(x)
    ),
    cv_settings = list(
      nrounds = nrounds,
      nfolds = nfolds,
      early_stopping_rounds = early_stopping_rounds
    ),
    preprocess = parsed$preprocess,
    params_used = params_full,
    training = list(
      y = y,
      fitted = fitted
    )
  )

  class(out) <- "qboost"
  out
}

.parse_qboost_inputs <- function(dots) {
  if (length(dots) == 0) {
    stop("Provide either a formula or an `x`/`y` pair.", call. = FALSE)
  }

  nm <- names(dots)
  if (is.null(nm)) {
    nm <- rep("", length(dots))
  }
  nm[is.na(nm)] <- ""

  is_formula <- vapply(dots, inherits, logical(1), what = "formula")
  if (any(is_formula)) {
    formula_idx <- which(is_formula)[1]
    formula <- dots[[formula_idx]]

    data_idx <- integer(0)
    if ("data" %in% nm) {
      data_idx <- which(nm == "data")[1]
    } else {
      unnamed <- which(!nzchar(nm))
      unnamed <- unnamed[unnamed != formula_idx]
      if (length(unnamed) > 0) {
        data_idx <- unnamed[1]
      }
    }

    data <- if (length(data_idx) > 0) dots[[data_idx]] else NULL

    mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
    y <- stats::model.response(mf)
    mm <- stats::model.matrix(formula, mf)
    terms_obj <- stats::terms(mf)

    preprocess <- list(
      type = "formula",
      terms = terms_obj,
      xlevels = stats::.getXlevels(terms_obj, mf),
      contrasts = attr(mm, "contrasts"),
      formula = formula,
      feature_names = colnames(mm)
    )

    extra_idx <- setdiff(seq_along(dots), c(formula_idx, data_idx))
    extra_args <- if (length(extra_idx) > 0) dots[extra_idx] else list()

    return(list(
      x = mm,
      y = y,
      extra_args = extra_args,
      preprocess = preprocess
    ))
  }

  x_idx <- if ("x" %in% nm) which(nm == "x")[1] else 1L
  remaining <- setdiff(seq_along(dots), x_idx)
  if (length(remaining) == 0) {
    stop("`y` must be provided when using `x`/`y` inputs.", call. = FALSE)
  }
  y_idx <- if ("y" %in% nm) which(nm == "y")[1] else remaining[1]

  x <- dots[[x_idx]]
  y <- dots[[y_idx]]

  extra_idx <- setdiff(seq_along(dots), c(x_idx, y_idx))
  extra_args <- if (length(extra_idx) > 0) dots[extra_idx] else list()

  preprocess <- list(
    type = "xy",
    feature_names = colnames(x)
  )

  list(
    x = x,
    y = y,
    extra_args = extra_args,
    preprocess = preprocess
  )
}

.merge_lgb_args <- function(base_args, extra_args) {
  if (length(extra_args) == 0) {
    return(base_args)
  }

  extra_names <- names(extra_args)
  if (is.null(extra_names)) {
    extra_names <- rep("", length(extra_args))
  }
  extra_names[is.na(extra_names)] <- ""

  for (i in seq_along(extra_args)) {
    nm <- extra_names[i]
    if (!nzchar(nm)) {
      base_args <- append(base_args, list(extra_args[[i]]))
    } else {
      if (nm %in% c("data", "label", "params")) {
        stop("`", nm, "` cannot be supplied via `...`.", call. = FALSE)
      }
      base_args[[nm]] <- extra_args[[i]]
    }
  }

  base_args
}
