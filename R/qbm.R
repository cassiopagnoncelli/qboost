#' Fit a LightGBM quantile regression model
#'
#' Provides a clean, `lm`-style interface for quantile regression using
#' LightGBM gradient boosting. The function supports both formula and matrix
#' input, automatically performs cross-validation to determine optimal iterations,
#' refits on the full training data, and computes comprehensive diagnostics
#' including calibration metrics, feature importance, and residual statistics.
#'
#' @param ... Either a formula and optional `data` argument, or an `x`/`y` pair.
#'   Additional arguments are forwarded to \code{lightgbm::lgb.cv()} and
#'   \code{lightgbm::lgb.train()}.
#' @param tau Target quantile level in (0, 1]. Use 0.5 for median regression,
#'   values < 0.5 for lower quantiles, and values > 0.5 for upper quantiles.
#'   Default is 0.5 (median).
#' @param nrounds Maximum number of boosting iterations. The actual number used
#'   may be less if early stopping is triggered. Default is 500.
#' @param nfolds Number of cross-validation folds for determining the optimal
#'   number of iterations. Default is 5.
#' @param params Optional named list of additional LightGBM parameters to override
#'   defaults. Common parameters include \code{learning_rate}, \code{num_leaves},
#'   \code{max_depth}, \code{min_data_in_leaf}, and \code{feature_fraction}.
#' @param early_stopping_rounds Number of rounds without improvement in CV metric
#'   before stopping. Default is 50.
#' @param seed Random seed for reproducibility of cross-validation splits and
#'   model training. Default is 1.
#' @param train_idx Optional integer vector of training indices for single train/val split.
#'   If provided with \code{val_idx}, performs single holdout validation instead of k-fold CV.
#'   Ignored if \code{folds} is specified.
#' @param val_idx Optional integer vector of validation indices for single train/val split.
#'   Must be provided together with \code{train_idx}. Ignored if \code{folds} is specified.
#' @param folds Optional list of integer vectors specifying custom fold structure for CV.
#'   Each element should contain validation indices for that fold. Takes priority over
#'   \code{train_idx}/\code{val_idx} and automatic k-fold generation.
#'
#' @return An object of class \code{qbm} containing:
#'   \item{model}{The fitted LightGBM model object}
#'   \item{tau}{The target quantile level}
#'   \item{best_iter}{Optimal number of iterations from cross-validation}
#'   \item{metrics}{Training metrics (pinball loss, MAE, pseudo-R²)}
#'   \item{calibration}{Calibration statistics (coverage, QCE, slope/intercept)}
#'   \item{tails}{Tail spread metrics}
#'   \item{complexity}{Model complexity measures (leaves, gain per leaf, entropy)}
#'   \item{importance}{Feature importance data frame}
#'   \item{residuals}{Residual statistics (median, IQR, skewness)}
#'   \item{timings}{Timing information}
#'   \item{data_info}{Dataset dimensions}
#'   \item{cv_settings}{Cross-validation settings}
#'   \item{preprocess}{Preprocessing information (for formula interface)}
#'   \item{params_used}{Final hyperparameters used}
#'   \item{training}{Training data (y and fitted values)}
#'
#' @details
#' The model fitting process includes:
#' \enumerate{
#'   \item Cross-validation to determine the optimal number of boosting iterations
#'   \item Retraining on the full dataset using the optimal iterations
#'   \item Computation of comprehensive metrics and diagnostics
#'   \item Feature importance calculation
#' }
#'
#' Default LightGBM parameters are optimized for quantile regression:
#' \itemize{
#'   \item \code{objective = "quantile"}
#'   \item \code{alpha = tau} (quantile level)
#'   \item \code{metric = "quantile"}
#'   \item \code{num_leaves = 200}
#'   \item \code{max_depth = 14}
#' }
#'
#' @seealso \code{\link{predict.qbm}}, \code{\link{summary.qbm}}, \code{\link{plot.qbm}},
#'   \code{\link{residuals.qbm}}, \code{\link{fitted.qbm}}, \code{\link{coef.qbm}}
#'
#' @examples
#' \dontrun{
#' # Formula interface
#' set.seed(1)
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   x3 = rnorm(200)
#' )
#' df$y <- df$x1 * 0.5 + df$x2 * 0.3 + rnorm(200)
#'
#' # Fit median regression (tau = 0.5)
#' fit_median <- qbm(y ~ x1 + x2 + x3, data = df, tau = 0.5, nrounds = 100)
#' print(fit_median)
#' summary(fit_median)
#'
#' # Fit upper quantile (tau = 0.9)
#' fit_upper <- qbm(y ~ x1 + x2 + x3, data = df, tau = 0.9, nrounds = 100)
#'
#' # Predictions
#' newdata <- data.frame(x1 = c(0, 1), x2 = c(0, 1), x3 = c(0, 1))
#' predict(fit_median, newdata)
#' predict(fit_upper, newdata)
#'
#' # Matrix interface
#' X <- as.matrix(df[, c("x1", "x2", "x3")])
#' y <- df$y
#' fit_matrix <- qbm(x = X, y = y, tau = 0.5, nrounds = 100)
#'
#' # Custom hyperparameters
#' fit_custom <- qbm(
#'   y ~ x1 + x2 + x3,
#'   data = df,
#'   tau = 0.75,
#'   params = list(
#'     learning_rate = 0.05,
#'     num_leaves = 100,
#'     max_depth = 10
#'   )
#' )
#' }
#' @export
qbm <- function(
    ...,
    tau = 0.5,
    nrounds = 500,
    nfolds = 5,
    params = list(),
    early_stopping_rounds = 50,
    seed = 1,
    train_idx = NULL,
    val_idx = NULL,
    folds = NULL) {
  start_time <- Sys.time()

  dots <- list(...)
  parsed <- .parse_qbm_inputs(dots)

  x <- parsed$x
  y <- as.numeric(parsed$y)
  extra_args <- parsed$extra_args

  # Always convert to plain numeric matrix to strip any special attributes
  # from model.matrix() that lightgbm cannot handle
  feature_names <- colnames(x)
  if (!is.matrix(x)) {
    x <- data.matrix(x)
  } else {
    # Strip all attributes and create a fresh plain numeric matrix
    x <- matrix(as.numeric(x), nrow = nrow(x), ncol = ncol(x))
    colnames(x) <- feature_names
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

  # Validate and prepare fold specification
  fold_spec <- .prepare_fold_specification(
    n = nrow(x),
    folds = folds,
    train_idx = train_idx,
    val_idx = val_idx,
    nfolds = nfolds,
    seed = seed
  )

  # Determine CV data subset based on indices
  if (fold_spec$type == "train_val_split") {
    # Use k-fold CV on combined train+val data
    cv_indices <- c(fold_spec$train_idx, fold_spec$val_idx)
    dtrain_cv <- lightgbm::lgb.Dataset(
      data = x[cv_indices, , drop = FALSE],
      label = y[cv_indices]
    )
  } else if (!is.null(fold_spec$train_idx)) {
    # Only train_idx provided (no val_idx) - use train_idx for CV
    dtrain_cv <- lightgbm::lgb.Dataset(
      data = x[fold_spec$train_idx, , drop = FALSE],
      label = y[fold_spec$train_idx]
    )
  } else {
    # No indices specified - use full data
    dtrain_cv <- lightgbm::lgb.Dataset(data = x, label = y)
  }
  
  # Perform k-fold cross-validation
  if (fold_spec$type == "train_val_split" || fold_spec$type == "auto_kfold") {
    # Standard k-fold CV - don't use early_stopping_rounds or valids for CV
    set.seed(seed)
    cv_args <- .merge_lgb_args(
      list(
        params = params_full,
        data = dtrain_cv,
        nrounds = nrounds,
        nfold = nfolds,
        verbose = -1
      ),
      extra_args
    )
    # Remove early_stopping_rounds and valids if accidentally included
    cv_args$early_stopping_rounds <- NULL
    cv_args$valids <- NULL
    
    cv <- do.call(lightgbm::lgb.cv, cv_args)
  } else {
    # Custom folds - don't use early_stopping_rounds or valids for CV
    dtrain_cv <- lightgbm::lgb.Dataset(data = x, label = y)
    
    set.seed(seed)
    cv_args <- .merge_lgb_args(
      list(
        params = params_full,
        data = dtrain_cv,
        nrounds = nrounds,
        folds = fold_spec$folds,
        verbose = -1
      ),
      extra_args
    )
    # Remove early_stopping_rounds and valids if accidentally included
    cv_args$early_stopping_rounds <- NULL
    cv_args$valids <- NULL
    
    cv <- do.call(lightgbm::lgb.cv, cv_args)
  }

  best_iter <- if (!is.null(cv$best_iter)) cv$best_iter else nrounds

  # Train final model
  if (fold_spec$use_subset) {
    # Train on train_idx only
    dtrain_final <- lightgbm::lgb.Dataset(
      data = x[fold_spec$train_idx, , drop = FALSE],
      label = y[fold_spec$train_idx]
    )
  } else {
    # Train on full data
    dtrain_final <- dtrain_cv
  }

  set.seed(seed)
  train_args <- .merge_lgb_args(
    list(
      params = params_full,
      data = dtrain_final,
      nrounds = best_iter,
      verbose = -1
    ),
    extra_args
  )
  # Remove early_stopping_rounds and valids from final training (no validation set)
  train_args$early_stopping_rounds <- NULL
  train_args$valids <- NULL
  
  final_model <- do.call(lightgbm::lgb.train, train_args)

  # Get fitted values and compute metrics
  if (fold_spec$use_subset) {
    # Separate train and validation predictions
    fitted_train <- .lgb_predict(final_model, x[fold_spec$train_idx, , drop = FALSE])
    fitted_val <- .lgb_predict(final_model, x[fold_spec$val_idx, , drop = FALSE])
    
    # Create full fitted vector (val indices are NA for compatibility)
    fitted <- numeric(nrow(x))
    fitted[fold_spec$train_idx] <- fitted_train
    fitted[fold_spec$val_idx] <- NA_real_
    
    # Compute separate metrics for train and validation
    train_metrics <- compute_qbm_metrics(
      y = y[fold_spec$train_idx],
      yhat = fitted_train,
      tau = tau,
      cv_result = cv,
      model = final_model,
      best_iter = best_iter
    )
    
    val_metrics <- compute_qbm_metrics(
      y = y[fold_spec$val_idx],
      yhat = fitted_val,
      tau = tau,
      cv_result = NULL,
      model = NULL,
      best_iter = NULL
    )
  } else {
    # No train/val split - compute on full data
    fitted <- .lgb_predict(final_model, x)
    
    train_metrics <- compute_qbm_metrics(
      y = y,
      yhat = fitted,
      tau = tau,
      cv_result = cv,
      model = final_model,
      best_iter = best_iter
    )
    
    val_metrics <- NULL
  }

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
    validation = val_metrics,
    timings = list(
      start = start_time,
      end = end_time,
      elapsed = as.numeric(difftime(end_time, start_time, units = "secs"))
    ),
    data_info = list(
      n = nrow(x),
      p = ncol(x),
      n_train = if (fold_spec$use_subset) length(fold_spec$train_idx) else nrow(x),
      n_val = if (fold_spec$use_subset) length(fold_spec$val_idx) else 0L
    ),
    cv_settings = list(
      nrounds = nrounds,
      nfolds = nfolds,
      early_stopping_rounds = early_stopping_rounds,
      fold_type = fold_spec$type,
      train_idx = if (fold_spec$use_subset) fold_spec$train_idx else NULL,
      val_idx = if (fold_spec$use_subset) fold_spec$val_idx else NULL
    ),
    preprocess = parsed$preprocess,
    params_used = params_full,
    training = list(
      y = y,
      fitted = fitted
    )
  )

  class(out) <- "qbm"
  out
}

.parse_qbm_inputs <- function(dots) {
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
    # Remove intercept column if present (LightGBM doesn't need it)
    if ("(Intercept)" %in% colnames(mm)) {
      mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
    }
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

#' Prepare fold specification for cross-validation
#'
#' @param n Number of observations
#' @param folds Optional list of validation indices
#' @param train_idx Optional training indices
#' @param val_idx Optional validation indices
#' @param nfolds Number of folds for automatic k-fold
#' @param seed Random seed
#' @return List with fold specification details
#' @keywords internal
.prepare_fold_specification <- function(n, folds, train_idx, val_idx, nfolds, seed) {
  # Priority 1: Custom folds
  if (!is.null(folds)) {
    if (!is.list(folds)) {
      stop("`folds` must be a list of integer vectors.", call. = FALSE)
    }
    if (length(folds) == 0) {
      stop("`folds` must contain at least one fold.", call. = FALSE)
    }
    # Validate each fold
    for (i in seq_along(folds)) {
      fold_idx <- folds[[i]]
      if (!is.numeric(fold_idx) && !is.integer(fold_idx)) {
        stop(sprintf("`folds[[%d]]` must be an integer vector.", i), call. = FALSE)
      }
      if (any(fold_idx < 1 | fold_idx > n)) {
        stop(sprintf("`folds[[%d]]` contains invalid indices.", i), call. = FALSE)
      }
    }
    return(list(
      type = "custom_folds",
      folds = folds,
      use_subset = FALSE,
      train_idx = NULL,
      val_idx = NULL
    ))
  }
  
  # Priority 2: Train/val split
  if (!is.null(train_idx) || !is.null(val_idx)) {
    if (is.null(train_idx) || is.null(val_idx)) {
      stop("`train_idx` and `val_idx` must both be provided for train/val split.", call. = FALSE)
    }
    if (!is.numeric(train_idx) && !is.integer(train_idx)) {
      stop("`train_idx` must be an integer vector.", call. = FALSE)
    }
    if (!is.numeric(val_idx) && !is.integer(val_idx)) {
      stop("`val_idx` must be an integer vector.", call. = FALSE)
    }
    if (any(train_idx < 1 | train_idx > n)) {
      stop("`train_idx` contains invalid indices.", call. = FALSE)
    }
    if (any(val_idx < 1 | val_idx > n)) {
      stop("`val_idx` contains invalid indices.", call. = FALSE)
    }
    if (any(train_idx %in% val_idx)) {
      stop("`train_idx` and `val_idx` must not overlap.", call. = FALSE)
    }
    
    # Create single fold for LightGBM
    folds_list <- list(val_idx)
    
    return(list(
      type = "train_val_split",
      folds = folds_list,
      use_subset = TRUE,
      train_idx = train_idx,
      val_idx = val_idx
    ))
  }
  
  # Priority 3: Automatic k-fold (default)
  list(
    type = "auto_kfold",
    folds = NULL,
    use_subset = FALSE,
    train_idx = NULL,
    val_idx = NULL
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
