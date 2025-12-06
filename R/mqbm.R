#' Fit multiplexed (multi-group) quantile regression models
#'
#' Trains separate \code{\link{qbm}} quantile regression models for each group
#' in the data. This multiplexer enables heterogeneous
#' quantile modeling where different subgroups can have distinct relationships
#' between predictors and outcomes. Particularly useful for multi-asset portfolios,
#' multi-site studies, or any scenario where group-specific quantile behavior differs.
#'
#' @param ... Either a formula with \code{data} argument (where \code{data} must
#'   contain a column specified by \code{multiplexer} identifying groups), or an \code{x}/\code{y}/\code{multiplexer}
#'   triplet for matrix input. Additional arguments are forwarded to \code{\link{qbm}}.
#' @param multiplexer Character string specifying the column name in \code{data} (for formula interface)
#'   or the name of the grouping variable to use as a multiplexer. Required parameter.
#' @param tau Target quantile level in (0, 1] applied to all group-specific models.
#'   Default is 0.5 (median).
#' @param nrounds Maximum number of boosting iterations per group model. Default is 500.
#' @param nfolds Number of cross-validation folds for each group model. Default is 5.
#' @param params Optional named list of LightGBM parameters shared across all
#'   group-specific models. See \code{\link{qbm}} for available parameters.
#' @param early_stopping_rounds Early stopping patience for each group model. Default is 50.
#' @param seed Random seed for reproducibility across all group models. Default is 1.
#' @param train_idx Optional integer vector of training indices (referring to full dataset rows).
#'   If provided with \code{val_idx}, performs single holdout validation for each group.
#'   Indices are automatically subset per group respecting group boundaries.
#'   Ignored if \code{folds} is specified.
#' @param val_idx Optional integer vector of validation indices (referring to full dataset rows).
#'   Must be provided together with \code{train_idx}. Ignored if \code{folds} is specified.
#' @param folds Optional list of integer vectors specifying custom fold structure for CV
#'   (referring to full dataset rows). Each element should contain validation indices for that fold.
#'   Takes priority over \code{train_idx}/\code{val_idx}. Indices are automatically subset per group.
#'
#' @return An object of class \code{mqbm} containing:
#'   \item{models}{Named list of fitted \code{\link{qbm}} objects, one per group}
#'   \item{multiplexer_values}{Character vector of unique group identifiers}
#'   \item{multiplexer_info}{List with sample size and indices for each group}
#'   \item{ecdf_funs}{Named list of ECDF functions, one per group, built from training y values}
#'   \item{tau}{The target quantile level used}
#'   \item{multiplexer}{Column name used for grouping}
#'   \item{preprocess}{Preprocessing information (for formula interface)}
#'   \item{timings}{Training time information}
#'   \item{data_info}{Dataset dimensions (n, p, n_multiplexer)}
#'   \item{training}{Training data (y and multiplexer vectors)}
#'
#' @details
#' The mqbm (multiplexed qbm) approach:
#' \enumerate{
#'   \item Partitions data by group
#'   \item Trains independent \code{\link{qbm}} models for each group
#'   \item Builds group-specific ECDF functions from training y values
#'   \item Maintains separate hyperparameters and feature importance per group
#'   \item Routes predictions to appropriate group-specific model
#'   \item Transforms predictions through group-specific ECDF to return probabilities
#' }
#'
#' This is beneficial when:
#' \itemize{
#'   \item Different groups have heterogeneous quantile behavior
#'   \item Sample sizes per group are sufficient for separate modeling
#'   \item Group-specific feature importance is of interest
#'   \item Interactions between predictors vary by group
#' }
#'
#' @seealso \code{\link{qbm}}, \code{\link{predict.mqbm}}, \code{\link{fitted.mqbm}},
#'   \code{\link{residuals.mqbm}}, \code{\link{coef.mqbm}}
#'
#' @examples
#' \dontrun{
#' # Multi-group financial data
#' set.seed(1)
#' df <- data.frame(
#'   x1 = rnorm(300),
#'   x2 = rnorm(300),
#'   cluster = sample(c("AAPL", "GOOGL", "MSFT"), 300, replace = TRUE)
#' )
#' # Different groups have different relationships
#' df$y <- ifelse(df$cluster == "AAPL",
#'                df$x1 * 2 + rnorm(300),
#'                df$x1 * 0.5 + df$x2 * 1.5 + rnorm(300))
#'
#' # Formula interface - data must contain multiplexer column
#' fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 100)
#' print(fit)
#'
#' # Predictions automatically route to correct group model
#' newdata <- data.frame(
#'   x1 = c(1, 1, 1),
#'   x2 = c(0, 0, 0),
#'   cluster = c("AAPL", "GOOGL", "MSFT")
#' )
#' predict(fit, newdata)
#'
#' # Matrix interface
#' X <- as.matrix(df[, c("x1", "x2")])
#' fit2 <- mqbm(x = X, y = df$y, multiplexer = df$cluster, tau = 0.75)
#'
#' # Access group-specific models
#' fit$models$AAPL  # qbm model for AAPL
#' coef(fit)  # Feature importance per group
#'
#' # Different quantiles per group
#' fit_lower <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.1)
#' fit_upper <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.9)
#' }
#' @export
mqbm <- function(
    ...,
    multiplexer,
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
  parsed <- .parse_mqbm_inputs(dots, multiplexer = multiplexer)

  x <- parsed$x
  y <- as.numeric(parsed$y)
  multiplexer_vector <- as.character(parsed$multiplexer_vector)
  extra_args <- parsed$extra_args

  if (nrow(x) != length(y) || nrow(x) != length(multiplexer_vector)) {
    stop("`x`, `y`, and `multiplexer` must have compatible dimensions.", call. = FALSE)
  }

  # Get unique group values
  unique_values <- unique(multiplexer_vector)
  if (length(unique_values) == 0) {
    stop("No groups found in the data.", call. = FALSE)
  }

  # Train one qbm per group
  models <- list()
  multiplexer_info <- list()
  ecdf_funs <- list()
  
  for (val in unique_values) {
    idx <- which(multiplexer_vector == val)
    x_val <- x[idx, , drop = FALSE]
    y_val <- y[idx]
    
    # Store group-specific info
    multiplexer_info[[val]] <- list(
      n = length(idx),
      indices = idx
    )
    
    # Build ECDF for this group's training y values
    ecdf_funs[[val]] <- stats::ecdf(y_val)
    
    # Subset and remap train/val indices for this group
    group_indices <- .subset_indices_for_group(
      global_idx = idx,
      train_idx = train_idx,
      val_idx = val_idx,
      folds = folds
    )
    
    # Train qbm for this group with group-specific indices
    if (parsed$preprocess$type == "formula") {
      # For formula interface, we need to reconstruct the call
      # We'll use the x/y interface for qbm since we've already processed the formula
      models[[val]] <- qbm(
        x = x_val,
        y = y_val,
        tau = tau,
        nrounds = nrounds,
        nfolds = nfolds,
        params = params,
        early_stopping_rounds = early_stopping_rounds,
        seed = seed,
        train_idx = group_indices$train_idx,
        val_idx = group_indices$val_idx,
        folds = group_indices$folds
      )
    } else {
      # Direct x/y interface
      models[[val]] <- qbm(
        x = x_val,
        y = y_val,
        tau = tau,
        nrounds = nrounds,
        nfolds = nfolds,
        params = params,
        early_stopping_rounds = early_stopping_rounds,
        seed = seed,
        train_idx = group_indices$train_idx,
        val_idx = group_indices$val_idx,
        folds = group_indices$folds
      )
    }
  }

  end_time <- Sys.time()

  out <- list(
    models = models,
    multiplexer_values = unique_values,
    multiplexer_info = multiplexer_info,
    ecdf_funs = ecdf_funs,
    tau = tau,
    multiplexer = multiplexer,
    preprocess = parsed$preprocess,
    timings = list(
      start = start_time,
      end = end_time,
      elapsed = as.numeric(difftime(end_time, start_time, units = "secs"))
    ),
    data_info = list(
      n = nrow(x),
      p = ncol(x),
      n_multiplexer = length(unique_values)
    ),
    training = list(
      y = y,
      multiplexer = multiplexer_vector
    )
  )

  class(out) <- "mqbm"
  out
}

.parse_mqbm_inputs <- function(dots, multiplexer) {
  if (length(dots) == 0) {
    stop(
      sprintf("Provide either a formula with data (containing '%s' column) or an `x`/`y`/`%s` triplet.", multiplexer, multiplexer),
      call. = FALSE
    )
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

    if (length(data_idx) == 0) {
      stop("When using formula interface, `data` must be provided.", call. = FALSE)
    }

    data <- dots[[data_idx]]
    
    if (!multiplexer %in% names(data)) {
      stop(sprintf("`data` must contain a '%s' column.", multiplexer), call. = FALSE)
    }

    # Extract multiplexer column before processing formula
    multiplexer_vector <- data[[multiplexer]]
    data_without_multiplexer <- data[, names(data) != multiplexer, drop = FALSE]

    mf <- stats::model.frame(formula, data = data_without_multiplexer, na.action = stats::na.pass)
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
      multiplexer_vector = multiplexer_vector,
      extra_args = extra_args,
      preprocess = preprocess
    ))
  }

  # x/y/multiplexer interface
  x_idx <- if ("x" %in% nm) which(nm == "x")[1] else 1L
  remaining <- setdiff(seq_along(dots), x_idx)
  if (length(remaining) == 0) {
    stop(sprintf("`y` and `%s` must be provided when using `x`/`y`/`%s` inputs.", multiplexer, multiplexer), call. = FALSE)
  }
  
  y_idx <- if ("y" %in% nm) {
    which(nm == "y")[1]
  } else {
    remaining[1]
  }
  
  remaining <- setdiff(remaining, y_idx)
  if (length(remaining) == 0) {
    stop(sprintf("`%s` must be provided when using `x`/`y`/`%s` inputs.", multiplexer, multiplexer), call. = FALSE)
  }
  
  # Get the multiplexer parameter
  multiplexer_idx <- if (multiplexer %in% nm) {
    which(nm == multiplexer)[1]
  } else {
    remaining[1]
  }

  x <- dots[[x_idx]]
  y <- dots[[y_idx]]
  multiplexer_vector <- dots[[multiplexer_idx]]

  extra_idx <- setdiff(seq_along(dots), c(x_idx, y_idx, multiplexer_idx))
  extra_args <- if (length(extra_idx) > 0) dots[extra_idx] else list()

  preprocess <- list(
    type = "xy",
    feature_names = colnames(x)
  )

  list(
    x = x,
    y = y,
    multiplexer_vector = multiplexer_vector,
    extra_args = extra_args,
    preprocess = preprocess
  )
}
