#' Fit symbol-based (multi-group) quantile regression models
#'
#' Trains separate \code{\link{qbm}} quantile regression models for each symbol
#' (group/label) in the data. This symbol-based multiplexer enables heterogeneous
#' quantile modeling where different subgroups can have distinct relationships
#' between predictors and outcomes. Particularly useful for multi-asset portfolios,
#' multi-site studies, or any scenario where group-specific quantile behavior differs.
#'
#' @param ... Either a formula with \code{data} argument (where \code{data} must
#'   contain a column specified by \code{multi} identifying groups), or an \code{x}/\code{y}/\code{multi}
#'   triplet for matrix input. Additional arguments are forwarded to \code{\link{qbm}}.
#' @param multi Character string specifying the column name in \code{data} (for formula interface)
#'   or the name of the grouping variable to use as a multiplexer. Default is \code{"symbol"}.
#' @param tau Target quantile level in (0, 1] applied to all symbol-specific models.
#'   Default is 0.5 (median).
#' @param nrounds Maximum number of boosting iterations per symbol model. Default is 500.
#' @param nfolds Number of cross-validation folds for each symbol model. Default is 5.
#' @param params Optional named list of LightGBM parameters shared across all
#'   symbol-specific models. See \code{\link{qbm}} for available parameters.
#' @param early_stopping_rounds Early stopping patience for each symbol model. Default is 50.
#' @param seed Random seed for reproducibility across all symbol models. Default is 1.
#'
#' @return An object of class \code{mqbm} containing:
#'   \item{models}{Named list of fitted \code{\link{qbm}} objects, one per symbol}
#'   \item{symbols}{Character vector of unique symbol identifiers}
#'   \item{symbol_info}{List with sample size and indices for each symbol}
#'   \item{ecdf_funs}{Named list of ECDF functions, one per symbol, built from training y values}
#'   \item{tau}{The target quantile level used}
#'   \item{preprocess}{Preprocessing information (for formula interface)}
#'   \item{timings}{Training time information}
#'   \item{data_info}{Dataset dimensions (n, p, n_symbols)}
#'   \item{training}{Training data (y and symbol vectors)}
#'
#' @details
#' The mqbm (multi-symbol qbm) approach:
#' \enumerate{
#'   \item Partitions data by symbol/group
#'   \item Trains independent \code{\link{qbm}} models for each symbol
#'   \item Builds symbol-specific ECDF functions from training y values
#'   \item Maintains separate hyperparameters and feature importance per symbol
#'   \item Routes predictions to appropriate symbol-specific model
#'   \item Transforms predictions through symbol-specific ECDF to return probabilities
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
#' # Multi-symbol financial data
#' set.seed(1)
#' df <- data.frame(
#'   x1 = rnorm(300),
#'   x2 = rnorm(300),
#'   symbol = sample(c("AAPL", "GOOGL", "MSFT"), 300, replace = TRUE)
#' )
#' # Different symbols have different relationships
#' df$y <- ifelse(df$symbol == "AAPL",
#'                df$x1 * 2 + rnorm(300),
#'                df$x1 * 0.5 + df$x2 * 1.5 + rnorm(300))
#'
#' # Formula interface - data must contain 'symbol' column
#' fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 100)
#' print(fit)
#'
#' # Predictions automatically route to correct symbol model
#' newdata <- data.frame(
#'   x1 = c(1, 1, 1),
#'   x2 = c(0, 0, 0),
#'   symbol = c("AAPL", "GOOGL", "MSFT")
#' )
#' predict(fit, newdata)
#'
#' # Matrix interface
#' X <- as.matrix(df[, c("x1", "x2")])
#' fit2 <- mqbm(x = X, y = df$y, symbol = df$symbol, tau = 0.75)
#'
#' # Access symbol-specific models
#' fit$models$AAPL  # qbm model for AAPL
#' coef(fit)  # Feature importance per symbol
#'
#' # Different quantiles per symbol
#' fit_lower <- mqbm(y ~ x1 + x2, data = df, tau = 0.1)
#' fit_upper <- mqbm(y ~ x1 + x2, data = df, tau = 0.9)
#' }
#' @export
mqbm <- function(
    ...,
    multi = "symbol",
    tau = 0.5,
    nrounds = 500,
    nfolds = 5,
    params = list(),
    early_stopping_rounds = 50,
    seed = 1) {
  start_time <- Sys.time()

  dots <- list(...)
  parsed <- .parse_mqbm_inputs(dots, multi = multi)

  x <- parsed$x
  y <- as.numeric(parsed$y)
  symbol <- as.character(parsed$symbol)
  extra_args <- parsed$extra_args

  if (nrow(x) != length(y) || nrow(x) != length(symbol)) {
    stop("`x`, `y`, and `symbol` must have compatible dimensions.", call. = FALSE)
  }

  # Get unique symbols
  unique_symbols <- unique(symbol)
  if (length(unique_symbols) == 0) {
    stop("No symbols found in the data.", call. = FALSE)
  }

  # Train one qbm per symbol
  models <- list()
  symbol_info <- list()
  ecdf_funs <- list()
  
  for (sym in unique_symbols) {
    idx <- which(symbol == sym)
    x_sym <- x[idx, , drop = FALSE]
    y_sym <- y[idx]
    
    # Store symbol-specific info
    symbol_info[[sym]] <- list(
      n = length(idx),
      indices = idx
    )
    
    # Build ECDF for this symbol's training y values
    ecdf_funs[[sym]] <- stats::ecdf(y_sym)
    
    # Train qbm for this symbol
    if (parsed$preprocess$type == "formula") {
      # For formula interface, we need to reconstruct the call
      # We'll use the x/y interface for qbm since we've already processed the formula
      models[[sym]] <- qbm(
        x = x_sym,
        y = y_sym,
        tau = tau,
        nrounds = nrounds,
        nfolds = nfolds,
        params = params,
        early_stopping_rounds = early_stopping_rounds,
        seed = seed
      )
    } else {
      # Direct x/y interface
      models[[sym]] <- qbm(
        x = x_sym,
        y = y_sym,
        tau = tau,
        nrounds = nrounds,
        nfolds = nfolds,
        params = params,
        early_stopping_rounds = early_stopping_rounds,
        seed = seed
      )
    }
  }

  end_time <- Sys.time()

  out <- list(
    models = models,
    symbols = unique_symbols,
    symbol_info = symbol_info,
    ecdf_funs = ecdf_funs,
    tau = tau,
    multi = multi,
    preprocess = parsed$preprocess,
    timings = list(
      start = start_time,
      end = end_time,
      elapsed = as.numeric(difftime(end_time, start_time, units = "secs"))
    ),
    data_info = list(
      n = nrow(x),
      p = ncol(x),
      n_symbols = length(unique_symbols)
    ),
    training = list(
      y = y,
      symbol = symbol
    )
  )

  class(out) <- "mqbm"
  out
}

.parse_mqbm_inputs <- function(dots, multi = "symbol") {
  if (length(dots) == 0) {
    stop(
      sprintf("Provide either a formula with data (containing '%s' column) or an `x`/`y`/`%s` triplet.", multi, multi),
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
    
    if (!multi %in% names(data)) {
      stop(sprintf("`data` must contain a '%s' column.", multi), call. = FALSE)
    }

    # Extract symbol before processing formula
    symbol <- data[[multi]]
    data_without_symbol <- data[, names(data) != multi, drop = FALSE]

    mf <- stats::model.frame(formula, data = data_without_symbol, na.action = stats::na.pass)
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
      symbol = symbol,
      extra_args = extra_args,
      preprocess = preprocess
    ))
  }

  # x/y/multi interface
  x_idx <- if ("x" %in% nm) which(nm == "x")[1] else 1L
  remaining <- setdiff(seq_along(dots), x_idx)
  if (length(remaining) == 0) {
    stop(sprintf("`y` and `%s` must be provided when using `x`/`y`/`%s` inputs.", multi, multi), call. = FALSE)
  }
  
  y_idx <- if ("y" %in% nm) {
    which(nm == "y")[1]
  } else {
    remaining[1]
  }
  
  remaining <- setdiff(remaining, y_idx)
  if (length(remaining) == 0) {
    stop(sprintf("`%s` must be provided when using `x`/`y`/`%s` inputs.", multi, multi), call. = FALSE)
  }
  
  # Try both the multi parameter name and "symbol" for backward compatibility
  symbol_idx <- if (multi %in% nm) {
    which(nm == multi)[1]
  } else if ("symbol" %in% nm) {
    which(nm == "symbol")[1]
  } else {
    remaining[1]
  }

  x <- dots[[x_idx]]
  y <- dots[[y_idx]]
  symbol <- dots[[symbol_idx]]

  extra_idx <- setdiff(seq_along(dots), c(x_idx, y_idx, symbol_idx))
  extra_args <- if (length(extra_idx) > 0) dots[extra_idx] else list()

  preprocess <- list(
    type = "xy",
    feature_names = colnames(x)
  )

  list(
    x = x,
    y = y,
    symbol = symbol,
    extra_args = extra_args,
    preprocess = preprocess
  )
}
