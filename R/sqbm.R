#' Fit symbol-based LightGBM quantile regression models
#'
#' Trains separate `qbm` models for each symbol/label in the data. This allows
#' for heterogeneous quantile models where different subgroups (symbols) have
#' their own prediction models.
#'
#' @param ... Either a formula and optional `data` argument (where `data` must
#'   contain a `symbol` column), or an `x`/`y`/`symbol` triplet, followed by
#'   additional arguments forwarded to `qbm()`.
#' @param tau Target quantile in (0, 1].
#' @param nrounds Maximum number of boosting iterations.
#' @param nfolds Number of cross-validation folds.
#' @param params Optional named list of extra LightGBM parameters.
#' @param early_stopping_rounds Early stopping patience in CV.
#' @param seed Random seed for reproducibility.
#'
#' @return An object of class `mqbm` containing one fitted `qbm` model per symbol.
#' @export
#'
#' @examples
#' \dontrun{
#' set.seed(1)
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
#' )
#' df$y <- df$x1 * 0.5 + rnorm(200)
#' 
#' # Formula interface
#' fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.3, nrounds = 50, nfolds = 3)
#' 
#' # x/y/symbol interface
#' fit2 <- mqbm(x = df[, c("x1", "x2")], y = df$y, symbol = df$symbol,
#'              tau = 0.3, nrounds = 50, nfolds = 3)
#' }
mqbm <- function(
    ...,
    tau = 0.5,
    nrounds = 500,
    nfolds = 5,
    params = list(),
    early_stopping_rounds = 50,
    seed = 1) {
  start_time <- Sys.time()

  dots <- list(...)
  parsed <- .parse_mqbm_inputs(dots)

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
  
  for (sym in unique_symbols) {
    idx <- which(symbol == sym)
    x_sym <- x[idx, , drop = FALSE]
    y_sym <- y[idx]
    
    # Store symbol-specific info
    symbol_info[[sym]] <- list(
      n = length(idx),
      indices = idx
    )
    
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
    tau = tau,
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

.parse_mqbm_inputs <- function(dots) {
  if (length(dots) == 0) {
    stop("Provide either a formula with data (containing 'symbol' column) or an `x`/`y`/`symbol` triplet.", call. = FALSE)
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
    
    if (!"symbol" %in% names(data)) {
      stop("`data` must contain a 'symbol' column.", call. = FALSE)
    }

    # Extract symbol before processing formula
    symbol <- data$symbol
    data_without_symbol <- data[, names(data) != "symbol", drop = FALSE]

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

  # x/y/symbol interface
  x_idx <- if ("x" %in% nm) which(nm == "x")[1] else 1L
  remaining <- setdiff(seq_along(dots), x_idx)
  if (length(remaining) == 0) {
    stop("`y` and `symbol` must be provided when using `x`/`y`/`symbol` inputs.", call. = FALSE)
  }
  
  y_idx <- if ("y" %in% nm) {
    which(nm == "y")[1]
  } else {
    remaining[1]
  }
  
  remaining <- setdiff(remaining, y_idx)
  if (length(remaining) == 0) {
    stop("`symbol` must be provided when using `x`/`y`/`symbol` inputs.", call. = FALSE)
  }
  
  symbol_idx <- if ("symbol" %in% nm) {
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
