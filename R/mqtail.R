#' Fit symbol-based (multi-group) extreme tail quantile models
#'
#' Trains separate \code{\link{qtail}} extreme quantile models for each symbol
#' (group/label) in the data. This symbol-based multiplexer enables heterogeneous
#' tail modeling where different subgroups can have distinct tail behavior.
#'
#' @param ... Either a formula with \code{data} argument (where \code{data} must
#'   contain a column specified by \code{multi} identifying groups), or an
#'   \code{x}/\code{y}/\code{multi} triplet for matrix input.
#' @param multi Character string specifying the column name in \code{data} (for formula interface)
#'   or the name of the grouping variable to use as a multiplexer. Default is \code{"symbol"}.
#' @param taus Numeric vector of quantile levels to fit with \code{\link{qbm}}.
#'   If \code{NULL}, defaults based on \code{tail} direction.
#' @param tail Character string specifying tail direction: \code{"upper"} or \code{"lower"}.
#'   Default is \code{"upper"}.
#' @param threshold_tau Numeric quantile level defining the EVT threshold.
#'   If \code{NULL}, defaults to 0.99 for upper tail or 0.01 for lower tail.
#' @param params Named list of parameters passed to \code{\link{qtail}} for each symbol.
#' @param verbose Logical; if \code{TRUE}, prints progress messages. Default is \code{FALSE}.
#' @param train_idx Optional integer vector of training indices (referring to full dataset rows).
#' @param val_idx Optional integer vector of validation indices (referring to full dataset rows).
#' @param folds Optional list of integer vectors specifying custom fold structure for CV.
#'
#' @return An object of class \code{mqtail} containing:
#'   \item{models}{Named list of fitted \code{\link{qtail}} objects, one per symbol}
#'   \item{symbols}{Character vector of unique symbol identifiers}
#'   \item{symbol_info}{List with sample size and indices for each symbol}
#'   \item{ecdf_funs}{Named list of ECDF functions, one per symbol}
#'   \item{taus}{Vector of fitted quantile levels}
#'   \item{tau_target}{Target extreme quantile}
#'   \item{threshold_tau}{EVT threshold quantile level}
#'   \item{tail}{Tail direction}
#'   \item{multi}{Multiplexer column name}
#'   \item{preprocess}{Preprocessing information}
#'   \item{timings}{Training time information}
#'   \item{data_info}{Dataset dimensions}
#'   \item{training}{Training data (y and symbol vectors)}
#'
#' @seealso \code{\link{qtail}}, \code{\link{predict.mqtail}}, \code{\link{fitted.mqtail}}
#'
#' @export
mqtail <- function(...,
                   multi = "symbol",
                   taus = NULL,
                   tail = c("upper", "lower"),
                   threshold_tau = NULL,
                   params = list(),
                   verbose = FALSE,
                   train_idx = NULL,
                   val_idx = NULL,
                   folds = NULL) {
  start_time <- Sys.time()
  tail <- match.arg(tail)

  dots <- list(...)
  parsed <- .parse_mqtail_inputs(dots, multi = multi)

  x <- parsed$x
  y <- as.numeric(parsed$y)
  symbol <- as.character(parsed$symbol)

  if (nrow(x) != length(y) || nrow(x) != length(symbol)) {
    stop("`x`, `y`, and `symbol` must have compatible dimensions.", call. = FALSE)
  }

  # Get unique symbols
  unique_symbols <- unique(symbol)
  if (length(unique_symbols) == 0) {
    stop("No symbols found in the data.", call. = FALSE)
  }

  if (verbose) {
    message(sprintf("Training mqtail for %d symbols...", length(unique_symbols)))
  }

  # Train one qtail per symbol
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

    # Subset and remap train/val indices for this symbol
    symbol_indices <- .subset_indices_for_symbol(
      global_idx = idx,
      train_idx = train_idx,
      val_idx = val_idx,
      folds = folds
    )

    if (verbose) {
      message(sprintf("  Symbol %s (n=%d)...", sym, length(idx)))
    }

    # Train qtail for this symbol
    models[[sym]] <- qtail(
      x = x_sym,
      y = y_sym,
      taus = taus,
      tail = tail,
      threshold_tau = threshold_tau,
      params = params,
      verbose = FALSE  # Suppress qtail's own verbosity
    )
  }

  end_time <- Sys.time()

  out <- list(
    models = models,
    symbols = unique_symbols,
    symbol_info = symbol_info,
    ecdf_funs = ecdf_funs,
    taus = taus %||% models[[unique_symbols[1]]]$taus,
    tau_target = models[[unique_symbols[1]]]$tau_target,
    threshold_tau = threshold_tau %||% models[[unique_symbols[1]]]$threshold_tau,
    tail = tail,
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

  class(out) <- "mqtail"
  out
}

.parse_mqtail_inputs <- function(dots, multi = "symbol") {
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

    return(list(
      x = mm,
      y = y,
      symbol = symbol,
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

  preprocess <- list(
    type = "xy",
    feature_names = colnames(x)
  )

  list(
    x = x,
    y = y,
    symbol = symbol,
    preprocess = preprocess
  )
}
