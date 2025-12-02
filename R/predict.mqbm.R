#' Predict from a `mqbm` model
#'
#' @param object A fitted `mqbm` object.
#' @param newdata New data.frame or matrix of predictors. For models fit with a
#'   formula, a data.frame with the same predictor columns used at training
#'   time is expected. Must contain a `symbol` column OR provide `symbol` as a
#'   separate argument.
#' @param symbol Optional vector of symbols for prediction. If not provided,
#'   will look for a `symbol` column in `newdata`.
#' @param ... Additional arguments passed to `stats::predict()`.
#'
#' @return Numeric vector of predicted quantiles.
#' @export
#' @method predict mqbm
predict.mqbm <- function(object, newdata, symbol = NULL, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  if (missing(newdata)) {
    stop("`newdata` is required for prediction.", call. = FALSE)
  }

  # Extract symbol from newdata or use provided symbol argument
  if (is.null(symbol)) {
    if (is.data.frame(newdata) && "symbol" %in% names(newdata)) {
      symbol <- as.character(newdata$symbol)
      # Remove symbol from newdata for prediction
      newdata_for_pred <- newdata[, names(newdata) != "symbol", drop = FALSE]
    } else {
      stop("`symbol` must be provided either as a column in `newdata` or as a separate argument.", call. = FALSE)
    }
  } else {
    symbol <- as.character(symbol)
    newdata_for_pred <- newdata
  }

  # Validate symbol length
  n_rows <- if (is.data.frame(newdata_for_pred) || is.matrix(newdata_for_pred)) {
    nrow(newdata_for_pred)
  } else {
    length(newdata_for_pred)
  }
  
  if (length(symbol) != n_rows) {
    stop("`symbol` length must match the number of rows in `newdata`.", call. = FALSE)
  }

  # Check for unknown symbols
  unknown_symbols <- setdiff(unique(symbol), object$symbols)
  if (length(unknown_symbols) > 0) {
    stop("Unknown symbols in prediction data: ", paste(unknown_symbols, collapse = ", "), 
         "\nAvailable symbols: ", paste(object$symbols, collapse = ", "), call. = FALSE)
  }

  # Process newdata according to preprocess type
  if (!is.null(object$preprocess) && identical(object$preprocess$type, "formula")) {
    terms_obj <- object$preprocess$terms
    if (is.null(terms_obj)) {
      stop("Terms information missing; cannot build design matrix for prediction.", call. = FALSE)
    }
    terms_noy <- stats::delete.response(terms_obj)
    nd <- newdata_for_pred
    if (!is.data.frame(nd)) {
      nd <- as.data.frame(nd)
    }
    mf <- stats::model.frame(
      terms_noy,
      nd,
      na.action = stats::na.pass,
      xlev = object$preprocess$xlevels
    )
    newdata_matrix <- stats::model.matrix(
      terms_noy,
      mf,
      contrasts.arg = object$preprocess$contrasts
    )
    # Remove intercept column if present (consistent with training)
    if ("(Intercept)" %in% colnames(newdata_matrix)) {
      newdata_matrix <- newdata_matrix[, colnames(newdata_matrix) != "(Intercept)", drop = FALSE]
    }
  } else if (!is.matrix(newdata_for_pred)) {
    newdata_matrix <- data.matrix(newdata_for_pred)
  } else {
    newdata_matrix <- newdata_for_pred
  }

  # Ensure plain numeric matrix
  if (is.matrix(newdata_matrix)) {
    feature_names <- colnames(newdata_matrix)
    newdata_matrix <- matrix(as.numeric(newdata_matrix), 
                            nrow = nrow(newdata_matrix), 
                            ncol = ncol(newdata_matrix))
    colnames(newdata_matrix) <- feature_names
  }

  # Initialize predictions vector
  predictions <- numeric(length(symbol))
  
  # Predict for each symbol using its corresponding model
  for (sym in object$symbols) {
    idx <- which(symbol == sym)
    if (length(idx) > 0) {
      x_sym <- newdata_matrix[idx, , drop = FALSE]
      predictions[idx] <- predict(object$models[[sym]], x_sym, ...)
    }
  }

  predictions
}
