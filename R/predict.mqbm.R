#' Predict quantiles from a symbol-based mqbm model
#'
#' Routes predictions to the appropriate symbol-specific \code{\link{qbm}} model
#' based on the symbol identifier for each observation. Each raw prediction from the
#' quantile regression model is transformed through the symbol-specific empirical
#' cumulative distribution function (ECDF) to return probabilities instead of raw values.
#'
#' @param object A fitted mqbm model object returned by \code{\link{mqbm}}.
#' @param newdata A data.frame or matrix of predictor variables. Must either
#'   contain a column with the name specified by \code{object$multi} (default \code{"symbol"})
#'   identifying which model to use for each observation, or \code{multi} must be provided
#'   as a separate argument.
#' @param multi Optional character vector specifying which symbol-specific model
#'   to use for each row of \code{newdata}. If \code{NULL} (default), the
#'   function looks for a column named \code{object$multi} in \code{newdata}. 
#'   For backward compatibility, also accepts \code{symbol} as an argument name.
#'   Must contain only symbols that were present in the training data.
#' @param symbol Deprecated. Use \code{multi} instead. Kept for backward compatibility.
#' @param type Character string specifying the type of prediction. Either \code{"surface"}
#'   for raw quantile predictions (default) or \code{"quantile"} for ECDF-transformed
#'   probabilities.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of length \code{nrow(newdata)} containing predictions.
#'   If \code{type = "surface"}, returns raw quantile values from the symbol-specific
#'   models. If \code{type = "quantile"}, returns ECDF-transformed probabilities (0-1 range)
#'   representing the empirical percentile within each symbol's training distribution.
#'
#' @details
#' The prediction process:
#' \enumerate{
#'   \item Extract or validate symbol identifiers for each observation
#'   \item Verify all symbols exist in the trained models
#'   \item Route each observation to its symbol-specific \code{\link{qbm}} model
#'   \item Transform raw predictions through symbol-specific ECDF to get probabilities
#'   \item Combine predictions maintaining original row order
#' }
#'
#' Symbols in \code{newdata} must match those used during training. Unknown
#' symbols will trigger an error with a list of available symbols.
#'
#' @seealso \code{\link{mqbm}}, \code{\link{predict.qbm}}, \code{\link{fitted.mqbm}},
#'   \code{\link{residuals.mqbm}}
#'
#' @examples
#' \dontrun{
#' # Train multi-symbol model
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
#' )
#' df$y <- df$x1 * 0.5 + rnorm(200)
#' fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50)
#'
#' # Method 1: Symbol in newdata
#' newdata_with_symbol <- data.frame(
#'   x1 = c(0, 1, -1),
#'   x2 = c(0, 0, 1),
#'   symbol = c("A", "B", "A")
#' )
#' predict(fit, newdata_with_symbol)
#'
#' # Method 2: Symbol as separate argument
#' newdata_no_symbol <- data.frame(
#'   x1 = c(0, 1, -1),
#'   x2 = c(0, 0, 1)
#' )
#' symbols <- c("A", "B", "C")
#' predict(fit, newdata_no_symbol, symbol = symbols)
#'
#' # Predictions automatically use correct symbol-specific model
#' # Each row gets prediction from its designated symbol model
#' }
#'
#' @export
#' @method predict mqbm
predict.mqbm <- function(object, newdata, multi = NULL, symbol = NULL, type = c("surface", "quantile"), ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  if (missing(newdata)) {
    stop("`newdata` is required for prediction.", call. = FALSE)
  }

  # Match and validate type argument
  type <- match.arg(type)

  # Get the multi column name from the object, default to "symbol"
  multi_colname <- if (!is.null(object$multi)) object$multi else "symbol"
  
  # For backward compatibility: if symbol argument is provided, use it
  if (!is.null(symbol)) {
    multi <- symbol
  }

  # Extract multi/symbol from newdata or use provided multi argument
  if (is.null(multi)) {
    # Try to find the column in newdata
    if (is.data.frame(newdata)) {
      if (multi_colname %in% names(newdata)) {
        multi <- as.character(newdata[[multi_colname]])
        # Remove multi column from newdata for prediction
        newdata_for_pred <- newdata[, names(newdata) != multi_colname, drop = FALSE]
      } else {
        stop(
          sprintf("`%s` must be provided either as a column in `newdata` or as a separate argument.",
                  multi_colname),
          call. = FALSE
        )
      }
    } else {
      stop(
        sprintf("`%s` must be provided either as a column in `newdata` or as a separate argument.",
                multi_colname),
        call. = FALSE
      )
    }
  } else {
    multi <- as.character(multi)
    newdata_for_pred <- newdata
  }
  
  # Use 'symbol' as the internal variable name for clarity
  symbol <- multi

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
      # Get raw predictions from the qbm model
      raw_preds <- predict(object$models[[sym]], x_sym, ...)
      
      # Apply transformation based on type
      if (type == "quantile") {
        # Transform through symbol-specific ECDF to get probabilities
        predictions[idx] <- object$ecdf_funs[[sym]](raw_preds)
      } else {
        # Return raw surface predictions
        predictions[idx] <- raw_preds
      }
    }
  }

  predictions
}
