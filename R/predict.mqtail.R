#' Predict from a symbol-based mqtail model
#'
#' Routes predictions to the appropriate symbol-specific \code{\link{qtail}} model
#' based on the symbol identifier for each observation.
#'
#' @param object A fitted mqtail model object returned by \code{\link{mqtail}}.
#' @param newdata A data.frame or matrix of predictor variables.
#' @param multi Optional character vector specifying which symbol-specific model
#'   to use for each row of \code{newdata}.
#' @param symbol Deprecated. Use \code{multi} instead. Kept for backward compatibility.
#' @param type Character string specifying the type of prediction. Either \code{"surface"}
#'   for raw quantile predictions (default) or \code{"quantile"} for ECDF-transformed
#'   probabilities.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of length \code{nrow(newdata)} containing predictions.
#'
#' @seealso \code{\link{mqtail}}, \code{\link{predict.qtail}}, \code{\link{fitted.mqtail}}
#'
#' @export
#' @method predict mqtail
predict.mqtail <- function(object, newdata, multi = NULL, symbol = NULL, type = c("surface", "quantile"), ...) {
  if (!inherits(object, "mqtail")) {
    stop("`object` must be a mqtail model.", call. = FALSE)
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
    if (is.data.frame(newdata)) {
      if (multi_colname %in% names(newdata)) {
        multi <- as.character(newdata[[multi_colname]])
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
    if ("(Intercept)" %in% colnames(newdata_matrix)) {
      newdata_matrix <- newdata_matrix[, colnames(newdata_matrix) != "(Intercept)", drop = FALSE]
    }
  } else if (!is.matrix(newdata_for_pred)) {
    newdata_matrix <- data.matrix(newdata_for_pred)
  } else {
    newdata_matrix <- newdata_for_pred
  }

  # Initialize predictions vector
  predictions <- numeric(length(symbol))

  # Predict for each symbol using its corresponding model
  for (sym in object$symbols) {
    idx <- which(symbol == sym)
    if (length(idx) > 0) {
      x_sym <- newdata_matrix[idx, , drop = FALSE]
      # Get raw predictions from the qtail model
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
