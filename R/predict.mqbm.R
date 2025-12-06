#' Predict quantiles from a multiplexed mqbm model
#'
#' Routes predictions to the appropriate group-specific \code{\link{qbm}} model
#' based on the group identifier for each observation. Each raw prediction from the
#' quantile regression model is transformed through the group-specific empirical
#' cumulative distribution function (ECDF) to return probabilities instead of raw values.
#'
#' @param object A fitted mqbm model object returned by \code{\link{mqbm}}.
#' @param newdata A data.frame or matrix of predictor variables. Must either
#'   contain a column with the name specified by \code{object$multiplexer}
#'   identifying which model to use for each observation, or \code{multiplexer} must be provided
#'   as a separate argument.
#' @param multiplexer Optional character vector specifying which group-specific model
#'   to use for each row of \code{newdata}. If \code{NULL} (default), the
#'   function looks for a column named \code{object$multiplexer} in \code{newdata}.
#'   Must contain only groups that were present in the training data.
#' @param type Character string specifying the type of prediction. Either \code{"surface"}
#'   for raw quantile predictions (default) or \code{"quantile"} for ECDF-transformed
#'   probabilities.
#' @param ... Additional arguments (currently unused).
#'
#' @return A numeric vector of length \code{nrow(newdata)} containing predictions.
#'   If \code{type = "surface"}, returns raw quantile values from the group-specific
#'   models. If \code{type = "quantile"}, returns ECDF-transformed probabilities (0-1 range)
#'   representing the empirical percentile within each group's training distribution.
#'
#' @details
#' The prediction process:
#' \enumerate{
#'   \item Extract or validate group identifiers for each observation
#'   \item Verify all groups exist in the trained models
#'   \item Route each observation to its group-specific \code{\link{qbm}} model
#'   \item Transform raw predictions through group-specific ECDF to get probabilities
#'   \item Combine predictions maintaining original row order
#' }
#'
#' Groups in \code{newdata} must match those used during training. Unknown
#' groups will trigger an error with a list of available groups.
#'
#' @seealso \code{\link{mqbm}}, \code{\link{predict.qbm}}, \code{\link{fitted.mqbm}},
#'   \code{\link{residuals.mqbm}}
#'
#' @examples
#' \dontrun{
#' # Train multi-group model
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   cluster = sample(c("A", "B", "C"), 200, replace = TRUE)
#' )
#' df$y <- df$x1 * 0.5 + rnorm(200)
#' fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 50)
#'
#' # Method 1: Group in newdata
#' newdata_with_group <- data.frame(
#'   x1 = c(0, 1, -1),
#'   x2 = c(0, 0, 1),
#'   cluster = c("A", "B", "A")
#' )
#' predict(fit, newdata_with_group)
#'
#' # Method 2: Group as separate argument
#' newdata_no_group <- data.frame(
#'   x1 = c(0, 1, -1),
#'   x2 = c(0, 0, 1)
#' )
#' groups <- c("A", "B", "C")
#' predict(fit, newdata_no_group, multiplexer = groups)
#'
#' # Predictions automatically use correct group-specific model
#' # Each row gets prediction from its designated group model
#' }
#'
#' @export
#' @method predict mqbm
predict.mqbm <- function(object, newdata, multiplexer = NULL, type = c("surface", "quantile"), ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  if (missing(newdata)) {
    stop("`newdata` is required for prediction.", call. = FALSE)
  }

  # Match and validate type argument
  type <- match.arg(type)

  # Get the multiplexer column name from the object
  multiplexer_colname <- object$multiplexer

  # Extract multiplexer from newdata or use provided multiplexer argument
  if (is.null(multiplexer)) {
    # Try to find the column in newdata
    if (is.data.frame(newdata)) {
      if (multiplexer_colname %in% names(newdata)) {
        multiplexer <- as.character(newdata[[multiplexer_colname]])
        # Remove multiplexer column from newdata for prediction
        newdata_for_pred <- newdata[, names(newdata) != multiplexer_colname, drop = FALSE]
      } else {
        stop(
          sprintf(
            "`%s` must be provided either as a column in `newdata` or as a separate argument.",
            multiplexer_colname
          ),
          call. = FALSE
        )
      }
    } else {
      stop(
        sprintf(
          "`%s` must be provided either as a column in `newdata` or as a separate argument.",
          multiplexer_colname
        ),
        call. = FALSE
      )
    }
  } else {
    multiplexer <- as.character(multiplexer)
    newdata_for_pred <- newdata
  }

  # Use 'group_vals' as the internal variable name
  group_vals <- multiplexer

  # Validate group length
  n_rows <- if (is.data.frame(newdata_for_pred) || is.matrix(newdata_for_pred)) {
    nrow(newdata_for_pred)
  } else {
    length(newdata_for_pred)
  }

  if (length(group_vals) != n_rows) {
    stop("`multiplexer` length must match the number of rows in `newdata`.", call. = FALSE)
  }

  # Check for unknown groups
  unknown_groups <- setdiff(unique(group_vals), object$multiplexer_values)
  if (length(unknown_groups) > 0) {
    stop("Unknown groups in prediction data: ", paste(unknown_groups, collapse = ", "),
      "\nAvailable groups: ", paste(object$multiplexer_values, collapse = ", "),
      call. = FALSE
    )
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
      ncol = ncol(newdata_matrix)
    )
    colnames(newdata_matrix) <- feature_names
  }

  # Initialize predictions vector
  predictions <- numeric(length(group_vals))

  # Predict for each group using its corresponding model
  for (val in object$multiplexer_values) {
    idx <- which(group_vals == val)
    if (length(idx) > 0) {
      x_val <- newdata_matrix[idx, , drop = FALSE]
      # Get raw predictions from the qbm model
      raw_preds <- predict(object$models[[val]], x_val, ...)

      # Apply transformation based on type
      if (type == "quantile") {
        # Transform through group-specific ECDF to get probabilities
        predictions[idx] <- object$ecdf_funs[[val]](raw_preds)
      } else {
        # Return raw surface predictions
        predictions[idx] <- raw_preds
      }
    }
  }

  predictions
}
