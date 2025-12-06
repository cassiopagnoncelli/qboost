#' Feature importance for an mqbm model
#'
#' Aggregates feature importance across all group-specific qbm models.
#' Returns the mean and standard deviation of information gain for each feature,
#' ranked by mean gain.
#'
#' @param object A fitted mqbm model object returned by \code{\link{mqbm}}.
#' @param ... Additional arguments passed to \code{\link{importance.qbm}}.
#'
#' @return A tibble with columns:
#'   \item{feature}{Feature name}
#'   \item{gain}{Average information gain across all group models}
#'   \item{sd_gain}{Standard deviation of information gain across groups}
#'   \item{n_models}{Number of group models where this feature appears}
#'   
#' @details
#' For each feature, this function:
#' \enumerate{
#'   \item Extracts feature importance from each group-specific qbm model
#'   \item Computes the mean information gain across all groups
#'   \item Computes the standard deviation of information gain
#'   \item Counts how many group models use this feature
#' }
#' 
#' Features are ranked by mean information gain in descending order.
#' Features that don't appear in all group models will have their gain
#' treated as 0 for those models when computing statistics.
#'
#' @seealso \code{\link{mqbm}}, \code{\link{importance.qbm}}
#'
#' @examples
#' \dontrun{
#' # Fit mqbm model
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   x3 = rnorm(200),
#'   cluster = sample(c("A", "B", "C"), 200, replace = TRUE)
#' )
#' df$y <- df$x1 * 0.5 + df$x2 * 0.3 + rnorm(200)
#' 
#' fit <- mqbm(y ~ x1 + x2 + x3, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 50)
#' 
#' # Get aggregated feature importance
#' importance(fit)
#' }
#'
#' @export
importance.mqbm <- function(object, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be a mqbm model.", call. = FALSE)
  }
  
  # Extract importance from each group-specific model
  imp_list <- lapply(object$multiplexer_values, function(val) {
    imp <- importance(object$models[[val]], ...)
    if (nrow(imp) == 0) {
      return(NULL)
    }
    imp$group <- val
    imp
  })
  
  # Remove NULL entries (if any model has no importance)
  imp_list <- imp_list[!vapply(imp_list, is.null, logical(1))]
  
  if (length(imp_list) == 0) {
    return(tibble::tibble(
      feature = character(),
      gain = double(),
      sd_gain = double(),
      n_models = integer()
    ))
  }
  
  # Combine all importance tables
  imp_combined <- dplyr::bind_rows(imp_list)
  
  # Get all unique features across all models
  all_features <- unique(imp_combined$feature)
  
  # For each feature, compute mean and sd of gain across groups
  result <- lapply(all_features, function(feat) {
    # Get gains for this feature from all groups
    feat_gains <- imp_combined[imp_combined$feature == feat, ]
    
    # Create a vector with one gain per group (0 if feature not present)
    gains_by_group <- numeric(length(object$multiplexer_values))
    names(gains_by_group) <- object$multiplexer_values
    
    for (i in seq_along(object$multiplexer_values)) {
      val <- object$multiplexer_values[i]
      # Extract gain as a numeric value
      val_rows <- feat_gains[feat_gains$group == val, ]
      if (nrow(val_rows) > 0) {
        gains_by_group[val] <- as.numeric(val_rows$gain[1])
      } else {
        gains_by_group[val] <- 0
      }
    }
    
    tibble::tibble(
      feature = feat,
      gain = mean(gains_by_group, na.rm = TRUE),
      sd_gain = stats::sd(gains_by_group, na.rm = TRUE),
      n_models = sum(gains_by_group > 0)
    )
  })
  
  # Combine and sort by gain
  result_df <- dplyr::bind_rows(result)
  result_df <- dplyr::arrange(result_df, dplyr::desc(gain))
  
  result_df
}
