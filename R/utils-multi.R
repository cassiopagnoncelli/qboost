#' Subset and remap global indices for a specific group
#'
#' Takes global dataset indices and remaps them to group-specific local indices,
#' respecting group boundaries (never mixing observations between groups).
#'
#' @param global_idx Integer vector of row indices for this group in the full dataset
#' @param train_idx Optional global training indices
#' @param val_idx Optional global validation indices
#' @param folds Optional list of global fold validation indices
#' @return List with group-specific train_idx, val_idx, and folds (or NULL)
#' @keywords internal
.subset_indices_for_group <- function(global_idx, train_idx, val_idx, folds) {
  # If no custom indices specified, return NULLs (use default k-fold)
  if (is.null(train_idx) && is.null(val_idx) && is.null(folds)) {
    return(list(train_idx = NULL, val_idx = NULL, folds = NULL))
  }

  # Priority 1: Custom folds
  if (!is.null(folds)) {
    group_folds <- lapply(folds, function(fold_val_idx) {
      # Find which validation indices belong to this group
      group_val_global <- intersect(fold_val_idx, global_idx)
      if (length(group_val_global) == 0) {
        return(integer(0))
      }
      # Remap global indices to local group-specific indices
      match(group_val_global, global_idx)
    })
    # Remove empty folds (where this group has no validation observations)
    group_folds <- group_folds[sapply(group_folds, length) > 0]

    if (length(group_folds) == 0) {
      # This group has no observations in any validation fold
      # Fall back to default k-fold
      return(list(train_idx = NULL, val_idx = NULL, folds = NULL))
    }

    return(list(train_idx = NULL, val_idx = NULL, folds = group_folds))
  }

  # Priority 2: Train/val split
  if (!is.null(train_idx) && !is.null(val_idx)) {
    # Find which global train/val indices belong to this group
    group_train_global <- intersect(train_idx, global_idx)
    group_val_global <- intersect(val_idx, global_idx)

    # If this group has no observations in either train or val
    if (length(group_train_global) == 0 && length(group_val_global) == 0) {
      # Group not in either set - fall back to default k-fold on all group data
      return(list(train_idx = NULL, val_idx = NULL, folds = NULL))
    }

    # If group has validation data but no training data, this is invalid
    # Use all group data for training with k-fold CV
    if (length(group_train_global) == 0) {
      return(list(train_idx = NULL, val_idx = NULL, folds = NULL))
    }

    # If group has training data but NO validation data
    # Use ALL group data with k-fold CV
    if (length(group_val_global) == 0) {
      # This group only appears in training set globally
      # Use all group's data with k-fold CV for validation
      return(list(train_idx = NULL, val_idx = NULL, folds = NULL))
    }

    # Both train and val have data for this group - use the split
    group_train_local <- match(group_train_global, global_idx)
    group_val_local <- match(group_val_global, global_idx)

    return(list(
      train_idx = group_train_local,
      val_idx = group_val_local,
      folds = NULL
    ))
  }

  # Default: no custom indices
  return(list(train_idx = NULL, val_idx = NULL, folds = NULL))
}
