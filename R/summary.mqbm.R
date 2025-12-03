#' Aggregate metrics across group-specific models
#'
#' @param mqbm_object A fitted mqbm model
#' @return List with aggregated metrics for training and validation
#' @keywords internal
.aggregate_symbol_metrics <- function(mqbm_object) {
  # Get fitted values and residuals for all symbols
  # Use type="surface" for raw quantile values in statistics
  fitted_vals <- fitted(mqbm_object, type = "surface")
  y_all <- mqbm_object$training$y
  
  # Check if any model has validation metrics
  has_validation <- any(sapply(mqbm_object$models, function(m) !is.null(m$validation)))
  
  # Compute overall training metrics (excluding NA values from validation set)
  valid_idx <- !is.na(fitted_vals)
  pinball <- pinball_loss_mean(y_all[valid_idx], fitted_vals[valid_idx], mqbm_object$tau)
  mae_val <- mae(y_all[valid_idx], fitted_vals[valid_idx])
  pseudo_r2 <- quantile_pseudo_r2(y_all[valid_idx], fitted_vals[valid_idx], mqbm_object$tau)
  
  # Get per-symbol training metrics
  symbol_metrics <- lapply(mqbm_object$symbols, function(sym) {
    model <- mqbm_object$models[[sym]]
    
    list(
      n = mqbm_object$symbol_info[[sym]]$n,
      n_train = if (!is.null(model$data_info$n_train)) model$data_info$n_train else model$data_info$n,
      n_val = if (!is.null(model$data_info$n_val)) model$data_info$n_val else 0L,
      pinball = model$metrics$pinball_loss,
      mae = model$metrics$mae,
      pseudo_r2 = model$metrics$pseudo_r2,
      coverage = model$calibration$coverage,
      qce = model$calibration$qce
    )
  })
  names(symbol_metrics) <- mqbm_object$symbols
  
  # Aggregate validation metrics if available
  val_overall <- NULL
  val_per_symbol <- NULL
  
  if (has_validation) {
    val_per_symbol <- lapply(mqbm_object$symbols, function(sym) {
      model <- mqbm_object$models[[sym]]
      if (!is.null(model$validation)) {
        list(
          n_val = model$data_info$n_val,
          pinball = model$validation$metrics$pinball_loss,
          mae = model$validation$metrics$mae,
          pseudo_r2 = model$validation$metrics$pseudo_r2,
          coverage = model$validation$calibration$coverage,
          qce = model$validation$calibration$qce
        )
      } else {
        NULL
      }
    })
    names(val_per_symbol) <- mqbm_object$symbols
    
    # Remove NULLs (symbols without validation)
    val_per_symbol <- val_per_symbol[!sapply(val_per_symbol, is.null)]
    
    # Compute weighted aggregate validation metrics
    if (length(val_per_symbol) > 0) {
      val_n <- sum(sapply(val_per_symbol, function(x) x$n_val))
      val_pinball <- weighted.mean(
        sapply(val_per_symbol, function(x) x$pinball),
        sapply(val_per_symbol, function(x) x$n_val)
      )
      val_mae <- weighted.mean(
        sapply(val_per_symbol, function(x) x$mae),
        sapply(val_per_symbol, function(x) x$n_val)
      )
      val_pseudo_r2 <- weighted.mean(
        sapply(val_per_symbol, function(x) x$pseudo_r2),
        sapply(val_per_symbol, function(x) x$n_val)
      )
      val_coverage <- weighted.mean(
        sapply(val_per_symbol, function(x) x$coverage),
        sapply(val_per_symbol, function(x) x$n_val)
      )
      val_qce <- weighted.mean(
        sapply(val_per_symbol, function(x) x$qce),
        sapply(val_per_symbol, function(x) x$n_val)
      )
      
      val_overall <- list(
        n = val_n,
        pinball_loss = val_pinball,
        mae = val_mae,
        pseudo_r2 = val_pseudo_r2,
        coverage = val_coverage,
        qce = val_qce
      )
    }
  }
  
  list(
    overall = list(
      pinball_loss = pinball,
      mae = mae_val,
      pseudo_r2 = pseudo_r2
    ),
    per_symbol = symbol_metrics,
    validation_overall = val_overall,
    validation_per_symbol = val_per_symbol
  )
}

#' Aggregate calibration metrics across symbols
#'
#' @param mqbm_object A fitted mqbm model
#' @return List with aggregated calibration metrics
#' @keywords internal
.aggregate_calibration <- function(mqbm_object) {
  # Use type="surface" for raw quantile values in calibration metrics
  fitted_vals <- fitted(mqbm_object, type = "surface")
  y_all <- mqbm_object$training$y
  
  # Overall calibration
  coverage <- mean(y_all <= fitted_vals, na.rm = TRUE)
  qce <- abs(coverage - mqbm_object$tau)
  
  # Per-symbol calibration
  symbol_calibration <- lapply(mqbm_object$symbols, function(sym) {
    idx <- mqbm_object$symbol_info[[sym]]$indices
    y_sym <- y_all[idx]
    fitted_sym <- fitted_vals[idx]
    
    cov <- mean(y_sym <= fitted_sym, na.rm = TRUE)
    list(
      coverage = cov,
      qce = abs(cov - mqbm_object$tau)
    )
  })
  names(symbol_calibration) <- mqbm_object$symbols
  
  # Calibration heterogeneity (variance in coverage across symbols)
  coverages <- vapply(symbol_calibration, function(x) x$coverage, numeric(1))
  coverage_sd <- stats::sd(coverages, na.rm = TRUE)
  
  list(
    overall_coverage = coverage,
    overall_qce = qce,
    coverage_sd = coverage_sd,
    per_symbol = symbol_calibration
  )
}

#' Create per-symbol summary table
#'
#' @param mqbm_object A fitted mqbm model
#' @param metrics_list Output from .aggregate_symbol_metrics
#' @param calibration_list Output from .aggregate_calibration
#' @return Tibble with per-symbol summary (includes validation if available)
#' @keywords internal
.compute_symbol_table <- function(mqbm_object, metrics_list, calibration_list) {
  # Check if validation metrics are available
  has_validation <- !is.null(metrics_list$validation_per_symbol) && 
                    length(metrics_list$validation_per_symbol) > 0
  
  symbol_rows <- lapply(mqbm_object$symbols, function(sym) {
    m <- metrics_list$per_symbol[[sym]]
    c <- calibration_list$per_symbol[[sym]]
    trees <- mqbm_object$models[[sym]]$best_iter
    
    base_cols <- tibble::tibble(
      symbol = sym,
      n = m$n,
      trees = trees,
      train_pinball = m$pinball,
      train_mae = m$mae,
      train_r2 = m$pseudo_r2,
      train_cov = c$coverage,
      train_qce = c$qce
    )
    
    # Add validation columns if available for this symbol
    if (has_validation && !is.null(metrics_list$validation_per_symbol[[sym]])) {
      val <- metrics_list$validation_per_symbol[[sym]]
      base_cols$val_pinball <- val$pinball
      base_cols$val_mae <- val$mae
      base_cols$val_r2 <- val$pseudo_r2
      base_cols$val_cov <- val$coverage
      base_cols$val_qce <- val$qce
    }
    
    base_cols
  })
  
  dplyr::bind_rows(symbol_rows)
}

#' Aggregate complexity metrics across symbols
#'
#' @param mqbm_object A fitted mqbm model
#' @return List with complexity metrics
#' @keywords internal
.aggregate_complexity <- function(mqbm_object) {
  # Get complexity from each symbol model
  symbol_complexity <- lapply(mqbm_object$symbols, function(sym) {
    model <- mqbm_object$models[[sym]]
    list(
      trees = model$best_iter,
      total_leaves = model$complexity$total_leaves,
      avg_leaves = model$complexity$avg_leaves_per_tree
    )
  })
  names(symbol_complexity) <- mqbm_object$symbols
  
  # Aggregate
  total_trees <- sum(vapply(symbol_complexity, function(x) x$trees, numeric(1)))
  avg_trees <- mean(vapply(symbol_complexity, function(x) x$trees, numeric(1)))
  total_leaves <- sum(vapply(symbol_complexity, function(x) x$total_leaves, numeric(1)), na.rm = TRUE)
  avg_leaves_per_tree <- mean(vapply(symbol_complexity, function(x) x$avg_leaves, numeric(1)), na.rm = TRUE)
  
  list(
    total_trees = total_trees,
    avg_trees_per_symbol = avg_trees,
    total_leaves = total_leaves,
    avg_leaves_per_tree = avg_leaves_per_tree,
    per_symbol = symbol_complexity
  )
}

#' Summarize an mqbm model
#'
#' Aggregates performance metrics across all symbol-specific models and provides
#' both overall and per-symbol diagnostics.
#'
#' @param object A fitted mqbm model object returned by \code{\link{mqbm}}.
#' @param detailed Logical; if TRUE, print extended diagnostics including
#'   per-symbol breakdowns. Default is TRUE.
#' @param top_features Integer; number of top features to show in importance table.
#'   Default is 10.
#' @param ... Additional arguments (currently unused).
#'
#' @importFrom stats fitted
#'
#' @return An object of class \code{mqbm_summary} containing:
#'   \item{tau}{Target quantile level}
#'   \item{multi}{Multiplexer column name used}
#'   \item{n_symbols}{Number of symbols/groups}
#'   \item{symbols}{Vector of symbol names}
#'   \item{data_info}{Dataset dimensions}
#'   \item{metrics}{Aggregated training metrics}
#'   \item{calibration}{Aggregated calibration metrics}
#'   \item{complexity}{Model complexity metrics}
#'   \item{symbol_table}{Per-symbol summary table}
#'   \item{importance}{Feature importance (from importance.mqbm)}
#'   \item{timings}{Training time information}
#'   \item{detailed}{Whether detailed output was requested}
#'   \item{top_features}{Number of features to display}
#'
#' @seealso \code{\link{mqbm}}, \code{\link{summary.qbm}}, \code{\link{importance.mqbm}}
#'
#' @examples
#' \dontrun{
#' # Fit mqbm model
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200),
#'   symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
#' )
#' df$y <- df$x1 * 0.5 + rnorm(200)
#' 
#' fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50)
#' 
#' # Get summary
#' summary(fit)
#' 
#' # Compact summary
#' summary(fit, detailed = FALSE)
#' }
#'
#' @export
#' @method summary mqbm
summary.mqbm <- function(object, detailed = TRUE, top_features = 10, ...) {
  if (!inherits(object, "mqbm")) {
    stop("`object` must be an mqbm model.", call. = FALSE)
  }
  
  # Aggregate metrics across symbols
  metrics <- .aggregate_symbol_metrics(object)
  calibration <- .aggregate_calibration(object)
  complexity <- .aggregate_complexity(object)
  
  # Create per-symbol summary table
  symbol_table <- .compute_symbol_table(object, metrics, calibration)
  
  # Get feature importance
  imp <- importance(object)
  
  # Build summary object
  out <- list(
    tau = object$tau,
    multi = if (!is.null(object$multi)) object$multi else "symbol",
    n_symbols = length(object$symbols),
    symbols = object$symbols,
    data_info = object$data_info,
    metrics = metrics,
    calibration = calibration,
    complexity = complexity,
    symbol_table = symbol_table,
    importance = imp,
    timings = object$timings,
    detailed = detailed,
    top_features = top_features
  )
  
  class(out) <- "mqbm_summary"
  out
}

#' Print method for mqbm_summary
#'
#' @param x An mqbm_summary object
#' @param ... Additional arguments (unused)
#'
#' @return The input object, invisibly
#' @export
print.mqbm_summary <- function(x, ...) {
  if (!inherits(x, "mqbm_summary")) {
    stop("`x` must be an mqbm_summary object.", call. = FALSE)
  }
  
  if (isTRUE(x$detailed)) {
    # Detailed output
    cat("Multi Quantile Gradient Boosting Model\n")
    cat(" Data:             ", x$data_info$n, " rows, ", x$data_info$p, " cols\n", sep = "")
    cat(" Multi:            ", x$multi, "\n", sep = "")
    cat(" Symbols:          ", x$n_symbols, " (", paste(x$symbols, collapse = ", "), ")\n", sep = "")
    cat(" Tau:              ", format(x$tau, digits = 3), "\n", sep = "")
    cat(" Elapsed (s):      ", format(x$timings$elapsed, digits = 4), "\n\n", sep = "")
    
    cat("Aggregate Training Metrics\n")
    cat(" Pinball loss:     ", format(x$metrics$overall$pinball_loss, digits = 4), "\n", sep = "")
    cat(" MAE:              ", format(x$metrics$overall$mae, digits = 4), "\n", sep = "")
    cat(" Pseudo-R2:        ", format(x$metrics$overall$pseudo_r2, digits = 4), "\n", sep = "")
    
    if (!is.null(x$metrics$validation_overall)) {
      cat("\nAggregate Validation Metrics")
      cat(" (n=", x$metrics$validation_overall$n, ")", sep = "")
      cat("\n")
      cat(" Pinball loss:     ", format(x$metrics$validation_overall$pinball_loss, digits = 4), "\n", sep = "")
      cat(" MAE:              ", format(x$metrics$validation_overall$mae, digits = 4), "\n", sep = "")
      cat(" Pseudo-R2:        ", format(x$metrics$validation_overall$pseudo_r2, digits = 4), "\n", sep = "")
      cat(" Coverage:         ", format(x$metrics$validation_overall$coverage, digits = 4), "\n", sep = "")
      cat(" QCE:              ", format(x$metrics$validation_overall$qce, digits = 4), "\n", sep = "")
    }
    cat("\n")
    
    cat("Aggregate Calibration\n")
    cat(" Coverage:         ", format(x$calibration$overall_coverage, digits = 4), 
        " (target ", format(x$tau, digits = 3), ")\n", sep = "")
    cat(" QCE:              ", format(x$calibration$overall_qce, digits = 4), "\n", sep = "")
    cat(" Coverage SD:      ", format(x$calibration$coverage_sd, digits = 4), 
        " (heterogeneity across symbols)\n\n", sep = "")
    
    cat("Model Complexity\n")
    cat(" Total trees:      ", x$complexity$total_trees, "\n", sep = "")
    cat(" Avg trees/symbol: ", format(x$complexity$avg_trees_per_symbol, digits = 1), "\n", sep = "")
    cat(" Total leaves:     ", format(x$complexity$total_leaves, big.mark = ","), "\n", sep = "")
    cat(" Avg leaves/tree:  ", format(x$complexity$avg_leaves_per_tree, digits = 3), "\n\n", sep = "")
    
    cat("Per-Symbol Summary\n")
    print(x$symbol_table, n = Inf)
    cat("\n")
    
    if (!is.null(x$importance) && nrow(x$importance) > 0) {
      cat("Feature Importance (Top ", x$top_features, ")\n", sep = "")
      top_imp <- utils::head(x$importance, x$top_features)
      print(top_imp, n = Inf)
    } else {
      cat("No feature importance available.\n")
    }
    
  } else {
    # Compact output
    cat("Multi Quantile Gradient Boosting Model\n")
    cat(" Symbols:          ", x$n_symbols, " (", x$multi, ")\n", sep = "")
    cat(" Tau:              ", format(x$tau, digits = 3), "\n", sep = "")
    cat(" Pinball loss:     ", format(x$metrics$overall$pinball_loss, digits = 4), "\n", sep = "")
    cat(" MAE:              ", format(x$metrics$overall$mae, digits = 4), "\n", sep = "")
    cat(" Pseudo-R2:        ", format(x$metrics$overall$pseudo_r2, digits = 4), "\n", sep = "")
    cat(" Coverage:         ", format(x$calibration$overall_coverage, digits = 4), 
        " | QCE: ", format(x$calibration$overall_qce, digits = 4), "\n", sep = "")
    cat("\n")
    
    cat("Per-Symbol Summary\n")
    print(x$symbol_table, n = Inf)
    cat("\n")
    
    if (!is.null(x$importance) && nrow(x$importance) > 0) {
      cat("Top Features:\n")
      top_imp <- utils::head(x$importance, min(5, x$top_features))
      print(top_imp[, c("feature", "gain", "sd_gain")], n = Inf)
    }
    
    cat("\n(Use detailed = TRUE for full report)\n")
  }
  
  invisible(x)
}
