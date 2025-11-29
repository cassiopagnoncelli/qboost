#' Summary for qevt model
#'
#' @param object A qevt object
#' @param detailed Logical; if TRUE, print extended diagnostics (default TRUE)
#' @param newdata Optional feature matrix for out-of-sample evaluation
#' @param y Optional outcomes aligned with `newdata`
#' @param top_features Integer; number of top features to show in importance table (default 10)
#' @param ... unused
#'
#' @return An object of class qevt_summary (invisibly)
#' @export
summary.qevt <- function(object, detailed = TRUE, newdata = NULL, y = NULL, 
                         top_features = 10, ...) {
  if (!inherits(object, "qevt")) {
    stop("`object` must be a qevt model.", call. = FALSE)
  }
  
  # Use training data if newdata not provided
  data_use <- newdata
  y_use <- y
  label <- "training"
  
  if (is.null(data_use) && !is.null(object$train_x)) {
    data_use <- object$train_x
    if (is.null(y_use) && !is.null(object$train_y)) {
      y_use <- object$train_y
    }
  } else if (!is.null(data_use)) {
    label <- "test"
  }
  
  # Get predictions
  if (!is.null(data_use)) {
    preds <- predict(object, data_use)
    
    # Compute metrics if y available
    metrics <- NULL
    if (!is.null(y_use)) {
      y_use <- as.numeric(y_use)
      
      # Coverage at each tau
      coverage <- sapply(seq_along(preds$taus), function(i) {
        mean(y_use <= preds$monotone[, i], na.rm = TRUE)
      })
      names(coverage) <- as.character(preds$taus)
      
      # Pseudo R^2 at key quantiles
      pseudo_r2 <- sapply(seq_along(preds$taus), function(i) {
        tau <- preds$taus[i]
        y_pred <- preds$monotone[, i]
        rho <- function(u, tau) u * (tau - (u < 0))
        null_pred <- stats::quantile(y_use, tau)
        loss_model <- mean(rho(y_use - y_pred, tau))
        loss_null <- mean(rho(y_use - null_pred, tau))
        1 - loss_model / loss_null
      })
      names(pseudo_r2) <- as.character(preds$taus)
      
      # Kendall correlation at extreme quantiles
      kendall_metrics <- list()
      for (tau_check in c(0.99, 0.999, object$tau_target)) {
        if (tau_check %in% preds$taus) {
          tau_idx <- which(preds$taus == tau_check)
          q_pred <- preds$monotone[, tau_idx]
          # Focus on extreme predictions
          extreme_idx <- q_pred > stats::quantile(q_pred, 0.99, na.rm = TRUE)
          if (sum(extreme_idx) > 5) {
            kendall <- suppressWarnings(
              stats::cor(y_use[extreme_idx], q_pred[extreme_idx],
                        method = "kendall", use = "pairwise.complete.obs")
            )
            kendall_metrics[[as.character(tau_check)]] <- kendall
          }
        }
      }
      
      # MAE at median
      median_idx <- which.min(abs(preds$taus - 0.5))
      mae_median <- mean(abs(y_use - preds$monotone[, median_idx]))
      
      metrics <- list(
        coverage = coverage,
        pseudo_r2 = pseudo_r2,
        kendall = kendall_metrics,
        mae_median = mae_median,
        n = length(y_use)
      )
    }
  } else {
    preds <- NULL
    metrics <- NULL
  }
  
  # Feature importance from sub-quantile models
  importance_list <- list()
  for (i in seq_along(object$sub_models)) {
    tau <- names(object$sub_models)[i]
    imp <- lightgbm::lgb.importance(object$sub_models[[i]])
    if (!is.null(imp) && nrow(imp) > 0) {
      imp$tau <- tau
      importance_list[[tau]] <- imp
    }
  }
  
  # Aggregate importance across models
  importance_agg <- NULL
  if (length(importance_list) > 0) {
    imp_combined <- do.call(rbind, importance_list)
    importance_agg <- stats::aggregate(
      Gain ~ Feature,
      data = imp_combined,
      FUN = mean
    )
    importance_agg <- importance_agg[order(-importance_agg$Gain), ]
    names(importance_agg) <- c("feature", "gain")
    importance_agg$share_gain <- importance_agg$gain / sum(importance_agg$gain)
  }
  
  # Build summary object
  out <- list(
    taus = object$taus,
    tau0 = object$tau0,
    tau_target = object$tau_target,
    taus_full = object$taus_full,
    u = object$u,
    n = if (!is.null(object$train_y)) length(object$train_y) else NA,
    p = if (!is.null(object$train_x)) ncol(object$train_x) else NA,
    gpd = object$gpd,
    n_sub_models = length(object$sub_models),
    tau_grid_sub = object$tau_grid_sub,
    importance = importance_agg,
    metrics = metrics,
    predictions = preds,
    label = label,
    detailed = detailed,
    top_features = top_features
  )
  
  class(out) <- "qevt_summary"
  
  if (detailed) {
    print(out)
  } else {
    print(out)
  }
  
  invisible(out)
}

#' Print method for qevt_summary
#'
#' @param x A qevt_summary object
#' @param ... Additional arguments (unused)
#'
#' @return The input object, invisibly
#' @export
print.qevt_summary <- function(x, ...) {
  if (!inherits(x, "qevt_summary")) {
    stop("`x` must be a qevt_summary object.", call. = FALSE)
  }
  
  if (isTRUE(x$detailed)) {
    cat("Extreme Quantile Model with EVT (qevt)\n")
    cat(" Data:              ", x$n, " rows, ", x$p, " cols\n", sep = "")
    cat(" EVT taus:          ", paste(format(x$taus, digits = 4), collapse = ", "), "\n", sep = "")
    cat(" Threshold (tau0):  ", format(x$tau0, digits = 4), "\n", sep = "")
    cat(" Target quantile:   ", format(x$tau_target, digits = 4), "\n", sep = "")
    cat(" Full quantile grid:", paste(format(x$taus_full, digits = 4), collapse = ", "), "\n\n", sep = "")
    
    cat("Model Architecture\n")
    cat(" Sub-quantile models:", x$n_sub_models, " LightGBM quantile regressors\n")
    cat("  - Taus:            ", paste(format(x$tau_grid_sub, digits = 4), collapse = ", "), "\n", sep = "")
    cat(" Exceedance model:   LightGBM binary classifier\n")
    cat(" Severity model:     LightGBM regression (fallback)\n")
    cat(" EVT model:          Generalized Pareto Distribution (GPD)\n")
    cat(" Monotonicity:       PAVA isotonic regression\n\n")
    
    cat("Extreme Value Theory (GPD)\n")
    cat(" Shape (xi):         ", format(x$gpd$xi, digits = 4), "\n", sep = "")
    cat(" Scale (beta):       ", format(x$gpd$beta, digits = 4), "\n", sep = "")
    cat(" Threshold (u):      ", format(x$u, digits = 6), "\n", sep = "")
    cat(" Exceedances:        ", x$gpd$n, "\n", sep = "")
    gpd_status <- if (isTRUE(x$gpd$converged)) "converged" else "not converged"
    cat(" Convergence:        ", gpd_status, "\n", sep = "")
    
    if (!isTRUE(x$gpd$converged)) {
      cat(" Warning:            GPD did not converge; using severity model fallback\n")
    } else if (x$gpd$n < 20) {
      cat(" Warning:            Few exceedances may affect GPD reliability\n")
    }
    cat("\n")
    
    # Metrics section
    if (!is.null(x$metrics)) {
      cat("Performance Metrics (", x$label, " data)\n", sep = "")
      cat(" Observations:       ", x$metrics$n, "\n", sep = "")
      cat(" MAE @ median:       ", format(x$metrics$mae_median, digits = 4), "\n\n", sep = "")
      
      cat(" Coverage:\n")
      cov_df <- data.frame(
        tau = names(x$metrics$coverage),
        coverage = as.numeric(x$metrics$coverage),
        nominal = as.numeric(names(x$metrics$coverage)),
        stringsAsFactors = FALSE
      )
      cov_df$deviation <- abs(cov_df$coverage - cov_df$nominal)
      key_taus <- c("0.95", "0.975", "0.99", "0.995", as.character(x$tau0), as.character(x$tau_target))
      cov_display <- cov_df[cov_df$tau %in% key_taus, ]
      if (nrow(cov_display) > 0) {
        for (i in seq_len(nrow(cov_display))) {
          cat("  ", format(cov_display$tau[i], width = 6), ": ",
              format(cov_display$coverage[i], digits = 4, width = 7),
              " (dev: ", format(cov_display$deviation[i], digits = 4), ")\n", sep = "")
        }
      }
      cat("\n")
      
      cat(" Pseudo R^2:\n")
      r2_df <- data.frame(
        tau = names(x$metrics$pseudo_r2),
        r2 = as.numeric(x$metrics$pseudo_r2),
        stringsAsFactors = FALSE
      )
      r2_display <- r2_df[r2_df$tau %in% key_taus, ]
      if (nrow(r2_display) > 0) {
        for (i in seq_len(nrow(r2_display))) {
          cat("  ", format(r2_display$tau[i], width = 6), ": ",
              format(r2_display$r2[i], digits = 4), "\n", sep = "")
        }
      }
      cat("\n")
      
      if (length(x$metrics$kendall) > 0) {
        cat(" Kendall correlation (at extreme predictions):\n")
        for (tau_name in names(x$metrics$kendall)) {
          cat("  ", format(tau_name, width = 6), ": ",
              format(x$metrics$kendall[[tau_name]], digits = 4), "\n", sep = "")
        }
        cat("\n")
      }
    }
    
    # Feature importance
    if (!is.null(x$importance) && nrow(x$importance) > 0) {
      cat("Feature Importance (averaged across sub-quantile models)\n")
      top <- utils::head(x$importance, min(x$top_features, nrow(x$importance)))
      top$share_pct <- sprintf("%.1f%%", top$share_gain * 100)
      cat(" Top ", nrow(top), " features:\n", sep = "")
      for (i in seq_len(nrow(top))) {
        cat("  ", format(i, width = 2), ". ",
            format(top$feature[i], width = 20), " ",
            format(top$gain[i], digits = 4, width = 10), " ",
            "(", top$share_pct[i], ")\n", sep = "")
      }
      cat("\n")
    }
    
  } else {
    # Compact summary
    cat("Extreme Quantile Model (qevt)\n")
    cat(" EVT taus:          ", paste(format(x$taus, digits = 4), collapse = ", "), "\n", sep = "")
    cat(" Target:            ", format(x$tau_target, digits = 4), "\n", sep = "")
    cat(" Data:              ", x$n, " rows, ", x$p, " cols\n", sep = "")
    cat(" Models:            ", x$n_sub_models, " sub-quantile + exceedance + EVT\n", sep = "")
    
    gpd_status <- if (isTRUE(x$gpd$converged)) "OK" else "fallback"
    cat(" GPD:               xi=", format(x$gpd$xi, digits = 3),
        ", beta=", format(x$gpd$beta, digits = 3),
        " (", gpd_status, ", n=", x$gpd$n, ")\n", sep = "")
    
    if (!is.null(x$metrics)) {
      cov_target <- x$metrics$coverage[as.character(x$tau_target)]
      dev_target <- abs(cov_target - x$tau_target)
      cat(" Coverage@target:   ", format(cov_target, digits = 4),
          " (dev: ", format(dev_target, digits = 4), ")\n", sep = "")
      
      r2_median_idx <- which.min(abs(as.numeric(names(x$metrics$pseudo_r2)) - 0.5))
      r2_median <- x$metrics$pseudo_r2[r2_median_idx]
      cat(" Pseudo R^2@median: ", format(r2_median, digits = 4), "\n", sep = "")
      
      if (length(x$metrics$kendall) > 0) {
        kendall_vals <- unlist(x$metrics$kendall)
        cat(" Kendall (extremes):", paste(format(kendall_vals, digits = 3), collapse = ", "), "\n")
      }
    }
    
    if (!is.null(x$importance) && nrow(x$importance) > 0) {
      top3 <- utils::head(x$importance, 3)
      cat(" Top features:      ", paste(top3$feature, collapse = ", "), "\n", sep = "")
    }
    
    cat("\n(detailed = TRUE for full report)\n")
  }
  
  invisible(x)
}
