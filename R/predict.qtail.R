#' Predict method for qtail
#'
#' @param object A qtail object
#' @param newdata Feature matrix for prediction
#' @param type Type of prediction: "final", "stack", "grid", or "evt"
#' @param tau_override Optional tau override for target quantile
#' @param ... Additional arguments (not used)
#'
#' @return Predicted values
#' @export
predict.qtail <- function(object, newdata,
                          type = c("final", "stack", "grid", "evt"),
                          tau_override = NULL, ...) {
  
  type <- match.arg(type)
  
  # Use tau_override if provided, otherwise use object's tau_target
  tau_target_use <- if (!is.null(tau_override)) tau_override else object$tau_target
  
  # Step 1: Compute grid predictions
  preds <- list()
  for (tau in object$taus) {
    pred_vals <- .lgb_predict(object$models[[as.character(tau)]]$model, data.matrix(newdata))
    # Handle any NA/Inf values with fallback to training quantiles
    if (any(!is.finite(pred_vals))) {
      fallback <- stats::quantile(object$y, probs = tau, na.rm = TRUE)
      pred_vals[!is.finite(pred_vals)] <- fallback
    }
    preds[[as.character(tau)]] <- pred_vals
  }
  preds_mat <- do.call(cbind, preds)
  colnames(preds_mat) <- names(preds)
  
  preds_mat <- apply_pava_monotonicity(preds_mat, object$taus)
  
  # Final check for any remaining NA/Inf values after monotonicity
  if (any(!is.finite(preds_mat))) {
    for (j in seq_len(ncol(preds_mat))) {
      if (any(!is.finite(preds_mat[, j]))) {
        fallback <- stats::quantile(object$y, probs = object$taus[j], na.rm = TRUE)
        preds_mat[!is.finite(preds_mat[, j]), j] <- fallback
      }
    }
  }
  
  if (type == "grid") {
    return(preds_mat)
  }
  
  # Step 2: Compute stacked prediction
  intercept <- object$stack$coef[1]
  coefs <- object$stack$coef[-1]
  Z <- preds_mat
  
  # Apply stacking scale/center used during fitting
  if (!is.null(object$stack$center) && !is.null(object$stack$scale)) {
    sc_center <- object$stack$center
    sc_scale <- object$stack$scale
    sc_scale[sc_scale == 0] <- 1
    Z <- sweep(Z, 2, sc_center, "-")
    Z <- sweep(Z, 2, sc_scale, "/")
  }
  
  q_stack <- intercept + as.numeric(Z %*% coefs)
  
  if (type == "stack") {
    return(q_stack)
  }
  
  # Step 3: EVT extension computation
  tau0 <- object$threshold_tau
  tauT <- tau_target_use
  xi <- object$evt$xi
  beta <- object$evt$beta
  
  if (object$tail == "upper") {
    p <- (1 - tauT) / (1 - tau0)
    EVT_ext_scalar <- (beta / xi) * (p^(-xi) - 1)
    EVT_ext <- rep(EVT_ext_scalar, length(q_stack))
    q_final <- q_stack + EVT_ext
  } else {
    p <- tauT / tau0
    EVT_ext_scalar <- (beta / xi) * (p^(-xi) - 1)
    EVT_ext <- rep(EVT_ext_scalar, length(q_stack))
    q_final <- q_stack - EVT_ext
  }
  
  if (type == "evt") {
    return(EVT_ext)
  }
  
  # type == "final"
  return(q_final)
}
