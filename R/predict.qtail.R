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
    preds[[as.character(tau)]] <- predict(object$models[[as.character(tau)]], newdata)
  }
  
  if (type == "grid") {
    preds_mat <- do.call(cbind, preds)
    colnames(preds_mat) <- names(preds)
    return(preds_mat)
  }
  
  # Step 2: Compute stacked prediction
  intercept <- object$stack$coef[1]
  coefs <- object$stack$coef[-1]
  
  q_stack <- intercept
  for (j in seq_along(object$taus)) {
    tau <- object$taus[j]
    q_stack <- q_stack + coefs[j] * preds[[as.character(tau)]]
  }
  
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
