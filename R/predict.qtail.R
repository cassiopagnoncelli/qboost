#' Predict extreme quantiles from a qtail model
#'
#' Generates predictions at extreme quantile levels using a multi-stage approach:
#' (1) predict from individual \code{\link{qbm}} models at each tau, (2) stack
#' predictions using elastic net weights, (3) extend to extreme quantiles via
#' Generalized Pareto Distribution (GPD) from Extreme Value Theory. Supports
#' multiple output types for diagnosis and analysis.
#'
#' @param object A fitted qtail model object returned by \code{\link{qtail}}.
#' @param newdata A data.frame or matrix of predictor variables with the same
#'   structure as training data. For models fit with a formula, provide a
#'   data.frame with predictor columns (factor levels handled automatically).
#' @param type Character string specifying prediction type:
#'   \itemize{
#'     \item \code{"final"} (default): Complete prediction including EVT extension
#'       to target extreme quantile
#'     \item \code{"stack"}: Stacked prediction from elastic net (without EVT)
#'     \item \code{"grid"}: Matrix of predictions from individual quantile models
#'       (one column per tau)
#'     \item \code{"evt"}: EVT extension component only (GPD-based extrapolation)
#'   }
#' @param tau_override Optional numeric value to override the target quantile
#'   level for prediction. If \code{NULL}, uses \code{object$tau_target}.
#'   Useful for predicting at different extreme quantiles than the training target.
#' @param ... Additional arguments (currently unused).
#'
#' @return Depends on \code{type}:
#'   \itemize{
#'     \item \code{"final"}: Numeric vector of extreme quantile predictions
#'     \item \code{"stack"}: Numeric vector of stacked predictions (pre-EVT)
#'     \item \code{"grid"}: Matrix with one column per tau in \code{object$taus}
#'     \item \code{"evt"}: Numeric vector of EVT extension values
#'   }
#'
#' @details
#' The prediction process consists of three stages:
#' \enumerate{
#'   \item \strong{Grid predictions}: Generate predictions from each quantile-specific
#'     \code{\link{qbm}} model and apply PAVA monotonicity correction
#'   \item \strong{Stacking}: Combine grid predictions using elastic net weights
#'     learned during training
#'   \item \strong{EVT extension}: Extrapolate from threshold quantile to target
#'     extreme quantile using GPD parameters (xi = shape, beta = scale)
#' }
#'
#' The GPD extension formula:
#' \itemize{
#'   \item Upper tail: \code{q_final = q_stack + (beta/xi) * (p^(-xi) - 1)}
#'     where \code{p = (1-tau_target)/(1-threshold_tau)}
#'   \item Lower tail: \code{q_final = q_stack - (beta/xi) * (p^(-xi) - 1)}
#'     where \code{p = tau_target/threshold_tau}
#' }
#'
#' Setting \code{type = "stack"} or \code{type = "grid"} is useful for
#' diagnosing model behavior or when EVT extrapolation is not desired.
#'
#' @seealso \code{\link{qtail}}, \code{\link{fitted.qtail}}, \code{\link{residuals.qtail}}
#'
#' @examples
#' \dontrun{
#' # Train qtail model
#' df <- data.frame(x1 = rnorm(200), x2 = rnorm(200))
#' df$y <- df$x1 * 2 + rt(200, df = 3) * 2
#' fit <- qtail(y ~ x1 + x2, data = df, tail = "upper",
#'              params = list(nrounds = 100))
#'
#' # Predict at default target quantile
#' newdata <- data.frame(x1 = c(-1, 0, 1), x2 = c(0, 0, 0))
#' predict(fit, newdata)  # type = "final" by default
#'
#' # Different prediction types
#' predict(fit, newdata, type = "stack")  # Stacked only
#' predict(fit, newdata, type = "grid")   # All quantile models
#' predict(fit, newdata, type = "evt")    # EVT component only
#'
#' # Override target quantile
#' predict(fit, newdata, tau_override = 0.9999)  # Even more extreme
#'
#' # Decompose predictions
#' stack_pred <- predict(fit, newdata, type = "stack")
#' evt_ext <- predict(fit, newdata, type = "evt")
#' final_pred <- predict(fit, newdata, type = "final")
#' # Relationship: final ≈ stack + evt (for upper tail)
#' }
#'
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
