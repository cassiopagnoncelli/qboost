#' Fit extreme quantile model with EVT and conditional modeling
#'
#' Advanced extreme quantile modeling combining three components: (1) exceedance
#' probability classifier determining likelihood of exceeding a high threshold,
#' (2) Generalized Pareto Distribution (GPD) via Maximum Likelihood Estimation
#' for tail behavior, (3) ensemble of sub-quantile \code{\link{qbm}} models for
#' smooth interpolation. Predictions are monotonized using PAVA. Particularly
#' suited for modeling ultra-extreme quantiles (e.g., 99.99th percentile) where
#' data are sparse.
#'
#' @param ... Either a formula with optional \code{data} argument, or an
#'   \code{x}/\code{y} pair for matrix input. Additional arguments are currently
#'   forwarded to internal fitting functions.
#' @param tau_target Numeric scalar defining the target extreme quantile level.
#'   Default is 0.98 (98.00th percentile). This is the quantile level that
#'   predictions will target.
#' @param taus Numeric vector of intermediate EVT quantile levels between the
#'   threshold and target. Default is \code{c(0.95, 0.98, 0.99, 0.993, 0.999)}.
#'   The first value (0.95) serves as the EVT threshold (tau0). Additional
#'   values provide intermediate quantiles for smooth transitions.
#'
#' @return An object of class \code{qevt} containing:
#'   \item{exceed_model}{Binary classifier (LightGBM) predicting exceedance probability}
#'   \item{gpd}{GPD fit results (xi = shape, beta = scale, convergence status)}
#'   \item{severity_model}{LightGBM regression model for exceedance magnitudes (fallback)}
#'   \item{sub_models}{Named list of \code{\link{qbm}} models at sub-threshold quantiles}
#'   \item{tau_grid_sub}{Vector of sub-threshold quantiles (default: 0.95, 0.975, 0.99, 0.995)}
#'   \item{taus}{Input EVT quantile vector}
#'   \item{tau0}{EVT threshold quantile (first element of taus)}
#'   \item{taus_evt}{Intermediate EVT quantiles}
#'   \item{tau_target}{Target extreme quantile}
#'   \item{u}{Threshold value (tau0 quantile of training data)}
#'   \item{taus_full}{Complete grid of all quantiles (sub + EVT + target)}
#'   \item{train_x}{Training feature matrix}
#'   \item{train_y}{Training response vector}
#'   \item{preprocess}{Preprocessing information (for formula interface)}
#'
#' @details
#' The qevt fitting process consists of four stages:
#' \enumerate{
#'   \item \strong{Exceedance Classifier}: Train LightGBM binary classifier to
#'     predict P(Y > u | X) where u is the tau0 quantile
#'   \item \strong{GPD Fitting}: Fit Generalized Pareto Distribution to exceedances
#'     (Y - u | Y > u) using MLE. Also fit severity model (LightGBM regression)
#'     as fallback for cases where GPD fails or is unreliable
#'   \item \strong{Sub-Quantile Models}: Train \code{\link{qbm}} models at
#'     quantiles below threshold (default: 0.95, 0.975, 0.99, 0.995) for smooth
#'     interpolation in non-extreme regions
#'   \item \strong{Assembly}: Combine all components into qevt object with
#'     full quantile grid for prediction
#' }
#'
#' Key differences from \code{\link{qtail}}:
#' \itemize{
#'   \item Uses exceedance probability model instead of stacking
#'   \item Includes severity regression as GPD fallback
#'   \item Focuses on single target quantile rather than multiple taus
#'   \item Better suited for ultra-extreme quantiles (>99.9%)
#' }
#'
#' @seealso \code{\link{predict.qevt}}, \code{\link{summary.qevt}},
#'   \code{\link{plot.qevt}}, \code{\link{fitted.qevt}},
#'   \code{\link{residuals.qevt}}, \code{\link{qtail}}, \code{\link{qbm}}
#'
#' @examples
#' \dontrun{
#' # Simulate data with heavy right tail
#' set.seed(123)
#' n <- 500
#' df <- data.frame(
#'   x1 = rnorm(n),
#'   x2 = rnorm(n)
#' )
#' # Pareto-like tail behavior
#' df$y <- exp(df$x1 * 0.5 + df$x2 * 0.3 + rnorm(n, sd = 0.5))
#'
#' # Fit qevt model for ultra-extreme quantile
#' fit <- qevt(
#'   y ~ x1 + x2,
#'   data = df,
#'   tau_target = 0.9999,  # 99.99th percentile
#'   taus = c(0.997, 0.9985, 0.999, 0.9993)
#' )
#'
#' print(fit)
#' summary(fit)
#'
#' # Predictions
#' newdata <- data.frame(x1 = c(-1, 0, 1), x2 = c(0, 0, 0))
#' predict(fit, newdata)
#'
#' # Access model components
#' fit$gpd$xi           # GPD shape parameter
#' fit$gpd$beta         # GPD scale parameter
#' fit$gpd$converged    # Convergence status
#'
#' # Matrix interface
#' X <- as.matrix(df[, c("x1", "x2")])
#' y <- df$y
#' fit2 <- qevt(X, y, tau_target = 0.9995)
#'
#' # Different target quantiles
#' fit_9999 <- qevt(y ~ x1 + x2, data = df, tau_target = 0.9999)
#' fit_99999 <- qevt(y ~ x1 + x2, data = df, tau_target = 0.99999)
#' }
#' @export
qevt <- function(
    ...,
    tau_target = 0.98,
    taus = c(0.95, 0.98, 0.99, 0.993, 0.999)) {
  # Parse inputs
  parsed <- .parse_qevt_inputs(list(...))
  X <- if (!is.matrix(parsed$x)) data.matrix(parsed$x) else parsed$x
  y <- as.numeric(parsed$y)

  if (nrow(X) != length(y)) {
    stop("`x` and `y` must have compatible dimensions.", call. = FALSE)
  }

  # Extract tau0 as the first tau (EVT threshold)
  tau0 <- taus[1]
  taus_evt <- if (length(taus) > 1) taus[-1] else numeric(0)

  total_steps <- 4
  step_idx <- 1
  t0 <- Sys.time()
  message(sprintf("[Step %d/%d] Fitting exceedance classifier (tau0=%.4f)...", step_idx, total_steps, tau0))
  tau_grid_sub <- c(0.95, 0.975, 0.99, 0.995)
  exceed_fit <- fit_exceedance_model(X, y, tau0 = tau0)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  -> done in %.2fs (ETA %.2fs)", elapsed, eta))

  step_idx <- step_idx + 1
  message(sprintf("[Step %d/%d] Fitting GPD (MLE) and severity fallback...", step_idx, total_steps))
  u <- exceed_fit$u
  tail_idx <- which(y > u)
  y_tail <- y[tail_idx]
  X_tail <- X[tail_idx, , drop = FALSE]
  z <- y_tail - u
  gpd_fit <- fit_gpd(y_tail, u)
  severity_model <- NULL
  if (length(z) > 0) {
    severity_model <- fit_severity_lgbm(X_tail, z)
  }
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  -> done in %.2fs (ETA %.2fs)", elapsed, eta))

  step_idx <- step_idx + 1
  message(sprintf("[Step %d/%d] Training sub-quantile LightGBM models...", step_idx, total_steps))
  sub_models <- fit_subquantile_models(X, y, tau_grid_sub = tau_grid_sub)
  # Build full tau grid: sub-quantiles, then EVT taus (tau0 + intermediates), then target
  # Remove tau0 from tau_grid_sub if present to avoid duplicates
  tau_grid_sub_clean <- tau_grid_sub[tau_grid_sub < tau0]
  taus_full <- c(tau_grid_sub_clean, tau0, taus_evt, tau_target)
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  eta <- elapsed / step_idx * (total_steps - step_idx)
  message(sprintf("  -> done in %.2fs (ETA %.2fs)", elapsed, eta))

  step_idx <- step_idx + 1
  message(sprintf("[Step %d/%d] Assembling model object...", step_idx, total_steps))
  model <- list(
    exceed_model = exceed_fit,
    gpd = gpd_fit,
    severity_model = severity_model,
    sub_models = sub_models,
    tau_grid_sub = tau_grid_sub,
    taus = taus,
    tau0 = tau0,
    taus_evt = taus_evt,
    tau_target = tau_target,
    u = u,
    taus_full = taus_full,
    train_x = X,
    train_y = y,
    preprocess = parsed$preprocess
  )
  class(model) <- "qevt"
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  message(sprintf("  -> done in %.2fs (complete)", elapsed))
  model
}

# Parse qevt inputs (formula or x/y) - similar to qbm/qtail
.parse_qevt_inputs <- function(dots) {
  if (length(dots) == 0) {
    stop("Provide either a formula or an `x`/`y` pair.", call. = FALSE)
  }

  nm <- names(dots)
  if (is.null(nm)) {
    nm <- rep("", length(dots))
  }
  nm[is.na(nm)] <- ""

  is_formula <- vapply(dots, inherits, logical(1), what = "formula")
  if (any(is_formula)) {
    formula_idx <- which(is_formula)[1]
    formula <- dots[[formula_idx]]

    data_idx <- integer(0)
    if ("data" %in% nm) {
      data_idx <- which(nm == "data")[1]
    } else {
      unnamed <- which(!nzchar(nm))
      unnamed <- unnamed[unnamed != formula_idx]
      if (length(unnamed) > 0) {
        data_idx <- unnamed[1]
      }
    }

    data <- if (length(data_idx) > 0) dots[[data_idx]] else NULL

    mf <- stats::model.frame(formula, data = data, na.action = stats::na.pass)
    y <- stats::model.response(mf)
    mm <- stats::model.matrix(formula, mf)
    # Remove intercept column if present (LightGBM doesn't need it)
    if ("(Intercept)" %in% colnames(mm)) {
      mm <- mm[, colnames(mm) != "(Intercept)", drop = FALSE]
    }
    terms_obj <- stats::terms(mf)

    preprocess <- list(
      type = "formula",
      terms = terms_obj,
      xlevels = stats::.getXlevels(terms_obj, mf),
      contrasts = attr(mm, "contrasts"),
      formula = formula,
      feature_names = colnames(mm)
    )

    extra_idx <- setdiff(seq_along(dots), c(formula_idx, data_idx))
    extra_args <- if (length(extra_idx) > 0) dots[extra_idx] else list()

    return(list(
      x = mm,
      y = y,
      extra_args = extra_args,
      preprocess = preprocess
    ))
  }

  x_idx <- if ("x" %in% nm) which(nm == "x")[1] else 1L
  remaining <- setdiff(seq_along(dots), x_idx)
  if (length(remaining) == 0) {
    stop("`y` must be provided when using `x`/`y` inputs.", call. = FALSE)
  }
  y_idx <- if ("y" %in% nm) which(nm == "y")[1] else remaining[1]

  x <- dots[[x_idx]]
  y <- dots[[y_idx]]

  extra_idx <- setdiff(seq_along(dots), c(x_idx, y_idx))
  extra_args <- if (length(extra_idx) > 0) dots[extra_idx] else list()

  preprocess <- list(
    type = "xy",
    feature_names = colnames(x)
  )

  list(
    x = x,
    y = y,
    extra_args = extra_args,
    preprocess = preprocess
  )
}
