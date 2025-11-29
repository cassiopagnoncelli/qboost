#' Fit extreme quantile model with EVT and PAVA monotonicity
#'
#' Provides a clean interface for extreme quantile modeling using
#' LightGBM quantile regression combined with Extreme Value Theory (EVT).
#' Accepts either a formula + data or an `x`/`y` pair.
#'
#' @param ... Either a formula and optional `data` argument or an `x`/`y` pair
#'   followed by additional arguments.
#' @param tau_target Final extreme tau (default 0.9999)
#' @param tau0 Threshold quantile for EVT (default 0.997)
#' @param tau1 Intermediate EVT quantile (default 0.9985)
#' @param tau2 Intermediate EVT quantile (default 0.999)
#' @param tau3 Intermediate EVT quantile (default 0.9993)
#' @return Object of class 'qevt'
#' @export
#'
#' @examples
#' \dontrun{
#' # Using formula interface
#' set.seed(123)
#' df <- data.frame(
#'   x1 = rnorm(200),
#'   x2 = rnorm(200)
#' )
#' df$y <- df$x1 * 2 + df$x2 * 0.5 + rt(200, df = 3) * 2
#'
#' fit <- qevt(y ~ x1 + x2, data = df)
#'
#' # Using x/y interface
#' x <- as.matrix(df[, c("x1", "x2")])
#' y <- df$y
#' fit <- qevt(x, y)
#' }
qevt <- function(
    ...,
    tau_target = 0.9999,
    tau0 = 0.997,
    tau1 = 0.9985,
    tau2 = 0.999,
    tau3 = 0.9993) {
  # Parse inputs
  parsed <- .parse_qevt_inputs(list(...))
  X <- if (!is.matrix(parsed$x)) data.matrix(parsed$x) else parsed$x
  y <- as.numeric(parsed$y)

  if (nrow(X) != length(y)) {
    stop("`x` and `y` must have compatible dimensions.", call. = FALSE)
  }

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
  taus_full <- c(tau_grid_sub, tau0, tau1, tau2, tau3, tau_target)
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
    tau0 = tau0,
    tau1 = tau1,
    tau2 = tau2,
    tau3 = tau3,
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

# Parse qevt inputs (formula or x/y) - similar to qboost/qtail
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
