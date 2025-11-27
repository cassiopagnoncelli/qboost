testthat::test_that("qtail_qce computes calibration error", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(1111)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  qce <- qtail_qce(fit)

  testthat::expect_s3_class(qce, "data.frame")
  testthat::expect_equal(nrow(qce), length(fit$taus))
  testthat::expect_true(all(c("tau", "observed", "qce") %in% names(qce)))
  testthat::expect_true(all(qce$qce >= 0))
})

testthat::test_that("qtail_qce works with custom taus", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(1112)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  custom_taus <- c(0.95, 0.99)
  qce <- qtail_qce(fit, taus = custom_taus)

  testthat::expect_equal(nrow(qce), length(custom_taus))
  testthat::expect_equal(qce$tau, custom_taus)
})

testthat::test_that("qtail_calibration_slope computes slope", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(2222)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  # May produce warnings from glm, suppress them
  slope_data <- suppressWarnings(qtail_calibration_slope(fit))

  testthat::expect_type(slope_data, "list")
  testthat::expect_true(all(c("slope", "intercept", "tau") %in% names(slope_data)))
  testthat::expect_true(is.numeric(slope_data$slope))
  testthat::expect_true(is.numeric(slope_data$intercept))
  testthat::expect_equal(slope_data$tau, fit$tau_target)
})

testthat::test_that("qtail_calibration_slope works with custom tau", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(2223)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  custom_tau <- 0.95
  slope_data <- suppressWarnings(qtail_calibration_slope(fit, tau = custom_tau))

  testthat::expect_equal(slope_data$tau, custom_tau)
})

testthat::test_that("qtail_pit computes PIT values", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(3333)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  pit <- qtail_pit(fit)

  testthat::expect_true(is.numeric(pit))
  testthat::expect_length(pit, length(fit$y))
  testthat::expect_true(all(pit >= 0 & pit <= 1))
})

testthat::test_that("qtail_pit works for lower tail", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(3334)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "lower", params = list(nrounds = 20, nfolds = 2)))

  pit <- qtail_pit(fit)

  testthat::expect_true(is.numeric(pit))
  testthat::expect_length(pit, length(fit$y))
  testthat::expect_true(all(pit >= 0 & pit <= 1))
})

testthat::test_that("qtail_pit_plot creates plot", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(4444)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  # May produce warnings from PIT computation, just check it doesn't error
  testthat::expect_error(suppressWarnings(qtail_pit_plot(fit)), NA)
})

testthat::test_that("qtail_pit_plot accepts custom breaks", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(4445)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  # May produce warnings, just check it doesn't error
  testthat::expect_error(suppressWarnings(qtail_pit_plot(fit, breaks = 20)), NA)
})

testthat::test_that("qtail_stability computes stability metric", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(5555)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  stab <- qtail_stability(fit)

  testthat::expect_type(stab, "list")
  testthat::expect_true(all(c("stability", "qce", "slope", "overfit_gap") %in% names(stab)))
  testthat::expect_true(is.numeric(stab$stability))
  testthat::expect_true(is.numeric(stab$qce))
  testthat::expect_true(is.numeric(stab$slope))
  testthat::expect_true(is.numeric(stab$overfit_gap))
})

testthat::test_that("qtail_validate works on new data", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(6666)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  # Create new data
  x_new <- matrix(rnorm(250), ncol = 5)
  y_new <- x_new[, 1] * 2 + rt(50, df = 3)

  val <- qtail_validate(fit, x_new, y_new)

  testthat::expect_s3_class(val, "data.frame")
  testthat::expect_true(all(c("tau", "observed", "qce") %in% names(val)))
  testthat::expect_true(all(val$qce >= 0))
  testthat::expect_true("pit_mean" %in% names(val))
  testthat::expect_true("pit_sd" %in% names(val))
})

testthat::test_that("qtail_validate works with custom taus", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(6667)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  # Create new data
  x_new <- matrix(rnorm(250), ncol = 5)
  y_new <- x_new[, 1] * 2 + rt(50, df = 3)

  custom_taus <- c(0.95, 0.99)
  val <- qtail_validate(fit, x_new, y_new, taus = custom_taus)

  testthat::expect_equal(nrow(val), length(custom_taus))
})
