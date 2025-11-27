testthat::test_that("qtail fits upper tail with defaults", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(123)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + x[, 2] * 0.5 + rt(100, df = 3) * 2

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  testthat::expect_s3_class(fit, "qtail")
  testthat::expect_equal(fit$tail, "upper")
  testthat::expect_equal(fit$taus, c(0.95, 0.97, 0.99, 0.993, 0.999), tolerance = 1e-10)
  testthat::expect_equal(fit$threshold_tau, 0.99)
  testthat::expect_equal(fit$tau_target, 0.999, tolerance = 1e-10)
  testthat::expect_equal(length(fit$models), 5)
  testthat::expect_true(!is.null(fit$stack$coef))
  testthat::expect_true(!is.null(fit$evt$xi))
  testthat::expect_true(!is.null(fit$evt$beta))
})

testthat::test_that("qtail fits lower tail with defaults", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(456)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + x[, 2] * 0.5 + rt(100, df = 3) * 2

  fit <- suppressWarnings(qtail(x, y, tail = "lower", params = list(nrounds = 20, nfolds = 2)))

  testthat::expect_s3_class(fit, "qtail")
  testthat::expect_equal(fit$tail, "lower")
  testthat::expect_equal(fit$taus, c(0.05, 0.03, 0.01, 0.007, 0.001), tolerance = 1e-10)
  testthat::expect_equal(fit$threshold_tau, 0.01)
  testthat::expect_equal(fit$tau_target, 0.001, tolerance = 1e-10)
})

testthat::test_that("qtail with custom taus", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(789)
  x <- matrix(rnorm(400), ncol = 4)
  y <- x[, 1] + rnorm(100)

  custom_taus <- c(0.9, 0.95, 0.99)
  fit <- suppressWarnings(qtail(x, y,
    taus = custom_taus, tail = "upper",
    threshold_tau = 0.95, params = list(nrounds = 15, nfolds = 2)
  ))

  testthat::expect_equal(fit$taus, custom_taus)
  testthat::expect_equal(fit$threshold_tau, 0.95)
  testthat::expect_equal(fit$tau_target, 0.99)
})

testthat::test_that("qtail handles NA rows with warning", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(321)
  x <- matrix(rnorm(400), ncol = 4)
  y <- x[, 1] + rnorm(100)

  x[1:5, 1] <- NA
  y[6:8] <- NA

  # Capture warnings
  w <- testthat::capture_warnings(
    fit <- qtail(x, y, tail = "upper", params = list(nrounds = 15, nfolds = 2))
  )

  # Check that NA removal warning is present
  testthat::expect_true(any(grepl("Removed .* rows with NA values", w)))
  testthat::expect_equal(fit$n, 92)
})

testthat::test_that("qtail errors if threshold_tau not in taus", {
  testthat::skip_on_cran()

  set.seed(111)
  x <- matrix(rnorm(200), ncol = 2)
  y <- rnorm(100)

  testthat::expect_error(
    qtail(x, y, taus = c(0.9, 0.95, 0.99), threshold_tau = 0.975),
    "threshold_tau must be one of the values in taus"
  )
})

testthat::test_that("predict.qtail works with type='final'", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(222)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))
  preds <- predict(fit, x[1:10, ], type = "final")

  testthat::expect_true(is.numeric(preds))
  testthat::expect_length(preds, 10)
})

testthat::test_that("predict.qtail works with type='stack'", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(333)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))
  preds <- predict(fit, x[1:10, ], type = "stack")

  testthat::expect_true(is.numeric(preds))
  testthat::expect_length(preds, 10)
})

testthat::test_that("predict.qtail works with type='grid'", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(444)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))
  preds <- predict(fit, x[1:10, ], type = "grid")

  testthat::expect_true(is.matrix(preds))
  testthat::expect_equal(nrow(preds), 10)
  testthat::expect_equal(ncol(preds), length(fit$taus))
  testthat::expect_equal(colnames(preds), as.character(fit$taus))
})

testthat::test_that("predict.qtail works with type='evt'", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(555)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))
  evt_ext <- predict(fit, x[1:10, ], type = "evt")

  testthat::expect_true(is.numeric(evt_ext))
  testthat::expect_length(evt_ext, 10)
  testthat::expect_true(all(evt_ext > 0))
})

testthat::test_that("predict.qtail lower tail EVT extension is negative", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(666)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "lower", params = list(nrounds = 20, nfolds = 2)))

  preds_stack <- predict(fit, x[1:10, ], type = "stack")
  preds_final <- predict(fit, x[1:10, ], type = "final")

  testthat::expect_true(all(preds_final < preds_stack))
})

testthat::test_that("summary.qtail produces output", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(777)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  output <- capture.output(summary(fit))

  testthat::expect_true(length(output) > 0)
  testthat::expect_true(any(grepl("Extreme Quantile Tail Model", output)))
  testthat::expect_true(any(grepl("Tail:", output)))
  testthat::expect_true(any(grepl("Extreme Value Theory", output)))
  testthat::expect_true(any(grepl("Coverage @ target", output)))
})

testthat::test_that("print.qtail produces output", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(888)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  output <- capture.output(print(fit))

  testthat::expect_true(length(output) > 0)
  testthat::expect_true(any(grepl("Extreme Quantile Tail Model", output)))
  testthat::expect_true(any(grepl("Tail:", output)))
  testthat::expect_true(any(grepl("Data:", output)))
})

testthat::test_that("qtail stacking coefficients are stored correctly", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(999)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  testthat::expect_equal(length(fit$stack$coef), length(fit$taus) + 1)
  testthat::expect_true(!is.null(names(fit$stack$coef)))
  testthat::expect_equal(names(fit$stack$coef)[1], "(Intercept)")
})

testthat::test_that("qtail EVT parameters are reasonable", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(1234)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  testthat::expect_true(is.numeric(fit$evt$xi))
  testthat::expect_true(is.numeric(fit$evt$beta))
  testthat::expect_true(fit$evt$beta > 0)
  testthat::expect_true(fit$evt$n_exceedances > 0)
})

testthat::test_that("qtail minimal data still fits", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(42)
  x <- matrix(rnorm(100), ncol = 2)
  y <- x[, 1] + rnorm(50)

  fit <- suppressWarnings(qtail(x, y,
    tail = "upper",
    taus = c(0.9, 0.95, 0.99),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2)
  ))

  testthat::expect_s3_class(fit, "qtail")
})

testthat::test_that("qtail prediction types produce different results", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(5678)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  preds_stack <- predict(fit, x[1:5, ], type = "stack")
  preds_final <- predict(fit, x[1:5, ], type = "final")
  evt_ext <- predict(fit, x[1:5, ], type = "evt")

  testthat::expect_true(!identical(preds_stack, preds_final))
  testthat::expect_equal(preds_final, preds_stack + evt_ext)
})

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

testthat::test_that("qtail_calibration_slope computes slope", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")
  testthat::skip_if_not_installed("glmnet")
  testthat::skip_if_not_installed("evgam")

  set.seed(2222)
  x <- matrix(rnorm(500), ncol = 5)
  y <- x[, 1] * 2 + rt(100, df = 3)

  fit <- suppressWarnings(qtail(x, y, tail = "upper", params = list(nrounds = 20, nfolds = 2)))

  slope_data <- qtail_calibration_slope(fit)

  testthat::expect_type(slope_data, "list")
  testthat::expect_true(all(c("slope", "intercept", "tau") %in% names(slope_data)))
  testthat::expect_true(is.numeric(slope_data$slope))
  testthat::expect_true(is.numeric(slope_data$intercept))
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
})
