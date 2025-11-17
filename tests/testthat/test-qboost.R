testthat::test_that("qboost fits and predicts", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")

  set.seed(123)
  x <- matrix(rnorm(200), ncol = 2)
  y <- x[, 1] * 0.5 + x[, 2] * 0.1 + rnorm(100)

  fit <- qboost(x, y, tau = 0.4, nrounds = 25, nfolds = 2, early_stopping_rounds = 5)
  preds <- predict(fit, x[1:5, ])

  testthat::expect_s3_class(fit, "qboost")
  testthat::expect_true(is.numeric(preds))
  testthat::expect_length(preds, 5)
})

testthat::test_that("summary works", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")

  set.seed(321)
  x <- matrix(rnorm(100), ncol = 2)
  y <- x[, 1] - x[, 2] + rnorm(50)

  fit <- qboost(x, y, tau = 0.5, nrounds = 20, nfolds = 2, early_stopping_rounds = 5)
  s <- summary(fit)

  testthat::expect_s3_class(s, "qboost_summary")
  testthat::expect_true(is.list(s$metrics))
  testthat::expect_true(!is.null(capture.output(print(s))))
})

testthat::test_that("plots are returned as ggplot list", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")

  set.seed(222)
  x <- matrix(rnorm(120), ncol = 3)
  y <- x[, 1] * 0.2 + rnorm(40)

  fit <- qboost(x, y, tau = 0.25, nrounds = 15, nfolds = 2, early_stopping_rounds = 3)
  plots <- plot(fit, plot = FALSE)

  testthat::expect_type(plots, "list")
  testthat::expect_true(all(vapply(plots, inherits, logical(1), what = "ggplot")))
})

testthat::test_that("importance returns a data frame", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("lightgbm")

  set.seed(111)
  x <- matrix(rnorm(120), ncol = 3)
  y <- x[, 1] * 0.4 + rnorm(40)

  fit <- qboost(x, y, tau = 0.7, nrounds = 15, nfolds = 2, early_stopping_rounds = 3)
  imp <- importance.qboost(fit)

  df <- as.data.frame(imp)
  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_true(nrow(df) >= 0)
})
