test_that("mqtail fit works with formula interface", {
  skip_on_cran()
  
  set.seed(123)
  n <- 200
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cluster = sample(c("A", "B", "C"), n, replace = TRUE)
  )
  df$y <- 1.5 * df$x1 + 0.8 * df$x2 + rt(n, df = 3)
  
  fit <- mqtail(
    y ~ x1 + x2,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95, 0.99),
    threshold_tau = 0.99,
    params = list(nrounds = 20, nfolds = 2),
    verbose = FALSE
  )
  
  expect_s3_class(fit, "mqtail")
  expect_equal(fit$multiplexer, "cluster")
  expect_equal(fit$taus, c(0.9, 0.95, 0.99))
  expect_equal(fit$threshold_tau, 0.99)
  expect_equal(fit$tail, "upper")
  expect_length(fit$mqbm_models, 3)
  expect_equal(names(fit$mqbm_models), c("0.9", "0.95", "0.99"))
})

test_that("mqtail fit works with matrix interface", {
  skip_on_cran()
  
  set.seed(124)
  n <- 150
  X <- matrix(rnorm(n * 2), ncol = 2)
  colnames(X) <- c("x1", "x2")
  cluster <- sample(c("A", "B"), n, replace = TRUE)
  y <- X[, 1] * 1.5 + X[, 2] * 0.8 + rnorm(n)
  
  fit <- mqtail(
    x = X,
    y = y,
    cluster = cluster,
    multiplexer = "cluster",
    tail = "lower",
    taus = c(0.01, 0.05, 0.1),
    threshold_tau = 0.01,
    params = list(nrounds = 20, nfolds = 2),
    verbose = FALSE
  )
  
  expect_s3_class(fit, "mqtail")
  expect_equal(fit$tail, "lower")
  expect_equal(fit$taus, c(0.01, 0.05, 0.1))
  expect_equal(fit$threshold_tau, 0.01)
  expect_length(fit$mqbm_models, 3)
})

test_that("mqtail fit works with custom multiplexer parameter", {
  skip_on_cran()
  
  set.seed(125)
  n <- 180
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cluster = sample(c("C1", "C2"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1 + x2,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 20, nfolds = 2),
    verbose = FALSE
  )
  
  expect_s3_class(fit, "mqtail")
  expect_equal(fit$multiplexer, "cluster")
  expect_equal(fit$multiplexer_values, c("C1", "C2"))
  expect_length(fit$evt_models, 2)
})

test_that("mqtail print works", {
  skip_on_cran()
  
  set.seed(126)
  n <- 120
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  # Test that print doesn't error and shows expected content
  expect_output(print(fit), "Extreme Tail Quantile Model")
  expect_output(print(fit), "upper")
  expect_output(print(fit), "cluster")
})

test_that("mqtail summary works", {
  skip_on_cran()
  
  set.seed(127)
  n <- 120
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  summ <- summary(fit)
  
  expect_s3_class(summ, "mqtail_summary")
  expect_output(print(summ), "Extreme Tail Quantile Model")
  expect_output(print(summ), "upper")
})

test_that("mqtail predict works with surface type", {
  skip_on_cran()
  
  set.seed(128)
  n <- 150
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + df$x2 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1 + x2,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  newdata <- data.frame(
    x1 = c(0, 1),
    x2 = c(0, 1),
    cluster = c("A", "B")
  )
  
  preds <- predict(fit, newdata, type = "surface")
  
  expect_type(preds, "double")
  expect_length(preds, 2)
  expect_true(all(is.finite(preds)))
})

test_that("mqtail predict works with quantile type", {
  skip_on_cran()
  
  set.seed(129)
  n <- 150
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + df$x2 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1 + x2,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  newdata <- data.frame(
    x1 = c(0, 1),
    x2 = c(0, 1),
    cluster = c("A", "B")
  )
  
  preds <- predict(fit, newdata, type = "quantile")
  
  # predict returns the mean across quantiles as a numeric vector
  expect_type(preds, "double")
  expect_length(preds, 2)
  expect_true(all(is.finite(preds)))
})

test_that("mqtail fitted works", {
  skip_on_cran()
  
  set.seed(130)
  n <- 120
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  fitted_vals <- fitted(fit, type = "surface")
  
  expect_type(fitted_vals, "double")
  expect_length(fitted_vals, n)
  expect_true(all(is.finite(fitted_vals)))
})

test_that("mqtail residuals works", {
  skip_on_cran()
  
  set.seed(131)
  n <- 120
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  resids <- residuals(fit)
  
  expect_type(resids, "double")
  expect_length(resids, n)
  expect_true(all(is.finite(resids)))
})

test_that("mqtail coef works", {
  skip_on_cran()
  
  set.seed(132)
  n <- 120
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + df$x2 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1 + x2,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  coefs <- coef(fit)
  
  expect_s3_class(coefs, "data.frame")
  expect_equal(nrow(coefs), 2)  # Two values
  expect_true("value" %in% names(coefs))
  expect_true("xi" %in% names(coefs))
  expect_true("beta" %in% names(coefs))
})

test_that("mqtail works with train/val split", {
  skip_on_cran()
  
  set.seed(133)
  n <- 200
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + df$x2 + rnorm(n)
  
  train_idx <- 1:150
  val_idx <- 151:200
  
  fit <- mqtail(
    y ~ x1 + x2,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    train_idx = train_idx,
    val_idx = val_idx,
    verbose = FALSE
  )
  
  expect_s3_class(fit, "mqtail")
  expect_length(fit$mqbm_models, 2)
})

test_that("mqtail errors with missing multiplexer column", {
  skip_on_cran()
  
  set.seed(134)
  n <- 100
  df <- data.frame(
    x1 = rnorm(n),
    y = rnorm(n)
  )
  
  expect_error(
    mqtail(
      y ~ x1,
      data = df,
      multiplexer = "cluster",
      params = list(nrounds = 10)
    ),
    "cluster"
  )
})

test_that("mqtail errors with invalid threshold_tau", {
  skip_on_cran()
  
  set.seed(135)
  n <- 100
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  expect_error(
    mqtail(
      y ~ x1,
      data = df,
      multiplexer = "cluster",
      taus = c(0.9, 0.95),
      threshold_tau = 0.99,
      params = list(nrounds = 10)
    ),
    "threshold_tau.*taus"
  )
})

test_that("mqtail default taus work correctly for upper tail", {
  skip_on_cran()
  
  set.seed(136)
  n <- 120
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  expect_equal(fit$taus, c(0.95, 0.97, 0.99, 0.995))
  expect_equal(fit$threshold_tau, 0.99)
})

test_that("mqtail default taus work correctly for lower tail", {
  skip_on_cran()
  
  set.seed(137)
  n <- 120
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1,
    data = df,
    multiplexer = "cluster",
    tail = "lower",
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  expect_equal(fit$taus, c(0.05, 0.03, 0.01, 0.005))
  expect_equal(fit$threshold_tau, 0.01)
})

test_that("mqtail handles custom folds", {
  skip_on_cran()
  
  set.seed(138)
  n <- 150
  df <- data.frame(
    x1 = rnorm(n),
    x2 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + df$x2 + rnorm(n)
  
  # Create custom folds
  folds <- list(
    1:50,
    51:100,
    101:150
  )
  
  fit <- mqtail(
    y ~ x1 + x2,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10),
    folds = folds,
    verbose = FALSE
  )
  
  expect_s3_class(fit, "mqtail")
  expect_length(fit$mqbm_models, 2)
})

test_that("mqtail GPD fitting handles small exceedances", {
  skip_on_cran()
  
  set.seed(139)
  n <- 80  # Small sample to test edge case
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  # This should work even with small samples
  fit <- mqtail(
    y ~ x1,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95, 0.99),
    threshold_tau = 0.99,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  expect_s3_class(fit, "mqtail")
  expect_true(all(sapply(fit$evt_models, function(x) !is.null(x$xi))))
  expect_true(all(sapply(fit$evt_models, function(x) !is.null(x$beta))))
})

test_that("mqtail timing information is recorded", {
  skip_on_cran()
  
  set.seed(140)
  n <- 100
  df <- data.frame(
    x1 = rnorm(n),
    cluster = sample(c("A", "B"), n, replace = TRUE)
  )
  df$y <- df$x1 + rnorm(n)
  
  fit <- mqtail(
    y ~ x1,
    data = df,
    multiplexer = "cluster",
    tail = "upper",
    taus = c(0.9, 0.95),
    threshold_tau = 0.95,
    params = list(nrounds = 10, nfolds = 2),
    verbose = FALSE
  )
  
  expect_true(!is.null(fit$timings))
  expect_true(!is.null(fit$timings$start))
  expect_true(!is.null(fit$timings$end))
  expect_true(!is.null(fit$timings$elapsed))
  expect_true(fit$timings$elapsed >= 0)
})
