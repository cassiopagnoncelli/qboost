test_that("residuals.qbm works correctly", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  df$y <- df$x1 * 0.5 + rnorm(100)
  
  # Fit qbm model
  fit <- qbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Get residuals
  resid <- residuals(fit)
  
  # Check basic properties
  expect_length(resid, 100)
  expect_type(resid, "double")
  expect_false(any(is.na(resid)))
  
  # Residuals should equal y - fitted
  fitted_vals <- fitted(fit)
  expected_resid <- df$y - fitted_vals
  expect_equal(as.numeric(resid), as.numeric(expected_resid))
  
  # For median quantile, residuals should be roughly centered
  if (fit$tau == 0.5) {
    expect_true(abs(median(resid)) < 0.5)
  }
})

test_that("residuals.mqbm works correctly", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Fit mqbm model
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Get residuals
  resid <- residuals(fit)
  
  # Check basic properties
  expect_length(resid, 200)
  expect_type(resid, "double")
  expect_false(any(is.na(resid)))
  
  # Residuals should equal y - fitted (using default type="surface")
  fitted_vals <- fitted(fit, type = "surface")
  expect_equal(resid, df$y - fitted_vals)
})

test_that("residuals.mqbm returns residuals in correct order", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(150),
    x2 = rnorm(150),
    cluster = sample(c("A", "B"), 150, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(150)
  
  # Fit mqbm model
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Get residuals and fitted values
  resid <- residuals(fit)
  fitted_vals <- fitted(fit, type = "surface")
  
  # Check each value's residuals
  for (val in fit$multiplexer_values) {
    idx <- which(df$cluster == val)
    
    # Extract residuals for this value
    resid_val <- resid[idx]
    fitted_val <- fitted_vals[idx]
    y_val <- df$y[idx]
    
    # Verify residuals = y - fitted for this value
    expect_equal(resid_val, y_val - fitted_val)
  }
})

test_that("residuals.qbm errors with wrong object type", {
  expect_error(
    residuals.qbm(list(foo = "bar")),
    "must be a qbm model"
  )
})

test_that("residuals.mqbm errors with wrong object type", {
  expect_error(
    residuals.mqbm(list(foo = "bar")),
    "must be a mqbm model"
  )
})

test_that("residuals work with x/y interface for qbm", {
  set.seed(123)
  x <- matrix(rnorm(200), ncol = 2)
  y <- x[, 1] * 0.5 + rnorm(100)
  
  fit <- qbm(x = x, y = y, tau = 0.5, nrounds = 20, nfolds = 2)
  
  resid <- residuals(fit)
  
  expect_length(resid, 100)
  expect_equal(resid, y - fit$training$fitted)
})

test_that("residuals work with x/y/multiplexer interface for mqbm", {
  set.seed(123)
  x <- matrix(rnorm(200), ncol = 2)
  y <- x[, 1] * 0.5 + rnorm(100)
  cluster <- sample(c("A", "B"), 100, replace = TRUE)
  
  fit <- mqbm(x = x, y = y, cluster = cluster, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  resid <- residuals(fit)
  fitted_vals <- fitted(fit, type = "surface")
  
  expect_length(resid, 100)
  expect_equal(resid, y - fitted_vals)
})

test_that("residuals sum properties for median quantile", {
  set.seed(456)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200)
  )
  df$y <- df$x1 * 2 + df$x2 * (-1) + rnorm(200)
  
  # Fit at median
  fit <- qbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50, nfolds = 3)
  
  resid <- residuals(fit)
  
  # For median, approximately half should be positive, half negative
  prop_positive <- mean(resid > 0)
  expect_true(prop_positive > 0.3 && prop_positive < 0.7)
})
