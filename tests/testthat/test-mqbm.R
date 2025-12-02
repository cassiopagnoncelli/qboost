test_that("mqbm works with formula interface", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test formula interface
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  expect_s3_class(fit, "mqbm")
  expect_equal(length(fit$symbols), 3)
  expect_true(all(c("A", "B", "C") %in% fit$symbols))
  expect_equal(fit$data_info$n, 200)
  expect_equal(fit$tau, 0.5)
  
  # Each model should be a qbm
  for (sym in fit$symbols) {
    expect_s3_class(fit$models[[sym]], "qbm")
  }
})

test_that("mqbm works with x/y/symbol interface", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test x/y/symbol interface
  fit <- mqbm(
    x = df[, c("x1", "x2")],
    y = df$y,
    symbol = df$symbol,
    tau = 0.3,
    nrounds = 20,
    nfolds = 2
  )
  
  expect_s3_class(fit, "mqbm")
  expect_equal(length(fit$symbols), 2)
  expect_true(all(c("A", "B") %in% fit$symbols))
  expect_equal(fit$tau, 0.3)
})

test_that("predict.mqbm works with symbol in newdata", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Create new data with symbol column
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    symbol = sample(c("A", "B"), 50, replace = TRUE)
  )
  
  preds <- predict(fit, newdata)
  
  expect_length(preds, 50)
  expect_type(preds, "double")
  expect_false(any(is.na(preds)))
})

test_that("predict.mqbm works with separate symbol argument", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Create new data without symbol column
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )
  symbols <- sample(c("A", "B"), 50, replace = TRUE)
  
  preds <- predict(fit, newdata, symbol = symbols)
  
  expect_length(preds, 50)
  expect_type(preds, "double")
  expect_false(any(is.na(preds)))
})

test_that("predict.mqbm errors on unknown symbols", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Try to predict with unknown symbol
  newdata <- data.frame(
    x1 = rnorm(10),
    x2 = rnorm(10),
    symbol = rep("C", 10)  # "C" was not in training data
  )
  
  expect_error(predict(fit, newdata), "Unknown symbols")
})

test_that("fitted.mqbm returns correct length", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  fitted_vals <- fitted(fit)
  
  expect_length(fitted_vals, 200)
  expect_type(fitted_vals, "double")
  expect_false(any(is.na(fitted_vals)))
})

test_that("print.mqbm works without error", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100),
    symbol = sample(c("A", "B"), 100, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(100)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Should not error
  expect_output(print(fit), "Symbol-based Quantile Gradient Boosting Model")
  expect_output(print(fit), "Symbols:")
})

test_that("mqbm errors when symbol column missing in formula interface", {
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  df$y <- df$x1 * 0.5 + rnorm(100)
  
  expect_error(
    mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20),
    "must contain a 'symbol' column"
  )
})

test_that("mqbm works with different tau values per symbol", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test with different tau
  fit_median <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  fit_upper <- mqbm(y ~ x1 + x2, data = df, tau = 0.9, nrounds = 20, nfolds = 2)
  
  expect_equal(fit_median$tau, 0.5)
  expect_equal(fit_upper$tau, 0.9)
  
  # Predictions should generally be different
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    symbol = sample(c("A", "B"), 50, replace = TRUE)
  )
  
  preds_median <- predict(fit_median, newdata)
  preds_upper <- predict(fit_upper, newdata)
  
  expect_false(all(preds_median == preds_upper))
})

test_that("mqbm works with custom multi parameter", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    group = sample(c("Group1", "Group2", "Group3"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test with custom multi column name
  fit <- mqbm(y ~ x1 + x2, data = df, multi = "group", tau = 0.5, nrounds = 20, nfolds = 2)
  
  expect_s3_class(fit, "mqbm")
  expect_equal(fit$multi, "group")
  expect_equal(length(fit$symbols), 3)
  expect_true(all(c("Group1", "Group2", "Group3") %in% fit$symbols))
  
  # Prediction should work with group column
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    group = sample(c("Group1", "Group2", "Group3"), 50, replace = TRUE)
  )
  
  preds <- predict(fit, newdata)
  expect_length(preds, 50)
  expect_false(any(is.na(preds)))
})

test_that("mqbm multi parameter works with x/y interface", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    category = sample(c("Cat1", "Cat2"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test x/y/category interface (using named argument)
  fit <- mqbm(
    x = df[, c("x1", "x2")],
    y = df$y,
    category = df$category,
    multi = "category",
    tau = 0.5,
    nrounds = 20,
    nfolds = 2
  )
  
  expect_s3_class(fit, "mqbm")
  expect_equal(fit$multi, "category")
  expect_equal(length(fit$symbols), 2)
  
  # Prediction with multi argument
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )
  categories <- sample(c("Cat1", "Cat2"), 50, replace = TRUE)
  
  preds <- predict(fit, newdata, multi = categories)
  expect_length(preds, 50)
  expect_false(any(is.na(preds)))
})

test_that("mqbm backward compatibility with symbol parameter", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Should still work without specifying multi (defaults to "symbol")
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  expect_s3_class(fit, "mqbm")
  expect_equal(fit$multi, "symbol")
  
  # Prediction should work with symbol argument (backward compatibility)
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )
  symbols <- sample(c("A", "B"), 50, replace = TRUE)
  
  preds <- predict(fit, newdata, symbol = symbols)
  expect_length(preds, 50)
  expect_false(any(is.na(preds)))
})
