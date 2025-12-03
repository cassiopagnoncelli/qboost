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

test_that("importance.mqbm returns aggregated importance", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    x3 = rnorm(200),
    symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.8 + df$x2 * 0.3 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2 + x3, data = df, tau = 0.5, nrounds = 30, nfolds = 2)
  
  imp <- importance(fit)
  
  # Should be a tibble
  expect_s3_class(imp, "tbl_df")
  
  # Should have the right columns
  expect_true(all(c("feature", "gain", "sd_gain", "n_models") %in% names(imp)))
  
  # Should have features
  expect_gt(nrow(imp), 0)
  expect_true(all(c("x1", "x2", "x3") %in% imp$feature))
  
  # Should be sorted by gain descending
  expect_equal(imp$gain, sort(imp$gain, decreasing = TRUE))
  
  # All values should be non-negative
  expect_true(all(imp$gain >= 0))
  expect_true(all(imp$sd_gain >= 0, na.rm = TRUE))
  
  # n_models should be between 1 and number of symbols
  expect_true(all(imp$n_models >= 1 & imp$n_models <= length(fit$symbols)))
})

test_that("importance.mqbm handles features present in subset of models", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 30, nfolds = 2)
  
  imp <- importance(fit)
  
  # Should report n_models for each feature
  expect_true(all(imp$n_models > 0))
  
  # gain and sd_gain should be computed correctly
  expect_false(any(is.na(imp$gain)))
})

test_that("importance.mqbm works with custom multi parameter", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    group = sample(c("G1", "G2"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multi = "group", tau = 0.5, nrounds = 30, nfolds = 2)
  
  imp <- importance(fit)
  
  expect_s3_class(imp, "tbl_df")
  expect_gt(nrow(imp), 0)
  expect_true(all(c("feature", "gain", "sd_gain", "n_models") %in% names(imp)))
})

test_that("summary.mqbm returns correct structure", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.8 + df$x2 * 0.3 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 30, nfolds = 2)
  summ <- summary(fit)
  
  # Check class
  expect_s3_class(summ, "mqbm_summary")
  
  # Check required components
  expect_true(all(c("tau", "multi", "n_symbols", "symbols", "data_info", 
                    "metrics", "calibration", "complexity", "symbol_table", 
                    "importance", "timings") %in% names(summ)))
  
  # Check metrics structure
  expect_true("overall" %in% names(summ$metrics))
  expect_true("per_symbol" %in% names(summ$metrics))
  expect_true(all(c("pinball_loss", "mae", "pseudo_r2") %in% names(summ$metrics$overall)))
  
  # Check calibration structure
  expect_true(all(c("overall_coverage", "overall_qce", "coverage_sd") %in% names(summ$calibration)))
  
  # Check symbol table
  expect_s3_class(summ$symbol_table, "tbl_df")
  expect_equal(nrow(summ$symbol_table), 3)
  expect_true(all(c("symbol", "n", "trees", "pinball", "mae", "pseudo_r2", 
                    "coverage", "qce") %in% names(summ$symbol_table)))
})

test_that("summary.mqbm computes aggregate metrics correctly", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 30, nfolds = 2)
  summ <- summary(fit)
  
  # Aggregate metrics should be computed
  expect_false(is.na(summ$metrics$overall$pinball_loss))
  expect_false(is.na(summ$metrics$overall$mae))
  expect_false(is.na(summ$metrics$overall$pseudo_r2))
  
  # Per-symbol metrics should exist for each symbol
  expect_equal(length(summ$metrics$per_symbol), 2)
  expect_true(all(c("A", "B") %in% names(summ$metrics$per_symbol)))
  
  # Each symbol should have all metrics
  for (sym in c("A", "B")) {
    expect_true(all(c("n", "pinball", "mae", "pseudo_r2", "coverage", "qce") %in% 
                      names(summ$metrics$per_symbol[[sym]])))
  }
})

test_that("print.mqbm_summary works without error", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 30, nfolds = 2)
  
  # Detailed summary
  summ_detailed <- summary(fit, detailed = TRUE)
  expect_output(print(summ_detailed), "Multi Quantile Gradient Boosting Model")
  expect_output(print(summ_detailed), "Aggregate Training Metrics")
  expect_output(print(summ_detailed), "Per-Symbol Summary")
  
  # Compact summary
  summ_compact <- summary(fit, detailed = FALSE)
  expect_output(print(summ_compact), "Multi Quantile Gradient Boosting Model")
  expect_output(print(summ_compact), "Use detailed = TRUE for full report")
})

test_that("summary.mqbm works with custom multi parameter", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    category = sample(c("Cat1", "Cat2"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multi = "category", tau = 0.5, nrounds = 30, nfolds = 2)
  summ <- summary(fit)
  
  expect_s3_class(summ, "mqbm_summary")
  expect_equal(summ$multi, "category")
  expect_equal(summ$n_symbols, 2)
})

test_that("predict.mqbm type='surface' returns raw quantile values", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    symbol = sample(c("A", "B"), 50, replace = TRUE)
  )
  
  preds_surface <- predict(fit, newdata, type = "surface")
  
  expect_length(preds_surface, 50)
  expect_type(preds_surface, "double")
  # Surface predictions are not constrained to [0,1]
  expect_true(any(preds_surface < 0) || any(preds_surface > 1))
})

test_that("predict.mqbm type='quantile' returns ECDF probabilities", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    symbol = sample(c("A", "B"), 50, replace = TRUE)
  )
  
  preds_quantile <- predict(fit, newdata, type = "quantile")
  
  expect_length(preds_quantile, 50)
  expect_type(preds_quantile, "double")
  # Quantile predictions should be probabilities in [0,1]
  expect_true(all(preds_quantile >= 0 & preds_quantile <= 1))
})

test_that("fitted.mqbm type='surface' returns raw fitted values", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  fitted_surface <- fitted(fit, type = "surface")
  
  expect_length(fitted_surface, 200)
  expect_type(fitted_surface, "double")
  # Surface fitted values are not constrained to [0,1]
  expect_true(any(fitted_surface < 0) || any(fitted_surface > 1))
})

test_that("fitted.mqbm type='quantile' returns ECDF probabilities", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  fitted_quantile <- fitted(fit, type = "quantile")
  
  expect_length(fitted_quantile, 200)
  expect_type(fitted_quantile, "double")
  # Quantile fitted values should be probabilities in [0,1]
  expect_true(all(fitted_quantile >= 0 & fitted_quantile <= 1))
})

test_that("type parameter defaults to 'surface' for backward compatibility", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    symbol = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 20, nfolds = 2)
  
  newdata <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20),
    symbol = sample(c("A", "B"), 20, replace = TRUE)
  )
  
  # Without specifying type, should default to "surface"
  preds_default <- predict(fit, newdata)
  preds_surface <- predict(fit, newdata, type = "surface")
  
  expect_equal(preds_default, preds_surface)
  
  fitted_default <- fitted(fit)
  fitted_surface <- fitted(fit, type = "surface")
  
  expect_equal(fitted_default, fitted_surface)
})
