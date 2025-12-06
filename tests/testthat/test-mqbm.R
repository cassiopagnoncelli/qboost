test_that("mqbm works with formula interface", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test formula interface
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  expect_s3_class(fit, "mqbm")
  expect_equal(length(fit$multiplexer_values), 3)
  expect_true(all(c("A", "B", "C") %in% fit$multiplexer_values))
  expect_equal(fit$data_info$n, 200)
  expect_equal(fit$tau, 0.5)
  
  # Each model should be a qbm
  for (val in fit$multiplexer_values) {
    expect_s3_class(fit$models[[val]], "qbm")
  }
})

test_that("mqbm works with x/y/multiplexer interface", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test x/y/multiplexer interface
  fit <- mqbm(
    x = df[, c("x1", "x2")],
    y = df$y,
    cluster = df$cluster,
    multiplexer = "cluster",
    tau = 0.3,
    nrounds = 20,
    nfolds = 2
  )
  
  expect_s3_class(fit, "mqbm")
  expect_equal(length(fit$multiplexer_values), 2)
  expect_true(all(c("A", "B") %in% fit$multiplexer_values))
  expect_equal(fit$tau, 0.3)
})

test_that("predict.mqbm works with multiplexer in newdata", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Create new data with multiplexer column
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    cluster = sample(c("A", "B"), 50, replace = TRUE)
  )
  
  preds <- predict(fit, newdata)
  
  expect_length(preds, 50)
  expect_type(preds, "double")
  expect_false(any(is.na(preds)))
})

test_that("predict.mqbm works with separate multiplexer argument", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Create new data without multiplexer column
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50)
  )
  clusters <- sample(c("A", "B"), 50, replace = TRUE)
  
  preds <- predict(fit, newdata, multiplexer = clusters)
  
  expect_length(preds, 50)
  expect_type(preds, "double")
  expect_false(any(is.na(preds)))
})

test_that("predict.mqbm errors on unknown values", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Try to predict with unknown value
  newdata <- data.frame(
    x1 = rnorm(10),
    x2 = rnorm(10),
    cluster = rep("C", 10)  # "C" was not in training data
  )
  
  expect_error(predict(fit, newdata), "Unknown")
})

test_that("fitted.mqbm returns correct length", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
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
    cluster = sample(c("A", "B"), 100, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(100)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  # Should not error
  expect_output(print(fit), "Multiplexed Quantile Gradient Boosting Model")
  expect_output(print(fit), "Values:")
})

test_that("mqbm errors when multiplexer column missing in formula interface", {
  df <- data.frame(
    x1 = rnorm(100),
    x2 = rnorm(100)
  )
  df$y <- df$x1 * 0.5 + rnorm(100)
  
  expect_error(
    mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20),
    "must contain a 'cluster' column"
  )
})

test_that("mqbm works with different tau values", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test with different tau
  fit_median <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  fit_upper <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.9, nrounds = 20, nfolds = 2)
  
  expect_equal(fit_median$tau, 0.5)
  expect_equal(fit_upper$tau, 0.9)
  
  # Predictions should generally be different
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    cluster = sample(c("A", "B"), 50, replace = TRUE)
  )
  
  preds_median <- predict(fit_median, newdata)
  preds_upper <- predict(fit_upper, newdata)
  
  expect_false(all(preds_median == preds_upper))
})

test_that("mqbm works with custom multiplexer parameter", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    category = sample(c("Cat1", "Cat2", "Cat3"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  # Test with custom multiplexer column name
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "category", tau = 0.5, nrounds = 20, nfolds = 2)
  
  expect_s3_class(fit, "mqbm")
  expect_equal(fit$multiplexer, "category")
  expect_equal(length(fit$multiplexer_values), 3)
  expect_true(all(c("Cat1", "Cat2", "Cat3") %in% fit$multiplexer_values))
  
  # Prediction should work with category column
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    category = sample(c("Cat1", "Cat2", "Cat3"), 50, replace = TRUE)
  )
  
  preds <- predict(fit, newdata)
  expect_length(preds, 50)
  expect_false(any(is.na(preds)))
})

test_that("importance.mqbm returns aggregated importance", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    x3 = rnorm(200),
    cluster = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.8 + df$x2 * 0.3 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2 + x3, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 30, nfolds = 2)
  
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
  
  # n_models should be between 1 and number of values
  expect_true(all(imp$n_models >= 1 & imp$n_models <= length(fit$multiplexer_values)))
})

test_that("importance.mqbm handles features present in subset of models", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 30, nfolds = 2)
  
  imp <- importance(fit)
  
  # Should report n_models for each feature
  expect_true(all(imp$n_models > 0))
  
  # gain and sd_gain should be computed correctly
  expect_false(any(is.na(imp$gain)))
})

test_that("summary.mqbm returns correct structure", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B", "C"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.8 + df$x2 * 0.3 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 30, nfolds = 2)
  summ <- summary(fit)
  
  # Check class
  expect_s3_class(summ, "mqbm_summary")
  
  # Check required components
  expect_true(all(c("tau", "multiplexer", "n_values", "multiplexer_values", "data_info", 
                    "metrics", "calibration", "complexity", "value_table", 
                    "importance", "timings") %in% names(summ)))
  
  # Check metrics structure
  expect_true("overall" %in% names(summ$metrics))
  expect_true("per_value" %in% names(summ$metrics))
  expect_true(all(c("pinball_loss", "mae", "pseudo_r2") %in% names(summ$metrics$overall)))
  
  # Check calibration structure
  expect_true(all(c("overall_coverage", "overall_qce", "coverage_sd") %in% names(summ$calibration)))
  
  # Check value table
  expect_s3_class(summ$value_table, "tbl_df")
  expect_equal(nrow(summ$value_table), 3)
  expect_true(all(c("value", "n", "trees", "train_pinball", "train_mae", "train_r2", 
                    "train_cov", "train_qce") %in% names(summ$value_table)))
})

test_that("summary.mqbm computes aggregate metrics correctly", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 30, nfolds = 2)
  summ <- summary(fit)
  
  # Aggregate metrics should be computed
  expect_false(is.na(summ$metrics$overall$pinball_loss))
  expect_false(is.na(summ$metrics$overall$mae))
  expect_false(is.na(summ$metrics$overall$pseudo_r2))
  
  # Per-value metrics should exist for each value
  expect_equal(length(summ$metrics$per_value), 2)
  expect_true(all(c("A", "B") %in% names(summ$metrics$per_value)))
  
  # Each value should have all metrics
  for (val in c("A", "B")) {
    expect_true(all(c("n", "pinball", "mae", "pseudo_r2", "coverage", "qce") %in% 
                      names(summ$metrics$per_value[[val]])))
  }
})

test_that("print.mqbm_summary works without error", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 30, nfolds = 2)
  
  # Detailed summary
  summ_detailed <- summary(fit, detailed = TRUE)
  expect_output(print(summ_detailed), "Multiplexed Quantile Gradient Boosting Model")
  expect_output(print(summ_detailed), "Aggregate Training Metrics")
  expect_output(print(summ_detailed), "Per-Value Summary")
  
  # Compact summary
  summ_compact <- summary(fit, detailed = FALSE)
  expect_output(print(summ_compact), "Multiplexed Quantile Gradient Boosting Model")
  expect_output(print(summ_compact), "Use detailed = TRUE for full report")
})

test_that("predict.mqbm type='surface' returns raw quantile values", {
  set.seed(123)
  df <- data.frame(
    x1 = rnorm(200),
    x2 = rnorm(200),
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    cluster = sample(c("A", "B"), 50, replace = TRUE)
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
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  newdata <- data.frame(
    x1 = rnorm(50),
    x2 = rnorm(50),
    cluster = sample(c("A", "B"), 50, replace = TRUE)
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
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
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
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
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
    cluster = sample(c("A", "B"), 200, replace = TRUE)
  )
  df$y <- df$x1 * 0.5 + rnorm(200)
  
  fit <- mqbm(y ~ x1 + x2, data = df, multiplexer = "cluster", tau = 0.5, nrounds = 20, nfolds = 2)
  
  newdata <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20),
    cluster = sample(c("A", "B"), 20, replace = TRUE)
  )
  
  # Without specifying type, should default to "surface"
  preds_default <- predict(fit, newdata)
  preds_surface <- predict(fit, newdata, type = "surface")
  
  expect_equal(preds_default, preds_surface)
  
  fitted_default <- fitted(fit)
  fitted_surface <- fitted(fit, type = "surface")
  
  expect_equal(fitted_default, fitted_surface)
})
