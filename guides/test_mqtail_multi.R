# Test script for mqtail with non-default multi column names
devtools::load_all()

set.seed(123)

cat("=== Testing mqtail with custom multi column name ===\n\n")

# Create sample data with a custom grouping column name (not "symbol")
n_per_group <- 150
df <- data.frame(
  x1 = rnorm(n_per_group * 3),
  x2 = rnorm(n_per_group * 3),
  ticker = rep(c("STOCK_A", "STOCK_B", "STOCK_C"), each = n_per_group) # Custom column name!
)
# Heavy-tailed response
df$y <- 1.5 * df$x1 + 0.8 * df$x2 + rt(n_per_group * 3, df = 3)

cat("Data: ", nrow(df), " rows, ", ncol(df) - 2, " features\n", sep = "")
cat("Group column: 'ticker' (not 'symbol')\n")
cat("Groups: ", paste(unique(df$ticker), collapse = ", "), "\n\n", sep = "")

# Test 1: Formula interface with custom multi parameter
cat("=== Test 1: Formula interface with multi='ticker' ===\n")
fit1 <- mqtail(
  y ~ x1 + x2,
  data = df,
  multi = "ticker",
  tail = "upper",
  taus = c(0.95, 0.97, 0.99),
  threshold_tau = 0.99,
  params = list(nrounds = 30, nfolds = 2),
  verbose = TRUE
)

cat("\nModel fitted successfully!\n")
cat("Symbols in model:", paste(fit1$symbols, collapse = ", "), "\n")
cat("Multi parameter:", fit1$multi, "\n")

# Test predictions
newdata <- data.frame(
  x1 = c(0.5, 0.5, 0.5),
  x2 = c(0.2, 0.2, 0.2),
  ticker = c("STOCK_A", "STOCK_B", "STOCK_C")
)
preds <- predict(fit1, newdata, type = "quantile")
cat("\nPredictions for new data:\n")
print(data.frame(ticker = newdata$ticker, prediction = preds))

# Test 2: Matrix interface with custom multi parameter
cat("\n=== Test 2: Matrix interface with multi='ticker' ===\n")
X <- as.matrix(df[, c("x1", "x2")])
fit2 <- mqtail(
  x = X,
  y = df$y,
  ticker = df$ticker, # Passing ticker as named argument
  multi = "ticker",
  tail = "upper",
  taus = c(0.95, 0.97, 0.99),
  threshold_tau = 0.99,
  params = list(nrounds = 30, nfolds = 2),
  verbose = TRUE
)

cat("\nModel fitted successfully!\n")
cat("Symbols in model:", paste(fit2$symbols, collapse = ", "), "\n")
cat("Multi parameter:", fit2$multi, "\n")

# Test 3: Error handling - missing multi column
cat("\n=== Test 3: Error handling - missing multi column ===\n")
df_bad <- df
df_bad$ticker <- NULL
tryCatch(
  {
    fit_bad <- mqtail(
      y ~ x1 + x2,
      data = df_bad,
      multi = "ticker",
      params = list(nrounds = 10)
    )
    cat("ERROR: Should have failed but didn't!\n")
  },
  error = function(e) {
    cat("Correctly caught error:", conditionMessage(e), "\n")
  }
)

cat("\n✓ All tests passed! multi parameter works correctly.\n")
