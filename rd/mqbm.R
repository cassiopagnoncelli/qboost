# Demo: Symbol-based Quantile Boosting Model (mqbm)

devtools::load_all()

# Create sample data with multiple symbols
set.seed(123)
n <- 3000
df <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  symbol = sample(c("AAPL", "GOOGL", "MSFT"), n, replace = TRUE)
)

# Each symbol has different relationship with y
df$y <- ifelse(df$symbol == "AAPL",
               df$x1 * 2.0 + df$x2 * 0.5 + rnorm(n, sd = 0.5),
               ifelse(df$symbol == "GOOGL",
                      df$x1 * 0.5 + df$x2 * 2.0 + rnorm(n, sd = 0.5),
                      df$x1 * 1.0 + df$x2 * 1.0 + rnorm(n, sd = 0.5)))

# Split data
train_idx <- sample(1:n, size = floor(0.7 * n))
train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]

# ------------------------------------------------------------------
# Train mqbm model (formula interface)
# ------------------------------------------------------------------
cat("Training mqbm model...\n")
fit <- mqbm(
  y ~ x1 + x2,
  data = train_df,
  tau = 0.5,
  nrounds = 100,
  nfolds = 3
)

# Print model summary
print(fit)

# ------------------------------------------------------------------
# Predictions
# ------------------------------------------------------------------
cat("\n\nMaking predictions...\n")

# Method 1: symbol column in newdata
preds1 <- predict(fit, test_df)

# Method 2: separate symbol argument
preds2 <- predict(fit, test_df[, c("x1", "x2")], symbol = test_df$symbol)

cat("Predictions match:", all(preds1 == preds2), "\n")

# ------------------------------------------------------------------
# Fitted values
# ------------------------------------------------------------------
cat("\nFitted values on training data:\n")
fitted_vals <- fitted(fit)
cat("Length:", length(fitted_vals), "\n")
cat("Range:", range(fitted_vals), "\n")

# ------------------------------------------------------------------
# Model per symbol
# ------------------------------------------------------------------
cat("\nIndividual models per symbol:\n")
for (sym in fit$symbols) {
  model <- fit$models[[sym]]
  cat(sprintf("  %s: %d trees, %d observations\n",
              sym, model$best_iter, fit$symbol_info[[sym]]$n))
}

# ------------------------------------------------------------------
# Compare predictions across symbols
# ------------------------------------------------------------------
cat("\nPrediction comparison:\n")
for (sym in fit$symbols) {
  sym_idx <- which(test_df$symbol == sym)
  if (length(sym_idx) > 0) {
    sym_preds <- preds1[sym_idx]
    cat(sprintf("  %s: mean=%.3f, sd=%.3f, n=%d\n",
                sym, mean(sym_preds), sd(sym_preds), length(sym_idx)))
  }
}

# ------------------------------------------------------------------
# Custom multi parameter example
# ------------------------------------------------------------------
cat("\n\nCustom multi parameter example:\n")

# Create data with a different grouping column
df2 <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  group = sample(c("GroupA", "GroupB", "GroupC"), 200, replace = TRUE)
)
df2$y <- df2$x1 * 0.5 + rnorm(200)

# Train with custom multi parameter
fit2 <- mqbm(
  y ~ x1 + x2,
  data = df2,
  multi = "group",  # Use "group" instead of default "symbol"
  tau = 0.5,
  nrounds = 50,
  nfolds = 3
)

cat("Multi parameter used:", fit2$multi, "\n")
cat("Groups:", paste(fit2$symbols, collapse = ", "), "\n")

# Predictions work with the group column
newdata <- data.frame(
  x1 = rnorm(50),
  x2 = rnorm(50),
  group = sample(c("GroupA", "GroupB", "GroupC"), 50, replace = TRUE)
)
preds_custom <- predict(fit2, newdata)
cat("Custom predictions generated:", length(preds_custom), "\n")

cat("\nDemo complete!\n")
