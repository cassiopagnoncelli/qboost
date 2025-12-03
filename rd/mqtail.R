# Demo: Symbol-based Extreme Tail Quantile Model (mqtail)

devtools::load_all()

# Create sample data with multiple symbols and heavy-tailed distributions
set.seed(123)
n <- 5000
df <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = runif(n, -2, 2),
  symbol = sample(c("AAPL", "GOOGL", "MSFT"), n, replace = TRUE)
)

# Each symbol has different tail behavior (using t-distributions with different df)
df$y <- ifelse(df$symbol == "AAPL",
               2 * df$x1 - 0.5 * df$x2 + 0.3 * df$x3 + 2 * rt(n, df = 3),  # heavier tails
               ifelse(df$symbol == "GOOGL",
                      1.5 * df$x1 + 1.0 * df$x2 - 0.2 * df$x3 + 1.5 * rt(n, df = 5),  # moderate tails
                      1.0 * df$x1 - 1.0 * df$x2 + 0.5 * df$x3 + 1.0 * rt(n, df = 7)))  # lighter tails

# Split data
train_idx <- 1:floor(0.7 * n)
val_idx <- (max(train_idx) + 1):floor(0.85 * n)
test_idx <- (max(val_idx) + 1):n

# ------------------------------------------------------------------
# Train mqtail model (formula interface)
# ------------------------------------------------------------------
cat("Training mqtail model...\n")
cat("Note: mqtail fits one qtail model per symbol\n")

fit <- mqtail(
  y ~ x1 + x2 + x3,
  data = df,
  train_idx = train_idx,
  val_idx = val_idx,
  tail = "upper",
  taus = c(0.95, 0.98, 0.99, 0.995),
  threshold_tau = 0.98,
  params = list(
    nrounds = 100,
    nfolds = 3
  ),
  verbose = TRUE
)

# Print model summary
print(fit)
summary(fit)

# ------------------------------------------------------------------
# Predictions for extreme quantiles
# ------------------------------------------------------------------
cat("\n\nMaking predictions for extreme quantile (tau = 0.993)...\n")

# Predict extreme quantile using EVT extrapolation
tau_extreme <- 0.993
preds <- predict(fit, df[test_idx, ], tau = tau_extreme)

cat("Predictions generated:", length(preds), "\n")
cat("Prediction range:", range(preds), "\n")

# ------------------------------------------------------------------
# Predictions per symbol
# ------------------------------------------------------------------
cat("\nPrediction statistics per symbol:\n")
test_df <- df[test_idx, ]
for (sym in fit$symbols) {
  sym_idx <- which(test_df$symbol == sym)
  if (length(sym_idx) > 0) {
    sym_preds <- preds[sym_idx]
    sym_actuals <- test_df$y[sym_idx]
    
    # Calculate exceedances
    n_exceed <- sum(sym_actuals > sym_preds)
    exceed_rate <- n_exceed / length(sym_idx)
    
    cat(sprintf("  %s: mean_pred=%.3f, n=%d, exceedances=%d (%.1f%%)\n",
                sym, mean(sym_preds), length(sym_idx), n_exceed, exceed_rate * 100))
  }
}

# ------------------------------------------------------------------
# Compare tail behavior across symbols
# ------------------------------------------------------------------
cat("\n\nComparing tail models across symbols:\n")
for (sym in fit$symbols) {
  model <- fit$models[[sym]]
  cat(sprintf("  %s: threshold_tau=%.3f, tau_target=%.4f, tail=%s\n",
              sym, model$threshold_tau, model$tau_target, model$tail))
  
  # Show EVT parameters (shape parameter)
  if (!is.null(model$evt_fit)) {
    cat(sprintf("    EVT shape parameter (xi): %.4f\n", model$evt_fit$xi))
  }
}

# ------------------------------------------------------------------
# Fitted values on training data
# ------------------------------------------------------------------
cat("\nFitted values (threshold quantile on training data):\n")
fitted_vals <- fitted(fit)
cat("Length:", length(fitted_vals), "\n")
cat("Range:", range(fitted_vals), "\n")

# Fitted values per symbol
for (sym in fit$symbols) {
  sym_fitted <- fitted_vals[df$symbol[train_idx] == sym]
  cat(sprintf("  %s: mean=%.3f, sd=%.3f\n", sym, mean(sym_fitted), sd(sym_fitted)))
}

# ------------------------------------------------------------------
# Custom multi parameter example
# ------------------------------------------------------------------
cat("\n\nCustom multi parameter example:\n")

# Create data with a different grouping column
df2 <- data.frame(
  x1 = rnorm(1000),
  x2 = rnorm(1000),
  x3 = runif(1000, -2, 2),
  region = sample(c("North", "South", "East", "West"), 1000, replace = TRUE)
)

# Different tail behavior per region
df2$y <- ifelse(df2$region == "North",
                df2$x1 + rt(1000, df = 4),
                ifelse(df2$region == "South",
                       df2$x2 + rt(1000, df = 4),
                       ifelse(df2$region == "East",
                              df2$x3 + rt(1000, df = 4),
                              df2$x1 + df2$x2 + rt(1000, df = 4))))

# Train with custom multi parameter
fit2 <- mqtail(
  y ~ x1 + x2 + x3,
  data = df2,
  multi = "region",  # Use "region" instead of default "symbol"
  tail = "upper",
  taus = c(0.95, 0.98, 0.99),
  threshold_tau = 0.98,
  params = list(nrounds = 50, nfolds = 3),
  verbose = TRUE
)

cat("Multi parameter used:", fit2$multi, "\n")
cat("Regions:", paste(fit2$symbols, collapse = ", "), "\n")

# Predictions work with the region column
newdata <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  x3 = runif(100, -2, 2),
  region = sample(c("North", "South", "East", "West"), 100, replace = TRUE)
)
preds_custom <- predict(fit2, newdata, tau = 0.99)
cat("Custom predictions generated:", length(preds_custom), "\n")

cat("\nDemo complete!\n")
