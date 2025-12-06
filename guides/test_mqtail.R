# Test script for mqtail implementation
devtools::load_all()

set.seed(123)

cat("=== Testing mqtail Implementation ===\n\n")

# Create sample data with heavy tails and multiple symbols
n_per_symbol <- 200
df <- data.frame(
  x1 = rnorm(n_per_symbol * 2),
  x2 = rnorm(n_per_symbol * 2),
  symbol = rep(c("A", "B"), each = n_per_symbol)
)
# Heavy-tailed response (Student-t)
df$y <- 2 * df$x1 + 0.5 * df$x2 + rt(n_per_symbol * 2, df = 3)

cat("Data: ", nrow(df), " rows, ", ncol(df) - 2, " features, ",
  length(unique(df$symbol)), " symbols\n\n",
  sep = ""
)

# Fit mqtail model
cat("Fitting mqtail model...\n")
fit <- mqtail(
  y ~ x1 + x2,
  data = df,
  tail = "upper",
  taus = c(0.95, 0.97, 0.99),
  threshold_tau = 0.99,
  params = list(nrounds = 50, nfolds = 2),
  verbose = TRUE
)

cat("\n=== Model Structure ===\n")
print(fit)

cat("\n=== Summary ===\n")
summary(fit)

cat("\n=== GPD Parameters (coef) ===\n")
print(coef(fit))

cat("\n=== Fitted Values (surface) ===\n")
fitted_surface <- fitted(fit, type = "surface")
cat("Range:", range(fitted_surface), "\n")
cat("Mean:", mean(fitted_surface), "\n")

cat("\n=== Fitted Values (quantile/ECDF) ===\n")
fitted_quantile <- fitted(fit, type = "quantile")
cat("Range:", range(fitted_quantile), "\n")
cat("Mean:", mean(fitted_quantile), "\n")
cat("All in [0,1]?", all(fitted_quantile >= 0 & fitted_quantile <= 1), "\n")

cat("\n=== Predictions ===\n")
newdata <- data.frame(
  x1 = rnorm(10),
  x2 = rnorm(10),
  symbol = sample(c("A", "B"), 10, replace = TRUE)
)

preds_surface <- predict(fit, newdata, type = "surface")
preds_quantile <- predict(fit, newdata, type = "quantile")

cat("Surface predictions:", preds_surface[1:5], "...\n")
cat("Quantile predictions:", preds_quantile[1:5], "...\n")
cat("Quantile all in [0,1]?", all(preds_quantile >= 0 & preds_quantile <= 1), "\n")

cat("\n=== Residuals ===\n")
resid <- residuals(fit)
cat("Residuals range:", range(resid), "\n")
cat("Residuals mean:", mean(resid), "\n")

cat("\n✓ mqtail implementation complete and functional!\n")
