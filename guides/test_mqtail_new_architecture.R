# Test script for new mqtail architecture
devtools::load_all()

set.seed(123)

cat("=== Testing New mqtail Architecture ===\n\n")

# Create sample data
n <- 200
df <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  symbol = sample(c("A", "B"), n, replace = TRUE)
)
df$y <- 2 * df$x1 + 0.5 * df$x2 + rt(n, df = 3)

cat("Data: ", nrow(df), " rows, ", ncol(df) - 2, " features, ", 
    length(unique(df$symbol)), " symbols\n\n", sep = "")

# Fit mqtail model with new architecture
cat("=== Fitting mqtail ===\n")
fit <- mqtail(
  y ~ x1 + x2,
  data = df,
  tail = "upper",
  taus = c(0.95, 0.99),
  threshold_tau = 0.99,
  params = list(nrounds = 50, nfolds = 2),
  verbose = TRUE
)

cat("\n=== Model Structure ===\n")
cat("Class:", class(fit), "\n")
cat("Symbols:", paste(fit$symbols, collapse = ", "), "\n")
cat("Taus:", paste(fit$taus, collapse = ", "), "\n")
cat("Has mqbm_models:", !is.null(fit$mqbm_models), "\n")
cat("Number of mqbm models:", length(fit$mqbm_models), "\n")
cat("Has evt_models:", !is.null(fit$evt_models), "\n")

cat("\n=== Fitted Values ===\n")
fitted_surface <- fitted(fit, type = "surface")
cat("Surface range:", range(fitted_surface), "\n")
cat("Surface mean:", mean(fitted_surface), "\n")

fitted_quantile <- fitted(fit, type = "quantile")
cat("Quantile range:", range(fitted_quantile), "\n")
cat("Quantile mean:", mean(fitted_quantile), "\n")
cat("All in [0,1]?", all(fitted_quantile >= 0 & fitted_quantile <= 1), "\n")

cat("\n=== Predictions ===\n")
newdata <- data.frame(
  x1 = c(0, 0),
  x2 = c(0, 0),
  symbol = c("A", "B")
)

preds_surface <- predict(fit, newdata, type = "surface")
preds_quantile <- predict(fit, newdata, type = "quantile")

cat("Surface predictions:", preds_surface, "\n")
cat("Quantile predictions:", preds_quantile, "\n")
cat("Quantile all in [0,1]?", all(preds_quantile >= 0 & preds_quantile <= 1), "\n")

cat("\n✓ New mqtail architecture works!\n")
