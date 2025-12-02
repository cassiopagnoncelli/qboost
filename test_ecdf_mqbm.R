# Test script to demonstrate mqbm ECDF transformation
devtools::load_all()

set.seed(123)

# Create sample data with two symbols
df <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  symbol = sample(c("A", "B"), 200, replace = TRUE)
)
df$y <- df$x1 * 0.5 + rnorm(200)

# Fit mqbm model
cat("Fitting mqbm model...\n")
fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50, nfolds = 2)

# Check that ecdf_funs are stored
cat("\nChecking ECDF functions are stored:\n")
cat("ECDF function for symbol A:", class(fit$ecdf_funs$A), "\n")
cat("ECDF function for symbol B:", class(fit$ecdf_funs$B), "\n")

# Get fitted values (should be probabilities between 0 and 1)
fitted_vals <- fitted(fit)
cat("\nFitted values (ECDF-transformed):\n")
cat("Range:", range(fitted_vals), "\n")
cat("Mean:", mean(fitted_vals), "\n")
cat("First 10 values:", head(fitted_vals, 10), "\n")

# Make predictions on new data
newdata <- data.frame(
  x1 = rnorm(20),
  x2 = rnorm(20),
  symbol = sample(c("A", "B"), 20, replace = TRUE)
)

predictions <- predict(fit, newdata)
cat("\nPredictions (ECDF-transformed):\n")
cat("Range:", range(predictions), "\n")
cat("Mean:", mean(predictions), "\n")
cat("All values:", predictions, "\n")

# Verify predictions are probabilities (between 0 and 1)
cat("\nVerification:\n")
cat("All predictions between 0 and 1?", all(predictions >= 0 & predictions <= 1), "\n")
cat("All fitted values between 0 and 1?", all(fitted_vals >= 0 & fitted_vals <= 1), "\n")

cat("\nSuccess! mqbm now returns ECDF(surface) as probabilities.\n")
