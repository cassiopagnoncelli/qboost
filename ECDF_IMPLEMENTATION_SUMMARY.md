# ECDF Implementation for mqbm

## Summary

Successfully adapted `mqbm` to yield `ecdf(surface)` instead of raw quantile predictions. Each symbol now has its own ECDF function built from training y values, and predictions are transformed to probabilities.

## Changes Made

### 1. R/mqbm.R
- Added `ecdf_funs` list to store symbol-specific ECDF functions
- Each symbol's ECDF is built using `stats::ecdf()` from its training y values
- Updated object structure to include `ecdf_funs` component
- Updated documentation to reflect ECDF transformation

### 2. R/predict.mqbm.R
- Modified prediction logic to transform raw predictions through symbol-specific ECDF
- Raw predictions from qbm models are now converted to probabilities (0-1 range)
- Updated documentation to describe probability output instead of raw quantile values

### 3. R/fitted.mqbm.R
- Updated to return ECDF-transformed fitted values (probabilities)
- Raw fitted values from internal models are transformed through symbol-specific ECDF
- Updated documentation to clarify that probabilities are returned

### 4. R/residuals.mqbm.R
- No changes needed - correctly uses raw fitted values from internal models
- Residuals are computed as y - raw_fitted (not ECDF-transformed)

### 5. tests/testthat/test-residuals.R
- Updated test expectations to account for ECDF-transformed fitted values
- Tests now correctly extract raw fitted values from internal models for residual validation

## Key Design Decisions

1. **Symbol-specific ECDFs**: Each symbol maintains its own ECDF function built from its training y values, ensuring proper probability calibration per group.

2. **Raw residuals**: Residuals continue to use raw fitted values (not ECDF-transformed) since they represent actual prediction errors in the original scale.

3. **Backward compatibility**: The changes maintain the existing API - all function signatures remain unchanged.

4. **Probability output**: Both `predict()` and `fitted()` now return probabilities in [0, 1] range, representing the empirical percentile of predictions within each symbol's training distribution.

## Usage Example

```r
library(qboost)

# Create data with multiple symbols
df <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  symbol = sample(c("A", "B"), 200, replace = TRUE)
)
df$y <- df$x1 * 0.5 + rnorm(200)

# Fit mqbm model
fit <- mqbm(y ~ x1 + x2, data = df, tau = 0.5, nrounds = 50)

# Check ECDF functions are stored
class(fit$ecdf_funs$A)  # "ecdf" "stepfun" "function"

# Get predictions (now probabilities via ECDF)
newdata <- data.frame(
  x1 = rnorm(20),
  x2 = rnorm(20),
  symbol = sample(c("A", "B"), 20, replace = TRUE)
)
predictions <- predict(fit, newdata)
range(predictions)  # Values between 0 and 1

# Get fitted values (also probabilities via ECDF)
fitted_probs <- fitted(fit)
range(fitted_probs)  # Values between 0 and 1
```

## Test Results

All 213 tests pass successfully:
- 80 tests for mqbm functionality
- 14 tests for qbm functionality  
- 32 tests for qtail-phase3
- 69 tests for qtail
- 18 tests for residuals

## Benefits

1. **Probability calibration**: Predictions are automatically calibrated to the training distribution of each symbol
2. **Interpretability**: Output probabilities are easier to interpret than raw quantile predictions
3. **Group-specific**: Each symbol has its own ECDF, accounting for different distributions across groups
4. **Consistency**: The transformation is applied consistently to both training (fitted) and test (predict) data
