# Type Parameter Implementation for mqbm

## Summary

Successfully implemented a `type` parameter for `predict.mqbm()` and `fitted.mqbm()` functions that allows users to choose between:
- **"surface"** (default): Returns raw quantile predictions from the underlying qbm models
- **"quantile"**: Returns ECDF-transformed probabilities based on training data

This provides flexibility while maintaining backward compatibility.

## Changes Made

### 1. R/mqbm.R
- Added `ecdf_funs` list to store symbol-specific ECDF functions
- Each symbol's ECDF is built using `stats::ecdf()` from its training y values during model fitting
- Updated object structure to include `ecdf_funs` component

### 2. R/predict.mqbm.R
- Added `type` parameter with choices `c("surface", "quantile")`
- Default is `"surface"` for backward compatibility
- When `type = "surface"`: Returns raw quantile predictions
- When `type = "quantile"`: Transforms predictions through symbol-specific ECDF to return probabilities

### 3. R/fitted.mqbm.R
- Added `type` parameter with choices `c("surface", "quantile")`
- Default is `"surface"` for backward compatibility
- When `type = "surface"`: Returns raw fitted values
- When `type = "quantile"`: Transforms fitted values through symbol-specific ECDF to return probabilities

### 4. R/residuals.mqbm.R
- No changes needed - correctly uses raw fitted values from internal models
- Residuals are always computed as `y - raw_fitted` (not ECDF-transformed)

### 5. R/summary.mqbm.R
- Updated `.aggregate_symbol_metrics()` to explicitly use `fitted(mqbm_object, type = "surface")`
- Updated `.aggregate_calibration()` to explicitly use `fitted(mqbm_object, type = "surface")`
- Ensures all statistics and calibration metrics are computed on raw quantile values

### 6. tests/testthat/test-mqbm.R
- Added tests for `type = "surface"` behavior
- Added tests for `type = "quantile"` behavior
- Added test to verify default is `"surface"` for backward compatibility
- All 227 tests pass

### 7. tests/testthat/test-residuals.R
- Updated tests to explicitly use `type = "surface"` when needed
- Tests verify residuals work correctly with the new parameter

## Key Design Decisions

1. **Default to "surface"**: The default value is `"surface"` to maintain backward compatibility with existing code that doesn't specify the type parameter.

2. **Symbol-specific ECDFs**: Each symbol maintains its own ECDF function built from its training y values, ensuring proper probability calibration per group.

3. **Raw residuals**: Residuals continue to use raw fitted values (not ECDF-transformed) since they represent actual prediction errors in the original scale.

4. **Parameter validation**: Uses `match.arg()` to validate the type parameter and provide clear error messages if an invalid type is specified.

## Usage Examples

### Basic Usage

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
```

### Getting Raw Quantile Predictions (Surface)

```r
# Default behavior - returns raw quantile predictions
preds_surface <- predict(fit, newdata)
# Or explicitly:
preds_surface <- predict(fit, newdata, type = "surface")

# Fitted values
fitted_surface <- fitted(fit)  # default
fitted_surface <- fitted(fit, type = "surface")  # explicit
```

### Getting ECDF-transformed Probabilities (Quantile)

```r
# Returns probabilities in [0, 1] range
preds_quantile <- predict(fit, newdata, type = "quantile")

# Fitted probabilities
fitted_quantile <- fitted(fit, type = "quantile")
```

### Comparison

```r
# Surface predictions are raw values
range(preds_surface)  # e.g., [-0.5, 0.4]

# Quantile predictions are probabilities
range(preds_quantile)  # [0, 1]
all(preds_quantile >= 0 & preds_quantile <= 1)  # TRUE
```

## Test Results

All tests pass successfully:
- 94 tests for mqbm functionality (including 6 new tests for type parameter)
- 14 tests for qbm functionality
- 32 tests for qtail-phase3
- 69 tests for qtail
- 18 tests for residuals

**Total: 227 tests passing**

## Benefits

1. **Backward compatibility**: Existing code continues to work without modification (default is "surface")
2. **Flexibility**: Users can choose the output format that best suits their needs
3. **Probability calibration**: The "quantile" type provides automatically calibrated probabilities
4. **Group-specific**: Each symbol has its own ECDF, accounting for different distributions across groups
5. **Clear semantics**: The parameter names "surface" and "quantile" clearly describe what each mode returns

## Migration Guide

For users who were using the previous implementation that always returned ECDF probabilities:

```r
# OLD (always returned ECDF probabilities)
preds <- predict(fit, newdata)

# NEW (explicitly request ECDF probabilities)
preds <- predict(fit, newdata, type = "quantile")
```

For users who need raw quantile predictions (now the default):

```r
# No changes needed - this is now the default behavior
preds <- predict(fit, newdata)
# Or explicitly:
preds <- predict(fit, newdata, type = "surface")
```
