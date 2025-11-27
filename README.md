# qboost: Quantile Gradient Boosting Models in R

[![R](https://img.shields.io/badge/R-%3E%3D4.1.0-blue.svg)](https://www.r-project.org/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

## Overview

`qboost` provides a high-level interface for quantile regression using LightGBM. It offers:

- Clean, `lm`-style formula interface or `x`/`y` input
- Automatic cross-validation and best-iteration selection
- Rich diagnostics including calibration analysis
- S3 methods for `predict`, `plot`, `summary`, `coef`, `importance`
- Publication-ready plots
- Tail modeling with `qtail()` for extreme value analysis
- Event detection with `qevt()` for anomaly detection

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("cassiopagnoncelli/qboost")
```

## Quick Start

### Basic Quantile Regression

```r
library(qboost)

# Generate example data
set.seed(1)
n <- 200
df <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n)
)
df$y <- 0.5 * df$x1 - 0.2 * df$x2 + rnorm(n, sd = 0.8)

# Fit quantile regression model
fit <- qboost(
  y ~ x1 + x2,
  data = df,
  tau = 0.3,
  nrounds = 200,
  nfolds = 5
)

# Make predictions
newdata <- head(df[, c("x1", "x2")], 5)
predict(fit, newdata)

# View summary
summary(fit)

# Plot diagnostics
plot(fit)

# Feature importance
importance(fit)
```

### Tail Modeling

```r
# Fit tail model for extreme values
tail_fit <- qtail(
  y ~ x1 + x2,
  data = df,
  u = 0.9,  # threshold quantile
  nrounds = 200
)

# Predict extreme quantiles
predict(tail_fit, newdata, at = c(0.95, 0.99))

# Validate calibration
qtail_validate(tail_fit)
```

## Key Features

### Quantile Regression (`qboost`)

- Flexible input: formula or `x`/`y` pairs
- Automatic hyperparameter tuning via cross-validation
- Calibration metrics (coverage, QCE, pseudo-R²)
- Pinball loss visualization
- Feature importance with entropy-based stability scores

### Tail Modeling (`qtail`)

- Peaks-over-threshold approach for extreme values
- GPD (Generalized Pareto Distribution) tail modeling
- Calibration curve analysis
- PIT (Probability Integral Transform) diagnostics
- Stability assessment across bootstrap samples

### Event Detection (`qevt`)

- Quantile-based anomaly detection
- Dynamic threshold selection
- Event summary statistics
- Visual diagnostics

## Dependencies

This package depends on:

- `lightgbm`: Fast gradient boosting implementation
- `evgam`: Extreme value modeling
- `ggplot2`, `dplyr`, `tibble`: Data manipulation and visualization
- `glmnet`: Elastic net regularization

## Documentation

For detailed documentation and examples, see the vignettes:

```r
vignette("qboost-intro", package = "qboost")
vignette("qtail-intro", package = "qboost")
```

## License

MIT License. See LICENSE file for details.

## Citation

If you use this package in your research, please cite:

```
Pagnoncelli, C. (2025). qboost: Quantile Gradient Boosting Models in R. 
R package version 0.4.7.
```

## Author

Cassio Pagnoncelli  
Email: cassiopagnoncelli@gmail.com  
ORCID: [0009-0000-7114-7008](https://orcid.org/0009-0000-7114-7008)
