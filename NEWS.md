# qboost 0.4.7

## New Features

* Initial CRAN submission
* Core quantile regression functionality with `qboost()`
* Tail modeling with `qtail()` for extreme value analysis
* Event detection with `qevt()` for anomaly detection
* Comprehensive S3 methods (predict, plot, summary, coef, importance)
* Rich diagnostics and calibration metrics
* Publication-ready visualizations

## Key Functions

### qboost
* Formula and x/y interfaces for quantile regression
* Automatic cross-validation and hyperparameter tuning
* Calibration analysis (coverage, QCE, pseudo-R²)
* Feature importance with entropy-based stability

### qtail
* Peaks-over-threshold modeling for extreme values
* GPD tail modeling via evgam
* Calibration curve analysis
* PIT diagnostics and stability assessment

### qevt
* Quantile-based anomaly detection
* Dynamic threshold selection
* Event summary and diagnostics

## Documentation

* Comprehensive vignettes for qboost and qtail
* Complete function documentation with examples
* Test coverage with testthat
