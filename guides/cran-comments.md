## Test environments

* local macOS install, R 4.4.0
* GitHub Actions (ubuntu-latest): R-release
* GitHub Actions (windows-latest): R-release
* GitHub Actions (macOS-latest): R-release

## R CMD check results

0 errors | 0 warnings | 0 notes

## Submission notes

This is the initial CRAN submission of the qboost package.

The package provides high-level quantile regression using LightGBM with:
- Clean formula-based interface similar to lm()
- Automatic cross-validation and hyperparameter tuning
- Comprehensive diagnostics and calibration metrics
- Tail modeling for extreme value analysis
- Publication-ready visualizations

All dependencies (lightgbm, evgam, ggplot2, dplyr, tibble, glmnet) are available on CRAN.

## Downstream dependencies

There are currently no downstream dependencies for this package.
