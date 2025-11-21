# Example use case script for qboost package
devtools::load_all()

library(qboost)

set.seed(123)
n <- 200

# create 30 predictors
x <- as.data.frame(
  replicate(30, rnorm(n))
)
names(x) <- paste0("x", seq_len(ncol(x)))

# define signal on a few predictors
y <- 0.7 * x$x1 - 0.3 * x$x2 + 0.2 * x$x3 + 0.1 * x$x10 + rnorm(n, sd = 0.7)

# Fit a quantile model
fit <- qboost(
  x = x,
  y = y,
  tau = 0.25,
  nrounds = 200,
  nfolds = 5,
  early_stopping_rounds = 20
)

# Print summary (compact)
summary(fit)

# Print detailed summary
summary(fit, detailed = TRUE)

# Plot diagnostics (returns list of ggplots)
plots <- plot(fit, plot = FALSE, top_features = 4)
print(plots$pinball)
print(plots$qq)
print(plots$calibration)
print(plots$importance)

# Predict on new data and include observed y for new-data summary
newx <- head(x, 20)
newy <- head(y, 20)
summary(fit, detailed = TRUE, newdata = newx, y_new = newy, top_features = 5)
