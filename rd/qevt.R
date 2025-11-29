devtools::load_all()

set.seed(123)

# Generate data with heavy tails
n <- 10000

df <- tibble::tibble(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = runif(n, -2, 2),
  y = 2 * x1 - 0.5 * x2 + 0.3 * x3 + 2 * rt(n, df = 3)
)

# Train/test split
train_idx <- seq_len(ceiling(.7 * n))
test_idx <- seq(max(train_idx) + 1, n)

# Fit EVT model
fit <- qevt(
  y ~ .,
  data = df,
  tau_target = 0.995,
  taus = c(0.95, 0.98, 0.99)
)

# Model summary and diagnostics
summary(fit)

plot(fit)

# Predictions
preds <- predict(fit, head(df, 10))

# Check monotonicity
stopifnot(all(apply(preds$monotone, 1, function(v) all(diff(v) >= 0))))
