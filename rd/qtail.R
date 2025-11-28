# Load package
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

# Fit qtail model for upper tail
fit <- qtail(
  y ~ .,
  data = df[train_idx, ],
  tail = "upper",
  taus = c(0.95, 0.98, 0.99, 0.995),
  threshold_tau = 0.98,
  params = list(nrounds = 100, nfolds = 5),
  verbose = TRUE
)

# Model summary and diagnostics
summary(fit)

plot(fit)

# Predictions at various quantiles
preds_95 <- predict(fit, newdata = df[test_idx, ], tau = 0.95)
preds_99 <- predict(fit, newdata = df[test_idx, ], tau = 0.99)
preds_extreme <- predict(fit, newdata = df[test_idx, ], tau = 0.9993) # EVT extrapolation

# Evaluate coverage
cat("\nTest Set Coverage:\n")
cat("tau=0.95 :", mean(df$y[test_idx] <= preds_95), "\n")
cat("tau=0.99 :", mean(df$y[test_idx] <= preds_99), "\n")

# Tail parameters
cat("\nGPD tail parameters (xi, beta):", fit$evt$xi, fit$evt$beta, "\n")
cat("Number of exceedances:", fit$evt$n_exceedances, "\n")
