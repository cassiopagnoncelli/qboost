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

# Predictions
tau = 0.993
q <- quantile(predict(fit, newdata = df[train_idx, ], tau = tau), tau)

preds <- predict(fit, newdata = df[train_idx, ], tau = tau) # EVT extrapolation
sum(preds_extreme > preds_993q)
