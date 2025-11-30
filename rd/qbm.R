devtools::load_all()

set.seed(123)

n <- 10000

df <- tibble::tibble(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = runif(n, -2, 2),
  y = 2 * x1 - 0.5 * x2^2 + 0.3 * x3 + 2 * rt(n, df = 3)
)

fit <- qbm(
  y ~ .,
  data = df,
  tau = 0.94,
  nrounds = 600,
  nfolds = 5,
  early_stopping_rounds = 50
)

pred <- predict(fit, head(df, 10))

summary(fit)

plot(fit)
