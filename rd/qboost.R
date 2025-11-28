devtools::load_all()

set.seed(1)

# Example data

n <- 500
df <- data.frame(
  x1 = rnorm(n),
  x2 = runif(n),
  x3 = rnorm(n)
)

df$y <- 0.7 * sin(df$x1) - 0.3 * df$x2 + 0.4 * df$x3^2 + rnorm(n, sd = 0.8)

head(df)

# Fit a non-linear quantile model
fit <- qboost(
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
