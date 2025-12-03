# Quick verification that mqtail works with train/val indices
devtools::load_all()

set.seed(123)

cat("=== Quick mqtail train/val verification ===\n\n")

# Small test
df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100),
  symbol = sample(c("A", "B"), 100, replace = TRUE)
)
df$y <- df$x1 + rt(100, df = 3)

train_idx <- 1:70
val_idx <- 71:100

cat("Fitting mqtail with train/val split...\n")
fit <- mqtail(
  y ~ x1 + x2,
  data = df,
  taus = c(0.95, 0.99),
  threshold_tau = 0.99,
  params = list(nrounds = 50, nfolds = 3),
  train_idx = train_idx,
  val_idx = val_idx,
  verbose = TRUE
)

cat("\n✓ SUCCESS! mqtail with train/val indices works\n")
print(fit)
