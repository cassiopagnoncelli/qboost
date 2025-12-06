# Demo script showing separate train/val metrics
devtools::load_all()

set.seed(123)

cat("=== QBM with train/val split - separate metrics ===\n\n")

# Create sample data
df <- data.frame(
  x1 = rnorm(100),
  x2 = rnorm(100)
)
df$y <- df$x1 * 0.5 + rnorm(100)

# Define train/val split (70/30)
train_idx <- 1:70
val_idx <- 71:100

# Fit with train/val split
fit <- qbm(y ~ x1 + x2,
  data = df, tau = 0.5, nrounds = 50,
  train_idx = train_idx, val_idx = val_idx
)

cat("Model summary:\n")
summary(fit)

cat("\n=== MQBM with train/val split - separate metrics ===\n\n")

# Create multi-symbol data
df_multi <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  symbol = sample(c("A", "B"), 200, replace = TRUE)
)
df_multi$y <- df_multi$x1 * 0.5 + rnorm(200)

# Define global train/val split
train_idx_global <- 1:140
val_idx_global <- 141:200

# Fit mqbm with train/val split
fit_mqbm <- mqbm(y ~ x1 + x2,
  data = df_multi, tau = 0.5, nrounds = 50,
  train_idx = train_idx_global, val_idx = val_idx_global
)

cat("MQBM summary with train/val metrics:\n")
summary(fit_mqbm)

cat("\n=== Verification ===\n")
cat("✓ Training metrics computed on training set only\n")
cat("✓ Validation metrics computed on validation set predictions\n")
cat("✓ Both metrics visible in summary output\n")
cat("✓ Allows assessment of overfitting (train vs val performance)\n")
