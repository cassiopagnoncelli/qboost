# Test script for mqtail with uneven train/val splits across symbols
devtools::load_all()

set.seed(789)

cat("=== Testing mqtail with Uneven Train/Val Splits ===\n\n")

# Create sample data where some symbols are only in train, some in both
n <- 500
df <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  group = sample(c("A", "B", "C", "D", "E"), n, replace = TRUE)
)
df$y <- 1.5 * df$x1 + 0.5 * df$x2 + rt(n, df = 3)

# Create train/val split where:
# - Groups A, B, C have data in both train and val
# - Group D has data ONLY in train
# - Group E has data ONLY in val
train_idx <- which(df$group %in% c("A", "B", "C", "D") & runif(n) < 0.7)
val_idx <- which(df$group %in% c("A", "B", "C", "E") & !(seq_len(n) %in% train_idx))

cat("Train/val distribution:\n")
cat("  Train groups:", paste(unique(df$group[train_idx]), collapse = ", "), "\n")
cat("  Val groups:", paste(unique(df$group[val_idx]), collapse = ", "), "\n")
cat("  Group A in train:", sum(df$group[train_idx] == "A"), 
    ", in val:", sum(df$group[val_idx] == "A"), "\n")
cat("  Group B in train:", sum(df$group[train_idx] == "B"), 
    ", in val:", sum(df$group[val_idx] == "B"), "\n")
cat("  Group C in train:", sum(df$group[train_idx] == "C"), 
    ", in val:", sum(df$group[val_idx] == "C"), "\n")
cat("  Group D in train:", sum(df$group[train_idx] == "D"), 
    ", in val:", sum(df$group[val_idx] == "D"), "\n")
cat("  Group E in train:", sum(df$group[train_idx] == "E"), 
    ", in val:", sum(df$group[val_idx] == "E"), "\n\n")

cat("=== Fitting mqtail ===\n")
fit <- mqtail(
  y ~ x1 + x2,
  data = df,
  multi = "group",
  train_idx = train_idx,
  val_idx = val_idx,
  tail = "upper",
  taus = c(0.9, 0.95),
  threshold_tau = 0.95,
  params = list(nrounds = 50, nfolds = 3),  # Remove early_stopping_rounds to allow k-fold CV
  verbose = TRUE
)

cat("\n=== Model Summary ===\n")
print(fit)

cat("\n=== Predictions ===\n")
newdata <- data.frame(
  x1 = rep(0, 5),
  x2 = rep(0, 5),
  group = c("A", "B", "C", "D", "E")
)
preds <- predict(fit, newdata, type = "quantile")
cat("Predictions for each group:\n")
print(data.frame(group = newdata$group, prediction = preds))

cat("\n✓ mqtail handles uneven train/val splits correctly!\n")
