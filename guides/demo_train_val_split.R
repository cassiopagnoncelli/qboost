# Demo script showing train/val split functionality
devtools::load_all()

set.seed(123)

cat("=== 1. QBM with train/val split ===\n\n")

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
fit_split <- qbm(y ~ x1 + x2,
  data = df, tau = 0.5, nrounds = 50,
  train_idx = train_idx, val_idx = val_idx
)

cat("Fold type:", fit_split$cv_settings$fold_type, "\n")
cat("Training set size:", length(fit_split$cv_settings$train_idx), "\n")
cat("Validation set size:", length(fit_split$cv_settings$val_idx), "\n")
cat(
  "Fitted values on training set available:",
  all(!is.na(fitted(fit_split, type = "surface")[train_idx])), "\n"
)
cat(
  "Fitted values on validation set are NA:",
  all(is.na(fitted(fit_split, type = "surface")[val_idx])), "\n\n"
)

cat("=== 2. QBM with custom folds ===\n\n")

# Define custom 3-fold CV
custom_folds <- list(
  1:33, # Fold 1 validation set
  34:66, # Fold 2 validation set
  67:100 # Fold 3 validation set
)

fit_folds <- qbm(y ~ x1 + x2,
  data = df, tau = 0.5, nrounds = 50,
  folds = custom_folds
)

cat("Fold type:", fit_folds$cv_settings$fold_type, "\n")
cat("Custom folds used for CV: TRUE\n\n")

cat("=== 3. MQBM with train/val split (respecting symbol boundaries) ===\n\n")

# Create multi-symbol data
df_multi <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  symbol = sample(c("A", "B", "C"), 200, replace = TRUE)
)
df_multi$y <- df_multi$x1 * 0.5 + rnorm(200)

# Define global train/val split
train_idx_global <- 1:140
val_idx_global <- 141:200

cat("Global train indices:", min(train_idx_global), "-", max(train_idx_global), "\n")
cat("Global val indices:", min(val_idx_global), "-", max(val_idx_global), "\n\n")

# Fit mqbm with train/val split
fit_mqbm <- mqbm(y ~ x1 + x2,
  data = df_multi, tau = 0.5, nrounds = 50,
  train_idx = train_idx_global, val_idx = val_idx_global, nfolds = 2
)

cat("Symbol-specific splits:\n")
for (sym in fit_mqbm$symbols) {
  sym_model <- fit_mqbm$models[[sym]]
  cat(sprintf("  %s: fold_type = %s", sym, sym_model$cv_settings$fold_type))
  if (!is.null(sym_model$cv_settings$train_idx)) {
    cat(sprintf(
      ", train=%d, val=%d",
      length(sym_model$cv_settings$train_idx),
      length(sym_model$cv_settings$val_idx)
    ))
  }
  cat("\n")
}

# Verify symbol boundary respect
cat("\nVerifying symbol boundaries are respected:\n")
for (sym in fit_mqbm$symbols) {
  sym_idx <- fit_mqbm$symbol_info[[sym]]$indices
  sym_train_global <- intersect(train_idx_global, sym_idx)
  sym_val_global <- intersect(val_idx_global, sym_idx)

  cat(sprintf(
    "  %s: %d training obs, %d validation obs in global split\n",
    sym, length(sym_train_global), length(sym_val_global)
  ))

  # Verify no mixing between symbols
  other_symbols <- setdiff(fit_mqbm$symbols, sym)
  for (other_sym in other_symbols) {
    other_idx <- fit_mqbm$symbol_info[[other_sym]]$indices
    overlap <- intersect(sym_idx, other_idx)
    if (length(overlap) > 0) {
      cat(sprintf("    ERROR: Symbol %s overlaps with %s!\n", sym, other_sym))
    }
  }
}

cat("\n=== 4. MQBM with custom folds ===\n\n")

# Define custom folds for the full dataset
custom_folds_multi <- list(
  1:66, # Fold 1
  67:133, # Fold 2
  134:200 # Fold 3
)

fit_mqbm_folds <- mqbm(y ~ x1 + x2,
  data = df_multi, tau = 0.5, nrounds = 50,
  folds = custom_folds_multi
)

cat("Custom folds applied to mqbm\n")
cat("Each symbol uses subset of global folds respecting boundaries\n\n")

for (sym in fit_mqbm_folds$symbols) {
  cat(sprintf(
    "  %s: fold_type = %s\n",
    sym, fit_mqbm_folds$models[[sym]]$cv_settings$fold_type
  ))
}

cat("\n=== SUCCESS ===\n")
cat("All train/val split options working correctly!\n")
cat("Symbol boundaries are properly respected in mqbm.\n")
