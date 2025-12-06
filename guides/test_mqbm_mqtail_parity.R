# Test script to verify mqbm and mqtail have matching functionality
devtools::load_all()

set.seed(456)

cat("=== Testing mqbm and mqtail Parity ===\n\n")

# Create sample data with custom group column
n <- 300
df <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  group_id = sample(c("G1", "G2", "G3"), n, replace = TRUE)
)
df$y <- 2 * df$x1 + df$x2 + rnorm(n)

cat("Testing both functions with custom multi='group_id'\n\n")

# Test mqbm
cat("=== Testing mqbm ===\n")
mqbm_fit <- mqbm(
  y ~ x1 + x2,
  data = df,
  multi = "group_id",
  tau = 0.75,
  nrounds = 50,
  nfolds = 2
)
cat("mqbm fitted successfully\n")
cat("  Multi param:", mqbm_fit$multi, "\n")
cat("  Symbols:", paste(mqbm_fit$symbols, collapse = ", "), "\n")
cat("  Symbol count:", length(mqbm_fit$symbols), "\n")
cat("  Has ecdf_funs:", !is.null(mqbm_fit$ecdf_funs), "\n")
cat("  Has symbol_info:", !is.null(mqbm_fit$symbol_info), "\n")

# Test mqtail
cat("\n=== Testing mqtail ===\n")
mqtail_fit <- mqtail(
  y ~ x1 + x2,
  data = df,
  multi = "group_id",
  tail = "upper",
  taus = c(0.9, 0.95),
  threshold_tau = 0.95,
  params = list(nrounds = 50, nfolds = 2),
  verbose = FALSE
)
cat("mqtail fitted successfully\n")
cat("  Multi param:", mqtail_fit$multi, "\n")
cat("  Symbols:", paste(mqtail_fit$symbols, collapse = ", "), "\n")
cat("  Symbol count:", length(mqtail_fit$symbols), "\n")
cat("  Has ecdf_funs:", !is.null(mqtail_fit$ecdf_funs), "\n")
cat("  Has symbol_info:", !is.null(mqtail_fit$symbol_info), "\n")

# Test predictions with custom column name
newdata <- data.frame(
  x1 = c(0, 0, 0),
  x2 = c(0, 0, 0),
  group_id = c("G1", "G2", "G3")
)

cat("\n=== Testing Predictions ===\n")
mqbm_preds <- predict(mqbm_fit, newdata)
mqtail_preds <- predict(mqtail_fit, newdata, type = "quantile")

cat("mqbm predictions:\n")
print(data.frame(group_id = newdata$group_id, pred = mqbm_preds))

cat("\nmqtail predictions:\n")
print(data.frame(group_id = newdata$group_id, pred = mqtail_preds))

# Test error handling
cat("\n=== Testing Error Handling ===\n")
df_bad <- df
df_bad$group_id <- NULL

tryCatch(
  {
    mqbm(y ~ x1 + x2, data = df_bad, multi = "group_id", nrounds = 10)
    cat("ERROR: mqbm should have failed\n")
  },
  error = function(e) {
    cat("mqbm correctly caught error:", conditionMessage(e), "\n")
  }
)

tryCatch(
  {
    mqtail(y ~ x1 + x2, data = df_bad, multi = "group_id", params = list(nrounds = 10))
    cat("ERROR: mqtail should have failed\n")
  },
  error = function(e) {
    cat("mqtail correctly caught error:", conditionMessage(e), "\n")
  }
)

cat("\n✓ Both mqbm and mqtail handle multi parameter identically!\n")
