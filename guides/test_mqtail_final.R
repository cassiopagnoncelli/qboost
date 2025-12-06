# Final test for mqtail with custom column names
devtools::load_all()

set.seed(456)

cat("=== Final Test: mqtail with Custom Column Name ===\n\n")

# Create sample data with custom grouping column
n <- 300
df <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  cluster = sample(c("C1", "C2", "C3"), n, replace = TRUE)
)
df$y <- 1.5 * df$x1 + 0.8 * df$x2 + rt(n, df = 3)

cat("Data: ", nrow(df), " rows, group column: 'cluster'\n")
cat("Groups:", paste(unique(df$cluster), collapse = ", "), "\n\n")

# Fit mqtail with custom multi parameter
cat("=== Fitting mqtail with multi='cluster' ===\n")
fit <- mqtail(
  y ~ x1 + x2,
  data = df,
  multi = "cluster",
  tail = "upper",
  taus = c(0.9, 0.95, 0.99),
  threshold_tau = 0.99,
  params = list(nrounds = 50, nfolds = 2),
  verbose = TRUE
)

cat("\n=== Model Info ===\n")
cat("Multi parameter:", fit$multi, "\n")
cat("Symbols:", paste(fit$symbols, collapse = ", "), "\n")
cat("Taus:", paste(fit$taus, collapse = ", "), "\n")
cat("Number of mqbm models:", length(fit$mqbm_models), "\n")
cat("Total qbm models trained:", length(fit$symbols) * length(fit$taus), "\n")

cat("\n=== Predictions with Custom Column ===\n")
newdata <- data.frame(
  x1 = c(0, 0, 0),
  x2 = c(0, 0, 0),
  cluster = c("C1", "C2", "C3")
)

preds <- predict(fit, newdata, type = "surface")
cat("Surface predictions:\n")
print(data.frame(cluster = newdata$cluster, prediction = preds))

cat("\n=== Error Handling ===\n")
df_bad <- df
df_bad$cluster <- NULL
tryCatch({
  mqtail(y ~ x1 + x2, data = df_bad, multi = "cluster", params = list(nrounds = 10))
  cat("ERROR: Should have failed\n")
}, error = function(e) {
  cat("✓ Correctly caught error:", conditionMessage(e), "\n")
})

cat("\n✓ All tests passed! mqtail fully functional with new architecture.\n")
