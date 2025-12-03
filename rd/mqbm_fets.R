devtools::load_all()

library("fets")
library("qetl")

quotes <- qetl::get_sample_quotes()
quotes <- quotes[symbol %in% unique(quotes$symbol)[seq_len(10)]]

fets::fwd(quotes, lookahead = 15, inplace = TRUE)

macro <- fets::macro()
fets::add_macro(quotes, macro)

sentiments <- qetl::sentiments()
fets::add_sentiment(quotes, sentiments)

quotes_fwd_fe <- fets::fe(quotes, inplace = TRUE)

fets::drop_ohlv(quotes_fwd_fe$X)
quotes_fwd_fe$X[, smoothed_close := NULL]

mXY <- quotes_fwd_fe$X %>%
  na.omit() %>%
  tibble::as_tibble()

decomposed <- fets::decomposeXY(mXY, na.rm.X = TRUE, na.rm.Y = TRUE)
meta <- decomposed$meta
close <- decomposed$close
volume <- decomposed$volume
Y <- decomposed$Y
X <- decomposed$X

train_end <- as.Date("2023-05-31")
val_end <- as.Date("2024-05-31")

train_full_idx <- which(meta$date <= train_end)
train_idx <- train_full_idx %>% sample(min(length(train_full_idx), 120000))
val_idx <- which(meta$date > train_end & meta$date <= val_end)
test_idx <- which(meta$date > val_end)
stages_idx <- c(train_idx, val_idx, test_idx)

zX <- scale(X[train_idx, ])
zX_centers <- attr(zX, "scaled:center")
zX_scales <- attr(zX, "scaled:scale")
zX <- tibble::as_tibble(scale_new_data(X, center = zX_centers, scale = zX_scales))

mzX <- tibble::tibble(meta, zX)
mczXY <- tibble::tibble(meta, close, zX, Y)
zXY <- tibble::tibble(zX, Y)

qXY <- tibble::tibble(meta[, "symbol"], dplyr::select(Y, y = excursion_high), zX)
qXY

# Training
cat("Training mqbm model...\n")
fit <- mqbm(
  y ~ .,
  data = qXY[train_idx, ],
  multi = "symbol",
  tau = 0.98,
  nrounds = 400,
  early_stopping_rounds = 20,
  nfolds = 3
)

# Fit model
print(fit)

summary(fit)

# Use predict, fitted, etc

# ------------------------------------------------------------------
# Predictions
# ------------------------------------------------------------------
cat("\n\nMaking predictions...\n")

# Method 1: symbol column in newdata
preds1 <- predict(fit, test_df)

# Method 2: separate symbol argument
preds2 <- predict(fit, test_df[, c("x1", "x2")], symbol = test_df$symbol)

cat("Predictions match:", all(preds1 == preds2), "\n")

# ------------------------------------------------------------------
# Fitted values
# ------------------------------------------------------------------
cat("\nFitted values on training data:\n")
fitted_vals <- fitted(fit)
cat("Length:", length(fitted_vals), "\n")
cat("Range:", range(fitted_vals), "\n")

# ------------------------------------------------------------------
# Model per symbol
# ------------------------------------------------------------------
cat("\nIndividual models per symbol:\n")
for (sym in fit$symbols) {
  model <- fit$models[[sym]]
  cat(sprintf("  %s: %d trees, %d observations\n",
              sym, model$best_iter, fit$symbol_info[[sym]]$n))
}

# ------------------------------------------------------------------
# Compare predictions across symbols
# ------------------------------------------------------------------
cat("\nPrediction comparison:\n")
for (sym in fit$symbols) {
  sym_idx <- which(test_df$symbol == sym)
  if (length(sym_idx) > 0) {
    sym_preds <- preds1[sym_idx]
    cat(sprintf("  %s: mean=%.3f, sd=%.3f, n=%d\n",
                sym, mean(sym_preds), sd(sym_preds), length(sym_idx)))
  }
}

# ------------------------------------------------------------------
# Custom multi parameter example
# ------------------------------------------------------------------
cat("\n\nCustom multi parameter example:\n")

# Create data with a different grouping column
df2 <- data.frame(
  x1 = rnorm(200),
  x2 = rnorm(200),
  group = sample(c("GroupA", "GroupB", "GroupC"), 200, replace = TRUE)
)
df2$y <- df2$x1 * 0.5 + rnorm(200)

# Train with custom multi parameter
fit2 <- mqbm(
  y ~ x1 + x2,
  data = df2,
  multi = "group",  # Use "group" instead of default "symbol"
  tau = 0.5,
  nrounds = 50,
  nfolds = 3
)

cat("Multi parameter used:", fit2$multi, "\n")
cat("Groups:", paste(fit2$symbols, collapse = ", "), "\n")

# Predictions work with the group column
newdata <- data.frame(
  x1 = rnorm(50),
  x2 = rnorm(50),
  group = sample(c("GroupA", "GroupB", "GroupC"), 50, replace = TRUE)
)
preds_custom <- predict(fit2, newdata)
cat("Custom predictions generated:", length(preds_custom), "\n")

cat("\nDemo complete!\n")
