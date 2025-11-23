devtools::load_all()

# ETL.
quotes <- qetl::get_sample_quotes()
macro <- fets::macro()

# Forward lookahead
fets::fwd(quotes, lookahead = 20, inplace = TRUE)

# Add macro indicators
fets::add_macro(quotes, macro)

# Engineer features based on current series solely
quotes_fwd_fe <- fets::fe(quotes, inplace = TRUE)

# Decomposition
mXY <- quotes_fwd_fe$X %>% na.omit() %>% tibble::tibble()

decomposed <- fets::decomposeXY(mXY, na.rm.X = TRUE, na.rm.Y = TRUE)
meta <- decomposed$meta
close <- decomposed$close
volume <- decomposed$volume
Y <- decomposed$Y
X <- decomposed$X %>% dplyr::select(-matches("^(smoothed_close)"))
y <- log(Y$excursion_high)

# Splits
train_end <- as.Date("2023-05-31")
val_end <- as.Date("2024-05-31")

train_full_idx <- which(meta$date <= train_end)
train_idx <- train_full_idx #%>% sample(100000)
val_idx <- which(meta$date > train_end & meta$date <= val_end)
test_idx <- which(meta$date > val_end)
stages_idx <- c(train_idx, val_idx, test_idx)

zX <- scale(X[train_idx, ])
zX_centers <- attr(zX, "scaled:center")
zX_scales <- attr(zX, "scaled:scale")
X_scaled <- scale_new_data(X, center = zX_centers, scale = zX_scales)
zX <- tibble::as_tibble(X_scaled)

mzX <- tibble::tibble(meta, zX)
mczXY <- tibble::tibble(meta, close, zX, Y)
zXY <- tibble::tibble(zX, Y)

# Qevt fit on scaled features.
model <- qevt(zX[train_idx, ], y[train_idx], tau_target = 0.9965)

summary(model, newdata = zX[test_idx, ], y = y[test_idx])

plot(model, newdata = zX[test_idx, ], y = y[test_idx])

# Predictions
preds <- predict(model, zX[-train_idx, ]) # Predict on holdout
stopifnot(all(apply(preds$monotone, 1, function(v) all(diff(v) >= 0)))) # Monotonicity
selected_q <- preds$monotone[, which(preds$taus == preds$taus[4])[1]] # Extract q0.999

actuals <- y[-train_idx]
residuals <- actuals - selected_q

tb <- tibble::tibble(data.frame(y = actuals, yhat = selected_q, e = residuals))

# Kendall Ordering
kendall_at <- function(thresh) {
  idx <- tb$yhat > stats::quantile(tb$yhat, thresh, na.rm = TRUE)
  if (sum(idx, na.rm = TRUE) < 2) return(NA_real_)
  suppressWarnings(stats::cor(tb$y[idx], tb$yhat[idx], method = "kendall", use = "pairwise.complete.obs"))
}
kendall_998 <- kendall_at(0.998)
kendall_999 <- kendall_at(0.999)
kendall_9993 <- kendall_at(0.9993)
kendall_998
kendall_999
kendall_9993

# Distributions
analyse_distribution(exp(actuals))

tb %>%
  dplyr::filter(yhat > quantile(yhat, probs = .997)) %>%
  dplyr::pull(y) %>%
  { exp(.) } %>%
  analyse_distribution()

tb %>%
  dplyr::filter(y > quantile(y, probs = .997)) %>%
  dplyr::pull(y) %>%
  { exp(.) } %>%
  analyse_distribution()
