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
model <- qevt(zX[train_idx, ], y[train_idx], tau_target = 0.999)

summary(model)

plot(model, newdata = zX[test_idx, ])

# Predictions
preds <- predict(model, zX[-train_idx, ]) # Predict on holdout
stopifnot(all(apply(preds$monotone, 1, function(v) all(diff(v) >= 0)))) # Monotonicity
q999 <- preds$monotone[, which(preds$taus == 0.999)] # Extract q0.999

actuals <- y[-train_idx]
residuals <- actuals - q999

tb <- tibble::tibble(data.frame(y = actuals, yhat = q999, e = residuals))

# Kendall Ordering
idx <- tb$yhat > quantile(tb$yhat, 0.998, na.rm = TRUE)
cor(tb$y[idx], tb$yhat[idx], method = "kendall")

idx <- tb$yhat > quantile(tb$yhat, 0.999, na.rm = TRUE)
cor(tb$y[idx], tb$yhat[idx], method = "kendall")

idx <- tb$yhat > quantile(tb$yhat, 0.9993, na.rm = TRUE)
cor(tb$y[idx], tb$yhat[idx], method = "kendall")

# Distributions
analyse_distribution(exp(actuals))

tb %>%
  dplyr::filter(yhat > quantile(yhat, probs = .999)) %>%
  dplyr::pull(y) %>%
  { exp(.) } %>%
  analyse_distribution()

tb %>%
  dplyr::filter(y > quantile(y, probs = .999)) %>%
  dplyr::pull(y) %>%
  { exp(.) } %>%
  analyse_distribution()
