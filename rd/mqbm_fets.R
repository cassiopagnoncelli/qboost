devtools::load_all()

library("fets")
library("qetl")
library("dtools")

quotes <- qetl::get_sample_quotes()
quotes <- quotes[symbol %in% sample(unique(quotes$symbol), 60)]

fets::fwd(quotes, lookahead = 15, inplace = TRUE)

macro <- fets::macro()
macro <- macro[, .SD, .SDcols = c("date", "vix", "dxy", "small_caps", "recession_prob_R_vel_0")]
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
fit <- mqtail(
  y ~ .,
  multi = "symbol",
  data = qXY,
  train_idx = train_idx,
  val_idx = val_idx,,
  taus = c(0.95, 0.97, 0.99, 0.995),
  threshold_tau = 0.97,
  nrounds = 600,
  early_stopping_rounds = 10,
  folds = 4
)

# Fit model
print(fit)

summary(fit)

# Assessing
yhat <- predict(fit, qXY[stages_idx, ], type = "quantile")

res <- tibble::tibble(yhat, qXY)

q <- res$yhat[c(train_idx, val_idx)] %>% quantile(0.999)
res[test_idx, ] %>%
  dplyr::filter(yhat < q) %>%
  dplyr::pull(y) %>%
  analyse(groups = 1)
