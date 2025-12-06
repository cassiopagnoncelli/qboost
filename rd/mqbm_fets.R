devtools::load_all()

library("fets")
library("qetl")
library("dtools")
library("dplyr")

quotes <- qetl::get_sample_quotes()
quotes <- quotes[symbol %in% sample(unique(quotes$symbol), 100)]

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

mX <- tibble::tibble(meta, X)
mcXY <- tibble::tibble(meta, close, X, Y)

qXY <- tibble::tibble(meta[, "symbol"], dplyr::select(Y, y = excursion_high), X)
qXY

# Grouping profiles
grouping_metrics <- mcXY[c(train_idx, val_idx), ] %>%
  group_by(symbol) %>%
  summarise(mean = mean(r, na.rm = TRUE), sd = sd(r, na.rm = TRUE))

km <- kmeans(grouping_metrics %>% select(sd), centers = 5, nstart = 50)
km

qXY$cluster <- km$cluster[match(qXY$symbol, grouping_metrics$symbol)]
qXY$symbol <- NULL

# Fit model
fit <- mqbm(
  y ~ .,
  multiplexer = "cluster",
  data = qXY,
  train_idx = train_idx,
  val_idx = val_idx,
  nfolds = 5,
  tau = 0.995,
  nrounds = 600,
  early_stopping_rounds = 10
)

# Summary
print(fit)

summary(fit)

# Assessing
yhat <- predict(fit, qXY[stages_idx, ], type = "quantile")

res <- tibble::tibble(yhat, qXY)

q <- res$yhat[c(train_idx, val_idx)] %>% quantile(0.999)
res[test_idx, ] %>%
  dplyr::filter(yhat > q) %>%
  dplyr::pull(y) %>%
  analyse(groups = 1)
