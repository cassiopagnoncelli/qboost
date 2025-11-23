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
Y <- decomposed$Y
X <- decomposed$X %>% dplyr::select(-matches("^(smoothed_close)"))
y <- log(Y$excursion_high)

# Qtail.
train <- 1:50000
fit <- qtail(X[train, ], y[train], tail = "upper", verbose = TRUE)

summary(fit)

preds <- predict(fit, newdata = X[-train, ], type = "final", tau_override = 0.999)
actuals <- y[-train]
residuals <- actuals - preds

tb <- tibble::tibble(data.frame(y = actuals, yhat = preds, e = residuals))

# Kendall Ordering
idx <- tb$yhat > quantile(tb$yhat, 0.98, na.rm = TRUE)
cor(tb$y[idx], tb$yhat[idx], method = "kendall")

idx <- tb$yhat > quantile(tb$yhat, 0.99, na.rm = TRUE)
cor(tb$y[idx], tb$yhat[idx], method = "kendall")

idx <- tb$yhat > quantile(tb$yhat, 0.999, na.rm = TRUE)
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
