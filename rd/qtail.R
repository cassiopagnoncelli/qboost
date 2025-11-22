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
y <- Y$excursion_high
