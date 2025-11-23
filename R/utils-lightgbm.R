# Internal helper to predict from an lgb.Booster without relying on exported symbols
.lgb_predict <- function(booster, data) {
  # Prefer the Booster's own predict method
  if (!is.null(booster$predict)) {
    return(as.numeric(booster$predict(data)))
  }
  # Try exported lgb.predict
  if (exists("lgb.predict", envir = asNamespace("lightgbm"), inherits = FALSE)) {
    pred_fun <- get("lgb.predict", envir = asNamespace("lightgbm"))
    return(as.numeric(pred_fun(booster, data)))
  }
  # Try internal predict.lgb.Booster
  if (exists("predict.lgb.Booster", envir = asNamespace("lightgbm"), inherits = FALSE)) {
    pred_fun <- get("predict.lgb.Booster", envir = asNamespace("lightgbm"))
    return(as.numeric(pred_fun(booster, data)))
  }
  stop("LightGBM predict method not available")
}
