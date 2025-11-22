#' Probability Integral Transform for qtail
#'
#' @param object A qtail object
#' @param ... Additional arguments (not used)
#'
#' @return Vector of PIT values
#' @export
qtail_pit <- function(object, ...) {
  if (!inherits(object, "qtail")) {
    stop("`object` must be a qtail model.", call. = FALSE)
  }
  
  n <- length(object$y)
  taus <- object$taus
  pit <- numeric(n)
  
  for (i in seq_len(n)) {
    # Predict quantiles for this observation
    qgrid <- numeric(length(taus))
    for (j in seq_along(taus)) {
      qgrid[j] <- predict.qtail(object, object$x[i, , drop = FALSE], 
                                 type = "final", tau_override = taus[j])
    }
    
    # Monotonic interpolation to get PIT value
    # For upper tail: taus increasing, qgrid increasing
    # For lower tail: taus decreasing, but we need to handle this
    if (object$tail == "lower") {
      # Reverse for interpolation to work correctly
      qgrid <- rev(qgrid)
      taus_interp <- rev(taus)
    } else {
      taus_interp <- taus
    }
    
    # Interpolate with error handling
    pit_val <- tryCatch(
      stats::approx(qgrid, taus_interp, xout = object$y[i], rule = 2)$y,
      error = function(e) {
        # If interpolation fails, use NA
        NA_real_
      }
    )
    
    # Ensure PIT is within [0, 1]
    pit[i] <- if (!is.na(pit_val)) pmax(0, pmin(1, pit_val)) else NA_real_
  }
  
  return(pit)
}
