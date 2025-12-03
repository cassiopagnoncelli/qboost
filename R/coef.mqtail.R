#' Extract GPD tail parameters from mqtail model
#'
#' Returns the Generalized Pareto Distribution (GPD) parameters for each symbol.
#'
#' @param object A fitted mqtail model object.
#' @param ... Additional arguments (unused).
#'
#' @return A data.frame with GPD parameters (xi, beta) for each symbol.
#'
#' @seealso \code{\link{mqtail}}, \code{\link{tail_params.qtail}}
#'
#' @export
coef.mqtail <- function(object, ...) {
  if (!inherits(object, "mqtail")) {
    stop("`object` must be a mqtail model.", call. = FALSE)
  }

  # Extract GPD parameters from each symbol
  params_list <- lapply(object$symbols, function(sym) {
    evt <- object$models[[sym]]$evt
    data.frame(
      symbol = sym,
      xi = evt$xi,
      beta = evt$beta,
      n_exceedances = evt$n_exceedances,
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, params_list)
}
