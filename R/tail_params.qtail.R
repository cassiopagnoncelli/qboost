#' Tail parameters method for qtail
#'
#' @param object A qtail object
#' @param ... Additional arguments (not used)
#'
#' @return List of EVT parameters
#' @export
tail_params.qtail <- function(object, ...) {
  return(list(
    xi = object$evt$xi,
    beta = object$evt$beta,
    threshold_tau = object$threshold_tau,
    tail = object$tail
  ))
}
