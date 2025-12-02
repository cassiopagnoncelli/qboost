#' qboost compatibility wrappers
#'
#' These wrappers keep the user-facing `qboost()` API available while the core
#' implementation lives in `qbm()`. They simply forward to the corresponding
#' `qbm` functions.
#'
#' @rdname qbm
#' @export
qboost <- function(
    ...,
    tau = 0.5,
    nrounds = 500,
    nfolds = 5,
    params = list(),
    early_stopping_rounds = 50,
    seed = 1) {
  qbm(
    ...,
    tau = tau,
    nrounds = nrounds,
    nfolds = nfolds,
    params = params,
    early_stopping_rounds = early_stopping_rounds,
    seed = seed
  )
}
