#' Lock variable bindings
#'
#' @param ... Variable names to lock (unquoted)
#' @param env Environment containing the variables (default: .GlobalEnv)
#' @return NULL (invisibly)
lock_all <- function(..., env = .GlobalEnv) {
  vars <- as.list(substitute(list(...)))[-1]
  for (v in vars) {
    name <- deparse(v)
    lockBinding(name, env)
  }
  invisible(NULL)
}

#' Unlock variable bindings
#'
#' @param ... Variable names to unlock (unquoted)
#' @param env Environment containing the variables (default: .GlobalEnv)
#' @return NULL (invisibly)
unlock_all <- function(..., env = .GlobalEnv) {
  vars <- as.list(substitute(list(...)))[-1]
  for (v in vars) {
    name <- deparse(v)
    unlockBinding(name, env)
  }
  invisible(NULL)
}
