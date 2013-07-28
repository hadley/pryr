#' Find a function with specified name.
#'
#' @param name length one character vector giving name
#' @param env environment to start search in.
#' @export
#' @examples
#' c <- 10
#' fget("c")
fget <- function(name, env = parent.frame()) {
  env <- to_env(env)
  if (identical(env, emptyenv())) {
    stop("Could not find function called ", name, call. = FALSE)
  }

  if (exists(name, env, inherits = FALSE) && is.function(env[[name]])) {
    env[[name]]
  } else {
    fget(name, parent.env(env))
  }
}
