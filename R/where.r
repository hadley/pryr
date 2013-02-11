#' Find where a name is defined.
#'
#' Implements the regular scoping rules, but instead of returning the value
#' associated with a name, it returns the environment in which it is located.
#'
#' @param name name, as string, to look for
#' @param env environment to start at. Defaults to the calling environment
#'   of this function.
#' @export
#' @examples
#' x <- 1
#' where("x")
#' where("t.test")
#' where("mean")
#' where("where")
where <- function(name, env = parent.frame()) {
  stopifnot(is.character(name), length(name) == 1)
  env <- to_env(env)

  if (identical(env, emptyenv())) {
    stop("Can't find ", name, call. = FALSE)
  }

  if (exists(name, env, inherits = FALSE)) {
    env
  } else {
    where(name, parent.env(env))
  }
}
