#' Recursive ls.
#'
#' Performs \code{\link{ls}} all the way up to a top-level environment (either
#' the parent of the global environment, the empty environment or a namespace
#' environment).
#'
#' @param env environment to start the search at. Defaults to the
#'  \code{\link{parent.frame}}. If a function is supplied, uses the environment
#'  associated with the function.
#' @param all.names Show all names, even those starting with \code{.}?
#'   Defaults to \code{TRUE}, the opposite of \code{\link{ls}}
#' @export
#' @author Winston Chang
rls <- function(env = parent.frame(), all.names = TRUE) {
  env <- to_env(env)
  if (terminal_env(env)) return()

  names <- ls(env, all.names = all.names)
  c(list(names), rls(parent.env(env), all.names = all.names))
}

terminal_env <- function(e) {
  identical(e, parent.env(globalenv())) || identical(e, emptyenv()) ||
      exists('.__NAMESPACE__.', e, inherits = FALSE)
}
