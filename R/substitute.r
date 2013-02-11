#' A version of substitute that evaluates its first argument.
#'
#' This version of substitute is needed because \code{substitute} does not
#' evaluate it's first argument, and it's often useful to be able to modify
#' a quoted call.
#'
#' @param x a quoted call
#' @param env an environment, or something that behaves like an environment
#'   (like a list or data frame), or a reference to an environment (like a
#'   positive integer or name, see \code{\link{as.environment}} for more
#'   details)
#' @export
#' @examples
#' x <- quote(a + b)
#' substitute(x, list(a = 1, b = 2))
#' substitute2(x, list(a = 1, b = 2))
substitute2 <- function(x, env) {
  env <- to_env(env)
  stopifnot(is.language(x))
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}
