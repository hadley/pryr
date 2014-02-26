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
#' substitute_q(x, list(a = 1, b = 2))
substitute_q <- function(x, env) {
  stopifnot(is.language(x))
  env <- to_env(env)

  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}

#' A version of substitute that works in the global environment.
#'
#' This version of \code{\link{substitute}} is more suited for interactive
#' exploration because it will perform substitution in the global environment:
#' the regular version has a special case for the global environment where it
#' effectively works like \code{\link{quote}}
#'
#' @section Substitution rules:
#'
#' Formally, substitution takes place by examining each name in the expression.
#' If the name refers to:
#'
#' \itemize{
#'
#'  \item an ordinary variable, it's replaced by the value of the variable.
#'
#'  \item a promise, it's replaced by the expression associated with the
#'     promise.
#'
#'  \item \code{...}, it's replaced by the contents of \code{...}
#' }
#' @inheritParams substitute_q
#' @export
#' @examples
#' a <- 1
#' b <- 2
#'
#' substitute(a + b)
#' subs(a + b)
subs <- function(x, env = parent.frame()) {
  if (identical(env, globalenv())) {
    env <- as.list(env)
  }

  substitute_q(substitute(x), env)
}
