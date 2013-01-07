#' Capture the call associated with promise.
#'
#' This is an alternative to subsitute that performs one job, and so gives
#' a stronger signal regarding the intention of your code.  It returns an error
#' if the name is not associated with a promise.
#'
#' @export
#' @examples
#' f <- function(x) {
#'    uneval(x)
#' }
#' f(a + b)
#' f(1 + 4)
#'
#' delayedAssign("x", 1 + 4)
#' uneval(x)
uneval <- function(x) {
  name <- substitute(x)
  stopifnot(is.name(name))

  env <- parent.frame()

  if (!is_promise2(name, env)) {
    stop(name, "is not a promise", call. = FALSE)
  }

  .Call("promise_code", name, env)
}
