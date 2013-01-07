#' Capture the call for a promise.
#'
#' Returns an error if not a promise.
uneval <- function(x) {
  name <- substitute(x)
  env <- parent.frame()

  if (!is_promise2(name, env)) {
    stop(name, "is not a promise", call. = FALSE)
  }

  .Call("promise_code", name, env)
}
