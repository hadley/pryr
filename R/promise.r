#' @useDynLib pryr
is_promise <- function(x) {
  is_promise2(substitute(x), parent.frame())
}

is_promise2 <- function(name, env) {
  .Call("is_promise", name, env)
}

promise_info <- function(x) {
  name <- substitute(x)
  env <- parent.frame()

  stopifnot(is_promise2(name, env))

  evaled <- .Call("promise_evaled", name, env)
  list(
    code = .Call("promise_code", name, env),
    env = .Call("promise_env", name, env),
    evaled = evaled,
    value = if (evaled) .Call("promise_value", name, env)
  )
}
