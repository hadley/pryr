#' Promise info
#' 
#' @useDynLib pryr
#' @param x unquoted object name
#' @family promise tools
#' @export
#' @examples
#' x <- 10
#' is_promise(x)
#' (function(x) is_promise(x))(x = 10)
is_promise <- function(x) {
  is_promise2(substitute(x), parent.frame())
}

#' @rdname is_promise
#' @export
promise_info <- function(x) {
  name <- substitute(x)
  env <- parent.frame()

  stopifnot(is_promise2(name, env))

  evaled <- promise_evaled(name, env)
  list(
    code = promise_code(name, env),
    env = promise_env(name, env),
    evaled = evaled,
    value = if (evaled) promise_value(name, env)
  )
}
