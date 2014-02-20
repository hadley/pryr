#' Promise info
#' 
#' @useDynLib pryr
#' @importFrom Rcpp sourceCpp
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

#' Find the parent (first) promise.
#' 
#' @param x unquoted name of promise to find initial value for for.
#' @export
#' @examples
#' f <- function(x) g(x)
#' g <- function(y) h(y)
#' h <- function(z) parent_promise(z)
#' 
#' h(x + 1)
#' g(x + 1)
#' f(x + 1)
parent_promise <- function(x) {
  name <- quote(x)
  
  for (frame in rev(sys.frames())) {
    if (!is_promise2(name, frame)) return(name)
    
    name <- promise_code(name, frame)
    if (!is.name(name)) return(name)    
  }
  
  name
}
