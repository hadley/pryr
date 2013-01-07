#' A version of substitute that doesn't evaluate it's first argument
substitute2 <- function(x, env) {
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}

substitute2 <- function(x, env) {
  stopifnot(is.language(x))
  env <- as.environment(env)
  .Call("substitute2", x, env)
}
