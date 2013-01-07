make_function1 <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  eval(call("function", args, body), env)
}
make_function2 <- function(args, body, env = parent.frame()) {
  f <- function() {}
  formals(f) <- args
  body(f) <- body
  environment(f) <- env

  f
}
make_function3 <- function(args, body, env = parent.frame()) {
  as.function(c(args, body), env)
}
make_function4 <- function(args, body, env = parent.frame()) {
  subs <- list(args = as.pairlist(args), body = body)
  eval(substitute(`function`(args, body), subs), env)
}

args <- alist(a = 1, b = 2)
body <- quote(a + b)
make_function1(args, body)
make_function2(args, body)
make_function3(args, body)
make_function4(args, body)

library(microbenchmark)
microbenchmark(
  make_function1(args, body),
  make_function2(args, body),
  make_function3(args, body),
  make_function4(args, body),
  function(a = 1, b = 2) a + b
)
