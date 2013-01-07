make_function <- function(args, body, env = parent.frame()) {
  args <- as.pairlist(args)
  eval(call("function", args, body), env)
}
