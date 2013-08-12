#' Standardise a function call
#' 
#' 
standardise_call <- function(call, env = parent.frame()) {
  stopifnot(is.call(call))
  f <- eval(call[[1]], env)
  if (is.primitive(f)) return(call)
  
  match.call(f, call)
}
