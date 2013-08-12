#' Given a function class, find correspoding S4 method
#' 
#' @param call unquoted function call
#' @param env environment in which to look for function definition
#' @export
#' @examples
#' library(stats4)
#' 
#' # From example(mle)
#' y <- c(26, 17, 13, 12, 20, 5, 9, 8, 5, 4, 8)
#' nLL <- function(lambda) -sum(dpois(y, lambda, log = TRUE))
#' fit <- mle(nLL, start = list(lambda = 5), nobs = length(y))
#' 
#' method_from_call(summary(fit))
#' method_from_call(coef(fit))
#' method_from_call(length(fit))
method_from_call <- function(call, env = parent.frame()) {
  call <- standardise_call(substitute(call), env)
  
  generic <- as.character(call[[1]])
  
  args_uneval <- as.list(call[-1])
  args <- lapply(args_uneval, eval, env = env)
  classes <- lapply(args, class)
  
  selectMethod(generic, classes)  
}