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
  g_args <- setdiff(names(formals(methods::getGeneric(generic))), "...")
  
  args_uneval <- as.list(call[intersect(g_args, names(call))])
  args <- lapply(args_uneval, eval, env = env)
  classes <- lapply(args, class)
  
  # Add in any missing args
  missing <- setdiff(g_args, names(classes))
  if (length(missing) > 0) {
    classes[missing] <- rep("missing", length(missing))  
  }
  
  methods::selectMethod(generic, classes)  
}
