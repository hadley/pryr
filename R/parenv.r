#' Get parent/ancestor environment
#'
#' @param env an environment
#' @param n number of parents to go up
#' @export
#' @examples
#' adder <- function(x) function(y) x + y
#' add2 <- adder(2)
#' parenv(add2)
parenv <- function(env = parent.frame(), n = 1) {
  env <- to_env(env)
  for(i in seq_len(n)) env <- parent.env(env)
  env
}
