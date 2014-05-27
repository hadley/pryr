#' Find the environment that encloses of a function.
#'
#' This is a wrapper around \code{\link{environment}} with a
#' consistent syntax.
#'
#' @param f The name of a function.
#' @export
#' @examples
#' enclosing_env("plot")
#' enclosing_env("t.test")
enclosing_env <- function(f) {
  f <- match.fun(f)
  environment(f)
}
