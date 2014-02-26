#' Unenclose a closure.
#'
#' Unenclose a closure by substituting names for values found in the enclosing
#' environment.
#'
#' @param f a closure
#' @export
#' @examples
#' power <- function(exp) {
#'   function(x) x ^ exp
#' }
#' square <- power(2)
#' cube <- power(3)
#'
#' square
#' cube
#' unenclose(square)
#' unenclose(cube)
unenclose <- function(f) {
  stopifnot(is.function(f))

  env <- environment(f)
  make_function(formals(f), substitute_q(body(f), env), parent.env(env))
}
