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
  body <- modify_lang(body(f), unenclose_a_to_b(env))
  make_function(formals(f), body, parent.env(env))
}


unenclose_a_to_b <- function(env, ls_env = ls(envir = env)) {
  function(x) {
    if (is.name(x)) {
      dep_x <- deparse(x)
      if (dep_x %in% ls_env) {
        return(get(dep_x, envir = env))
      }
    }
    x
  }
}
