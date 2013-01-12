#' Substitute names for values.
#'
#' This function is a specialised version of \code{\link{substitute}} that
#' only substitutes names for values.
#'
#' @param expr a language object, function, expression or list of the above to
#'   modify
#' @param env an environment-like object containing named values to subsitute.
#' @export
#' @examples
#' subs(quote(a + 1), list(a = 2))
#' f <- function(z = x) y * z
#' g <- subs(f, list(x = quote(runif(1)), y = 2, z = quote(x)))
subs <- function(expr, env) {
  replace <- function(expr) {
    if (!is.name(expr)) return(expr)

    name <- as.character(expr)
    if (!exists(name, env)) return(expr)

    get(name, env)
  }

  modify_lang(expr, replace)
}
