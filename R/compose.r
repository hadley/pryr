#' Compose multiple functions
#'
#' In infix and prefix forms.
#'
#' @param ... n functions to apply in order from right to left
#' @param f,g two functions to compose for the infix form
#'
#' @export
#' @examples
#' not_null <- `!` %.% is.null
#' not_null(4)
#' not_null(NULL)
#' 
#' add1 <- function(x) x + 1
#' compose(add1,add1)(8)
compose <- function(...) {
  fs <- lapply(list(...), match.fun)
  n <- length(fs)

  last <- fs[[n]]
  rest <- fs[-n]

  function(...) {
    out <- last(...)
    for (f in rev(rest)) {
      out <- f(out)
    }
    out
  }
}

#' @rdname compose
#' @export
#' @usage f \%.\% g
'%.%' <- function(f, g) {
  f <- match.fun(f)
  g <- match.fun(g)
  function(...) {
    f(g(...))
  }
}
