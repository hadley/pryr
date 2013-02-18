#' Compose multiple functions
#'
#' In infix and prefix forms.
#'
#' @param ... n functions to apply in order from right to left
#' @param f,g two functions to compose for the infix form
#'
#' @export
#' @examples
#' not_null <- "!" %.% is.null
#' not_null(4)
#' not_null(NULL)
compose <- function(...) {
  fs <- lapply(list(...), match.fun)
  n <- length(fs)

  last <- fs[[n]]
  rest <- fs[-n]

  function(...) {
    out <- last(...)
    for (f in fs) {
      out <- f(out)
    }
    out
  }
}

#' @rdname compose
#' @export
'%.%' <- function(f, g) {
  f <- match.fun(f)
  g <- match.fun(g)
  function(...) {
    f(g(...))
  }
}
