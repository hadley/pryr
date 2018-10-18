#' Chain multiple functions
#'
#' @param ... n functions to apply in order from left to right
#'
#' @export
#' @examples
#' 
#' add1 <- function(x) x + 1
#' times2 <- function(x) 2 * x
#' chain(add1, times2)(8)
chain <- function(...) {
  fs <- lapply(list(...), match.fun)

  first <- fs[[1]]
  rest <- fs[-1]

  function(...) {
    out <- first(...)
    for (f in rest) {
      out <- f(out)
    }
    out
  }
}
