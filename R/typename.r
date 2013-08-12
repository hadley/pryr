#' Determine the C typename associated with a name.
#' 
#' @param x name of object to inspect. This can not be a value. 
#' @family object inspection
#' @export
#' @examples
#' x <- 1:10
#' typename(x)
#' y <- 1L
#' typename(y)
#' z <- list(1:10)
#' typename(z)
#' delayedAssign("a", 1 + 2)
#' typename(a)
#' a
#' typename(a)
typename <- function(x) {
  typename2(substitute(x), parent.frame())
}
