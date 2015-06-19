#' Active binding info
#' 
#' @param x unquoted object name
#' @export
#' @examples
#' x <- 10
#' is_active_binding(x)
#' x %<a-% runif(1)
#' is_active_binding(x)
#' y <- x
#' is_active_binding(y)
is_active_binding <- function(x) {
  bindingIsActive(substitute(x), parent.frame())
}
