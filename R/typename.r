#' @export
typename <- function(x) {
  typename2(substitute(x), parent.frame())
}
