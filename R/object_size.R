#' @export
#' @rdname object_size
compare_size <- function(x) {
  c(base = object.size(x), pryr = object_size(x))
}
