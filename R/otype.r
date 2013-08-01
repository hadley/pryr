#' Determine object type
#' 
#' @export
otype <- function(x) {
  if (isS4(x)) {
    if (is(x, "refClass")) {
      "RC"
    } else {
      "S4"
    }
  } else {
    if (is.null(attr(x, "class"))) {
      "basic"
    } else {
      "S3"
    }
  }
}
