#' Determine object type.
#' 
#' @details
#' Figure out which object system an object belongs to:
#' 
#' \itemize{
#'   \item primitive: no class attribute
#'   \item S3: class attribute, but not S4
#'   \item S4: \code{\link{isS4}}, but not RC
#'   \item RC: inherits from "refClass"
#' }
#' 
#' @export
#' @family object inspection
#' @examples
#' otype(data.frame())
#' otype(1:10)
otype <- function(x) {
  if (isS4(x)) {
    if (is(x, "refClass")) {
      "RC"
    } else {
      "S4"
    }
  } else {
    if (is.null(attr(x, "class"))) {
      "primitive"
    } else {
      "S3"
    }
  }
}
