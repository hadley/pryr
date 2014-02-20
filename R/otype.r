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
#' @param x object to determine type of
#' @export
#' @family object inspection
#' @examples
#' otype(data.frame())
#' otype(1:10)
otype <- function(x) {
  if (!is.object(x)) return("primitive")
  if (!isS4(x)) return("S3")

  if (is(x, "refClass")) {
    "RC"
  } else {
    "S4"
  }
}
