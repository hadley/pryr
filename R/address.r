#' Return the memory address of an R object.
#' 
#' This is useful when determining if an object is modified or not. This
#' function uses non-standard evaluation to avoid incrementing the reference
#' counter for the object, which distorts investigation of in-place
#' modification.
#' 
#' @param x object to inspect
#' @export
#' @examples
#' x <- 1:5
#' address(x)
#' \dontrun{.Internal(inspect(x))}
#' x[1] <- 3L
#' address(x)
address <- function(x, env = parent.frame()) {
  address2(substitute(x), env)
}
