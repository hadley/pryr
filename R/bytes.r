##' Print the byte-wise representation of a value
##'
##' @param x Either an \code{integer} or a \code{numeric} vector.
##' @param split Boolean; if \code{TRUE} the returned string is split at
##'   regular indices for readability.
##' @export
##' @examples
##' ## Encoding doesn't change the internal bytes used to represent characters;
##' ## it just changes how they are interpretted!
##'
##' x <- y <- z <- "\u9b3c"
##' Encoding(y) <- "bytes"
##' Encoding(z) <- "latin1"
##' print(x); print(y); print(z)
##' bytes(x); bytes(y); bytes(z)
##' bits(x); bits(y); bits(z)
##'
##' ## in R, integers are signed ints. The first bit indicates the sign.
##' ## NA_integer is really just -0
##' bits(NA_integer_)
##'
##' ## There are multiple kinds of NAs, NaNs for real numbers
##' ## (at least, on 64bit architectures)
##' print( c(NA_real_, NA_real_ + 1) )
##' rbind( bytes(NA_real_), bytes(NA_real_ + 1) )
##' rbind( bytes(NaN), bytes(0/0) )
bytes <- function(x, split = TRUE) {
  repr <- hex_repr(x)
  if (split) slice(repr, 2)
  else repr
}

##' @rdname bytes
##' @export
bits <- function(x, split = TRUE) {
  repr <- binary_repr(x)
  if (split) slice(repr, 8)
  else repr
}

slice <- function(x, n, collapse = " ") {
  vapply(x, FUN.VALUE = character(1), USE.NAMES = FALSE, function(string) {
    starts <- seq(1L, nchar(string), by = n)
    paste(substring(string, starts, starts + n - 1L), collapse = collapse)
  })
}
