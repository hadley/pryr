#' Print the byte-wise representation of a value
#'
#' @param x An \R vector of type \code{integer}, \code{numeric}, \code{logical}
#'   or \code{character}.
#' @param split Whether we should split the output string at each byte.
#' @export
#' @examples
#' ## Encoding doesn't change the internal bytes used to represent characters;
#' ## it just changes how they are interpretted!
#'
#' x <- y <- z <- "\u9b3c"
#' Encoding(y) <- "bytes"
#' Encoding(z) <- "latin1"
#' print(x); print(y); print(z)
#' bytes(x); bytes(y); bytes(z)
#' bits(x); bits(y); bits(z)
#'
#' ## In R, integers are signed ints. The first bit indicates the sign, but
#' ## values are stored in a two's complement representation. We see that
#' ## NA_integer_ is really just the smallest negative integer that can be
#' ## stored in 4 bytes
#' bits(NA_integer_)
#'
#' ## There are multiple kinds of NAs, NaNs for real numbers
#' ## (at least, on 64bit architectures)
#' print( c(NA_real_, NA_real_ + 1) )
#' rbind( bytes(NA_real_), bytes(NA_real_ + 1) )
#' rbind( bytes(NaN), bytes(0/0) )
#' @references
#' \url{https://en.wikipedia.org/wiki/Two's_complement} for more
#' information on the representation used for \code{int}s.
#'
#' \url{https://en.wikipedia.org/wiki/IEEE_floating_point} for more
#' information the floating-point representation used for \code{double}s.
#'
#' \url{https://en.wikipedia.org/wiki/Character_encoding} for an introduction
#' to character encoding, and \code{?\link{Encoding}} for more information on
#' how \R handles character encoding.
bytes <- function(x, split = TRUE) {
  repr <- hex_repr(x)
  if (split) slice(repr, 2L)
  else repr
}

#' @rdname bytes
#' @export
bits <- function(x, split = TRUE) {
  repr <- binary_repr(x)
  if (split) slice(repr, 8L)
  else repr
}
