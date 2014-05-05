#' How much memory is currently used by R?
#'
#' R breaks down memory usage into Vcells (memory used by vectors) and
#' Ncells (memory used by everything else). However, neither this distinction
#' nor the "gc trigger" and "max used" columns are typically important. What
#' we're usually most interested in is the the first column: the total memory
#' used. This function wraps around \code{gc()} to return the total amount of
#' memory (in megabytes) currently used by R.
#'
#' @export
#' @return Megabytes of ram used by R objects.
#' @examples
#' mem_used()
mem_used <- function() {
  sum(gc()[, 1] * c(node_size(), 8)) / (1024 ^ 2)
}

node_size <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }

  if (bit == 32L) 28L else 56L
}
