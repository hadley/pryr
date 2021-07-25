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
  show_bytes(sum(gc()[, 1] * c(node_size(), 8)))
}

node_size <- function() {
  bit <- 8L * .Machine$sizeof.pointer
  if (!(bit == 32L || bit == 64L)) {
    stop("Unknown architecture", call. = FALSE)
  }

  if (bit == 32L) 28L else 56L
}

#' Determine change in memory from running code
#'
#' @param code Code to evaluate.
#' @return Change in memory (in megabytes) before and after running code.
#' @examples
#' # Need about 4 mb to store 1 million integers
#' mem_change(x <- 1:1e6)
#' # We get that memory back when we delete it
#' mem_change(rm(x))
#' @export
mem_change <- function(code) {
  start <- mem_used()

  expr <- substitute(code)
  eval(expr, parent.frame())
  rm(code, expr)

  show_bytes(mem_used() - start)
}

show_bytes <- function(x) {
  structure(x, class = "pryr_bytes")
}

#' @export
print.pryr_bytes <- function(x, digits = 3, ...) {
  power <- min(floor(log(abs(x), 1000)), 4)
  if (power < 1) {
    unit <- "B"
  } else {
    unit <- c("kB", "MB", "GB", "TB")[[power]]
    x <- x / (1000 ^ power)
  }

  formatted <- format(signif(x, digits = digits), big.mark = ",",
    scientific = FALSE)

  cat(formatted, " ", unit, "\n", sep = "")
}
