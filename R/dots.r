#' Capture unevaluated dots.
#'
#' @param ... \code{...} passed in to the parent function
#' @return a list of expressions (not expression objects). \code{named_dots}
#'  will use the deparsed expressions as default names.
#' @export
#' @examples
#' y <- 2
#' str(dots(x = 1, y, z = ))
#' str(named_dots(x = 1, y, z =))
dots <- function(...) {
  eval(substitute(alist(...)))
}

#' @rdname dots
#' @export
named_dots <- function(...) {
  args <- dots(...)

  nms <- names(args) %||% rep("", length(args))
  missing <- nms == ""
  if (all(!missing)) return(args)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(args[missing], deparse2, character(1), USE.NAMES = FALSE)

  names(args)[missing] <- defaults
  args
}
