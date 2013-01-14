#' Create a constant (locked) binding.
#'
#' Infix wrapper for \code{\link{assign}} + \code{\link{lockBinding}} that
#' creates a constant: a binding whose value can not be changed.
#'
#' @param x unquoted expression naming variable to create
#' @param value constant value
#' @export
#' @rdname assign-active
#' @examples
#' x %<c-% 10
#' #' Generates an error:
#' \dontrun{x <- 20}
"%<c-%" <- function(x, value) {
  name <- substitute(x)
  value <- substitute(value)

  if (!is.name(name)) stop("Left-hand side must be a name")

  env <- parent.frame()
  assign(as.character(name), value, env)
  lockBinding(name, env)

  invisible(value)
}
