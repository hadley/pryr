#' Create a constant (locked) binding.
#'
#' Infix wrapper for \code{\link{assign}} + \code{\link{lockBinding}} that
#' creates a constant: a binding whose value can not be changed.
#'
#' @usage x \%<c-\% value
#' @param x unquoted expression naming variable to create
#' @param value constant value
#' @export
#' @rdname assign-constant
#' @examples
#' x %<c-% 10
#' #' Generates an error:
#' \dontrun{x <- 20}
#'
#' # Note that because of R's operator precedence rules, you
#' # need to wrap compound RHS expressions in ()
#' y %<c-% 1 + 2
#' y
#' z %<c-% (1 + 2)
#' z
"%<c-%" <- function(x, value) {
  name <- substitute(x)
  if (!is.name(name)) stop("Left-hand side must be a name")

  env <- parent.frame()
  assign(as.character(name), value, env)
  lockBinding(name, env)

  invisible(value)
}
