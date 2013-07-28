#' Create an active binding.
#'
#' Infix form of \code{\link{makeActiveBinding}} which creates an \emph{active}
#' binding between a name and an expression: every time the name is accessed
#' the expression is recomputed.
#'
#' @usage x \%<a-\% value
#' @param x unquoted expression naming variable to create
#' @param value unquoted expression to evaluate every time \code{name} is
#'   accessed
#' @export
#' @rdname assign-active
#' @examples
#' x %<a-% runif(1)
#' x
#' x
#' x %<a-% runif(10)
#' x
#' x
#' rm(x)
"%<a-%" <- function(x, value) {
  x <- substitute(x)
  value <- substitute(value)

  if (!is.name(x)) stop("Left-hand side must be a name")

  env <- parent.frame()
  f <- make_function(alist(value = ), value, env)

  # Mimic regular assignment operation which overrides existing bindings
  if (exists(deparse(x), envir = env, inherits = FALSE)) {
    rm(x, envir = env)
  }

  makeActiveBinding(deparse(x), f, env)
}
