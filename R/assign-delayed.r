#' Create an delayed binding.
#'
#' Infix form of \code{\link{delayedAssign}} which creates an \emph{delayed}
#' or lazy binding, which only evaluates the expression the first time it is
#' used.
#'
#' @usage x \%<d-\% value
#' @param x unquoted expression naming variable to create
#' @param value unquoted expression to evaluate the first time \code{name} is
#'   accessed
#' @export
#' @rdname assign-delayed
#' @examples
#' x %<d-% (a + b)
#' a <- 10
#' b <- 100
#' x
"%<d-%" <- function(x, value) {
  name <- substitute(x)
  value <- substitute(value)

  if (!is.name(name)) stop("Left-hand side must be a name")

  env <- parent.frame()
  call <- substitute(delayedAssign(deparse(name), value,
    eval.env = env, assign.env = env), list(value = value))
  eval(call)

  invisible()
}
