#' Tools for making promises explicit
#'
#' Deprecated: please use the lazyeval package instead.
#'
#' @param x expression to make explicit, or to evaluate.
#' @export
explicit <- function(x) {
  .Deprecated("Please use the lazyeval package instead")

  explicitPromise(substitute(x), parent.frame())
}

#' @rdname explicit
#' @export
#' @param data Data in which to evaluate code
#' @param env Enclosing environment to use if data is a list or data frame.
eval2 <- function(x, data = NULL, env = parent.frame()) {
  .Deprecated("Please use the lazyeval package instead")
  if (is.formula(x)) {
    env <- environment(x)
    x <- x[[2]] # RHS of the formula
  }

  if (is.atomic(x)) return(x)
  stopifnot(is.call(x) || is.name(x))

  if (!is.null(data)) {
    eval(x, data, env)
  } else {
    eval(x, env)
  }
}

is.formula <- function(x) inherits(x, "formula")
