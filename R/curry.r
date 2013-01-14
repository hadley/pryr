#' Curry (partially apply) a function.
#'
#' @param f function as string or quoted name
#' @export
#' @examples
#' # All of these ways of specifying a function work:
#' curry("mean", na.rm = TRUE)
#' curry(quote(mean), na.rm = TRUE)
#' x <- quote(mean)
#' curry(x, na.rm = TRUE)
curry <- function(fname, ..., env = parent.frame()) {
  if (is.character(fname)) {
    fname <- as.name(fname)
    f <- match.fun(fname)
  } else if (is.name(fname)) {
    f <- eval(fname, env)
  } else {
    stop("fname must be the name of a function", call. = FALSE)
  }
  args <- match.call(expand.dots = FALSE)$...

  f_args <- formals(f)
  curried_args <- f_args[setdiff(names(f_args), names(args))]
  call_args <- c(lapply(names(curried_args), as.name), args)
  curry_call <- as.call(c(list(fname), call_args))

  f <- make_function(curried_args, curry_call, env)
  environment(f) <- env
  f
}
