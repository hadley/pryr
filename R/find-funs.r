#' Find functions matching criteria.
#'
#' This is a flexible function that matches function component against
#' a regular expression, returning the name of the function if there are any
#' matches. \code{fun_args} and \code{fun_calls} are helper functions that
#' make it possible to search for functions with specified argument names, or
#' which call certain functions.
#'
#' @param env environment in which to search for functions
#' @param extract component of function to extract. Should be a function that
#'   takes a function as input as returns a character vector as output,
#'   like \code{fun_calls} or \code{fun_args}.
#' @param pattern \pkg{stringr} regular expression to results of \code{extract}
#'   function. Use \code{\link[string]{fixed}} or
#'   \code{\link[string]{ignore.case}} to control the behaviour of the regexp.
#' @param obj function object to inspect.
#' @export
#' @examples
#' library(stringr)
#' find_funs("package:base", fun_calls, fixed("match.fun"))
#' find_funs("package:stats", fun_args, "^[A-Z]+$")
#'
#' fun_calls(match.call)
#' fun_calls(write.csv)
find_funs <- function(env = parent.frame(), extract, pattern) {
  env <- to_env(env)
  if (length(pattern) > 1) pattern <- str_c(pattern, collapse = "|")

  test <- function(x) {
    f <- get(x, env)
    if (!is.function(f)) return(FALSE)

    any(str_detect(extract(f), pattern))
  }

  fs <- ls(env)
  Filter(test, fs)
}

#' @export
#' @rdname find_funs
fun_calls <- function(obj) {
  if (is.function(obj)) {
    fun_calls(body(obj))
  } else if (is.call(obj)) {
    f <- as.character(obj[[1]])
    unique(c(f, unlist(lapply(obj[-1], fun_calls), use.names = FALSE)))
  }
}

#' @export
#' @rdname find_funs
fun_args <- function(x) names(formals(x))
