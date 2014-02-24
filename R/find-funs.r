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
#'   function.
#' @param ... other arguments passed on to \code{\link{grepl}}
#' @export
#' @examples
#' find_funs("package:base", fun_calls, "match.fun", fixed = TRUE)
#' find_funs("package:stats", fun_args, "^[A-Z]+$")
#'
#' fun_calls(match.call)
#' fun_calls(write.csv)
#'
#' fun_body(write.csv)
#' find_funs("package:utils", fun_body, "write", fixed = TRUE)
find_funs <- function(env = parent.frame(), extract, pattern, ...) {
  env <- to_env(env)
  if (length(pattern) > 1) pattern <- str_c(pattern, collapse = "|")

  test <- function(x) {
    f <- get(x, env)
    if (!is.function(f)) return(FALSE)

    any(grepl(pattern, extract(f) ,...))
  }

  fs <- ls(env)
  Filter(test, fs)
}

#' @export
#' @rdname find_funs
#' @param f function to extract information from
fun_calls <- function(f) {
  if (is.function(f)) {
    fun_calls(body(f))
  } else if (is.call(f)) {
    fname <- as.character(f[[1]])

    # Calls inside .Internal are special and shouldn't be included
    if (identical(fname, ".Internal")) return(fname)

    unique(c(fname, unlist(lapply(f[-1], fun_calls), use.names = FALSE)))
  }
}

#' @export
#' @rdname find_funs
fun_args <- function(f) {
  stopifnot(is.function(f))
  names(formals(f))
}

#' @export
#' @rdname find_funs
fun_body <- function(f) deparse(body(f))
