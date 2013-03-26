\name{find_funs}
\alias{find_funs}
\alias{fun_args}
\alias{fun_body}
\alias{fun_calls}
\title{Find functions matching criteria.}
\usage{
  find_funs(env = parent.frame(), extract, pattern)

  fun_calls(f)

  fun_args(f)

  fun_body(f)
}
\arguments{
  \item{env}{environment in which to search for functions}

  \item{extract}{component of function to extract. Should
  be a function that takes a function as input as returns a
  character vector as output, like \code{fun_calls} or
  \code{fun_args}.}

  \item{pattern}{\pkg{stringr} regular expression to
  results of \code{extract} function. Use
  \code{\link[string]{fixed}} or
  \code{\link[string]{ignore.case}} to control the
  behaviour of the regexp.}

  \item{obj}{function object to inspect.}
}
\description{
  This is a flexible function that matches function
  component against a regular expression, returning the
  name of the function if there are any matches.
  \code{fun_args} and \code{fun_calls} are helper functions
  that make it possible to search for functions with
  specified argument names, or which call certain
  functions.
}
\examples{
library(stringr)
find_funs("package:base", fun_calls, fixed("match.fun"))
find_funs("package:stats", fun_args, "^[A-Z]+$")

fun_calls(match.call)
fun_calls(write.csv)

fun_body(write.csv)
find_funs("package:utils", fun_body, fixed("write"))
}

