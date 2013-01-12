\name{call_tree}
\alias{call_tree}
\title{Display a call (or expression) as a tree.}
\usage{
  call_tree(x, width = getOption("width"))
}
\arguments{
  \item{x}{quoted call, list of calls, or expression to
  display}

  \item{width}{displays width, defaults to current width as
  reported by \code{getOption("width")}}
}
\description{
  Display a call (or expression) as a tree.
}
\examples{
call_tree(quote(f(x, 1, g(), h(i()))))
call_tree(quote(if (TRUE) 3 else 4))
call_tree(expression(1, 2, 3))

fq <- quote(f <- function(a = 1, b = 2) {a + b})
call_tree(fq)
}

