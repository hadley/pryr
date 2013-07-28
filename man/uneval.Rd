\name{uneval}
\alias{uneval}
\title{Capture the call associated with a promise.}
\usage{
  uneval(x)
}
\description{
  This is an alternative to subsitute that performs one
  job, and so gives a stronger signal regarding the
  intention of your code.  It returns an error if the name
  is not associated with a promise.
}
\examples{
f <- function(x) {
   uneval(x)
}
f(a + b)
f(1 + 4)

delayedAssign("x", 1 + 4)
uneval(x)
x
uneval(x)
}

