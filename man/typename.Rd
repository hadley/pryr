\name{typename}
\alias{typename}
\title{Determine the C typename associated with a name.}
\usage{
  typename(x)
}
\arguments{
  \item{x}{name of object to inspect. This can not be a
  value.}
}
\description{
  Determine the C typename associated with a name.
}
\examples{
x <- 1:10
typename(x)
y <- 1L
typename(y)
z <- list(1:10)
typename(z)
delayedAssign("a", 1 + 2)
typename(a)
a
typename(a)
}
\seealso{
  Other object inspection: \code{\link{address}},
  \code{\link{ftype}}, \code{\link{inspect}},
  \code{\link{otype}}, \code{\link{refs}},
  \code{\link{typename}}
}

