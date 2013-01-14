\name{curry}
\alias{curry}
\title{Curry (partially apply) a function.}
\usage{
  curry(fname, ..., env = parent.frame())
}
\arguments{
  \item{f}{function as string or quoted name}
}
\description{
  Curry (partially apply) a function.
}
\examples{
# All of these ways of specifying a function work:
curry("mean", na.rm = TRUE)
curry(quote(mean), na.rm = TRUE)
x <- quote(mean)
curry(x, na.rm = TRUE)
}

