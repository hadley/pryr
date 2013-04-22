\name{dots}
\alias{dots}
\alias{named_dots}
\title{Capture unevaluated dots.}
\usage{
  dots(...)

  named_dots(...)
}
\arguments{
  \item{...}{}
}
\value{
  a list of expressions (not expression objects).
  \code{named_dots} will use the deparsed expressions as
  default names.
}
\description{
  Capture unevaluated dots.
}
\examples{
y <- 2
str(dots(x = 1, y, z = ))
str(named_dots(x = 1, y, z =))
}

