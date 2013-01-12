\name{subs}
\alias{subs}
\title{Substitute names for values.}
\usage{
  subs(expr, env)
}
\arguments{
  \item{expr}{a language object, function, expression or
  list of the above to modify}

  \item{env}{an environment-like object containing named
  values to subsitute.}
}
\description{
  This function is a specialised version of
  \code{\link{substitute}} that only substitutes names for
  values.
}
\examples{
subs(quote(a + 1), list(a = 2))
f <- function(z = x) y * z
g <- subs(f, list(x = quote(runif(1)), y = 2, z = quote(x)))
}

