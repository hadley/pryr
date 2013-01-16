\name{f}
\alias{f}
\title{A compact syntax for anonymous functions.}
\usage{
  f(..., .env = parent.frame())
}
\arguments{
  \item{...}{The last argument is the body of the function,
  all others are arguments to the function.  If there is
  only one argument, the formals are guessed from the
  code.}
}
\value{
  a function
}
\description{
  A compact syntax for anonymous functions.
}
\examples{
f(x + y)
f(x + y)(1, 10)
f(x, y = 2, x + y)

f({y <- runif(1); x + y})
}

