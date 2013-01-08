\name{\%<d-\%}
\alias{\%<d-\%}
\title{Create an delayed binding.}
\usage{
  %<d-%(x, value)
}
\arguments{
  \item{x}{unquoted expression naming variable to create}

  \item{value}{unquoted expression to evaluate the first
  time \code{name} is accessed}
}
\description{
  Infix form of \code{\link{delayedBinding}} which creates
  an \emph{delayed} or lazy binding, which only evaluates
  the expression the first time it is used.
}
\examples{
x \%<d-\% a + b
a <- 10
b <- 100
x
}

