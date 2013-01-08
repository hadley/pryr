\name{\%<a-\%}
\alias{\%<a-\%}
\title{Create an active binding.}
\usage{
  %<a-%(x, value)
}
\arguments{
  \item{x}{unquoted expression naming variable to create}

  \item{value}{unquoted expression to evaluate every time
  \code{name} is accessed}
}
\description{
  Infix form of \code{\link{makeActiveBinding}} which
  creates an \emph{active} binding between a name and an
  expression: every time the name is accessed the
  expression is recomputed.
}
\examples{
x \%<a-\% runif(1)
x
x
x \%<a-\% runif(10)
x
x
rm(x)
}

