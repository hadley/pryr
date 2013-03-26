\name{\%<a-\%}
\alias{\%<a-\%}
\alias{\%<c-\%}
\title{Create an active binding.}
\usage{
  %<a-%(x, value)

  %<c-%(x, value)
}
\arguments{
  \item{x}{unquoted expression naming variable to create}

  \item{value}{unquoted expression to evaluate every time
  \code{name} is accessed}

  \item{x}{unquoted expression naming variable to create}

  \item{value}{constant value}
}
\description{
  Infix form of \code{\link{makeActiveBinding}} which
  creates an \emph{active} binding between a name and an
  expression: every time the name is accessed the
  expression is recomputed.

  Infix wrapper for \code{\link{assign}} +
  \code{\link{lockBinding}} that creates a constant: a
  binding whose value can not be changed.
}
\examples{
x \%<a-\% runif(1)
x
x
x \%<a-\% runif(10)
x
x
rm(x)
x \%<c-\% 10
#' Generates an error:
\dontrun{x <- 20}

# Note that because of R's operator precedence rules, you
# need to wrap compound RHS expressions in ()
y \%<c-\% 1 + 2
y
z \%<c-\% (1 + 2)
z
}

