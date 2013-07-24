\name{address}
\alias{address}
\title{Return the memory address of an R object.}
\usage{
  address(x, env = parent.frame())
}
\arguments{
  \item{x}{object to inspect}
}
\description{
  This is useful when determining if an object is modified
  or not. This function uses non-standard evaluation to
  avoid incrementing the reference counter for the object,
  which distorts investigation of in-place modification.
}
\examples{
x <- 1:5
address(x)
\dontrun{.Internal(inspect(x))}
x[1] <- 3L
address(x)
}

