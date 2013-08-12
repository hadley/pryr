\name{otype}
\alias{otype}
\title{Determine object type.}
\usage{
  otype(x)
}
\description{
  Determine object type.
}
\details{
  Figure out which object system an object belongs to:

  \itemize{ \item primitive: no class attribute \item S3:
  class attribute, but not S4 \item S4: \code{\link{isS4}},
  but not RC \item RC: inherits from "refClass" }
}
\examples{
otype(data.frame())
otype(1:10)
}
\seealso{
  Other object inspection: \code{\link{address}},
  \code{\link{ftype}}, \code{\link{typename}}
}

