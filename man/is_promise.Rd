\name{is_promise}
\alias{is_promise}
\alias{promise_info}
\title{Promise info}
\usage{
  is_promise(x)

  promise_info(x)
}
\description{
  Promise info
}
\examples{
x <- 10
is_promise(x)
(function(x) is_promise(x))(x = 10)
}
\seealso{
  Other promise.tools: \code{\link{uneval}}
}

