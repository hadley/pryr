\name{rls}
\alias{rls}
\title{Recursive ls.}
\usage{
  rls(e = parent.frame(), all.names = TRUE)
}
\arguments{
  \item{e}{environment to start the search at. Defaults to
  the \code{\link{parent.frame}}}

  \item{all.names}{Show all names, even those starting with
  \code{.}?  Defaults to \code{TRUE}, the opposite of
  \code{\link{ls}}}
}
\description{
  Performs \code{\link{ls}} all the way up to a top-level
  environment (either the parent of the global environment,
  the empty environment or a namespace environment).
}
\author{
  Winston Chang
}

