\name{compose}
\alias{\%.\%}
\alias{compose}
\title{Compose multiple functions}
\usage{
  compose(...)

  %.%(f, g)
}
\arguments{
  \item{...}{n functions to apply in order from right to
  left}

  \item{f,g}{two functions to compose for the infix form}
}
\description{
  In infix and prefix forms.
}
\examples{
not_null <- `!` \%.\% is.null
not_null(4)
not_null(NULL)
}

