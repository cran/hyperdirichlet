\name{params}
\alias{params}
\alias{params,hyperdirichlet-method}
\alias{powers}
\title{Parameters of the hyperdirichlet distribution}
\description{
Parameters of the hyperdirichlet distribution
}
\usage{
params(x)
powers(x)
}

\arguments{
  \item{x}{Object of class hyperdirichlet}
}
\details{

  Function \code{powers()} lists the powers of the various combinations;
  function \code{params()} lists the parameters.

  The two functions differ only in single-p combinations (by one).

}
\value{
  These functions return a vector of length \code{2^dim(x)}.
  }
\note{
  The default print method gives powers and parameters.
}
\examples{

a <- gd(1:3,3:1)
params(a)
powers(a)

}

