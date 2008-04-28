\name{pnames}
\alias{pnames}
\alias{pnames<-}
\alias{pnames,hyperdirichlet-method}
\alias{pnames<-,hyperdirichlet-method}
\title{Names for the columns}
\description{
  Get and set column names
}
\usage{
pnames(x)
pnames(x) <- value
}
\arguments{
  \item{x}{Object of class hyperdirichlet}
  \item{value}{Names of the columns}
}
\author{Robin K. S. Hankin}
\note{
The default is to leave the columns unnamed; the print method puts them
there for printing if null.
}

\examples{
a <- dirichlet(1:3)
pnames(a) <- letters[1:3]
a
}
