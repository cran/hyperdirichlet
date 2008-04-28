\name{head}
\docType{methods}
\alias{head}
\alias{head,hyperdirichlet-method}
\alias{tail,hyperdirichlet-method}
\title{Head and tail}
\description{
Print the first few, or last few, lines of a hyperdirichlet object
}
\usage{
\S4method{head}{hyperdirichlet}(x, n = 6, ...)
\S4method{tail}{hyperdirichlet}(x, n = 6, ...)
}
\arguments{
  \item{x}{object of class hyperdirichlet}
  \item{n}{number of lines to print as per same argument in
    \code{head()} and \code{tail()}}
  \item{\dots}{Further arguments passed to \code{head()} or \code{tail()}}
}
\details{
  Prints the head or tail of the \code{binmat()} matrix and then prints
  the normalizing constant.
}
\value{
  The functions documented here coerce to a matrix,  then return the
  appropriate rows (as a matrix).
}
  
\author{Robin K. S. Hankin}
\seealso{\code{\link{binmat}}}
\examples{
head(dirichlet(1:6))
}
