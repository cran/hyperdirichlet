\name{binmat}
\alias{binmat}
\title{Create a matrix of all binary combinations}
\description{
Create a matrix of all binary combinations for use with the
hyperdirichlet distribution
}
\usage{
binmat(n, alternatives = NULL, pnames=NULL)
}
\arguments{
  \item{n}{Number of binary digits}
  \item{alternatives}{The alternatives; prototypically \code{TRUE} and
    \code{FALSE}, but default \code{NULL}, taken to mean \code{0} and
    \code{1}, is easier on
    the eye}
  \item{pnames}{Optional vector specifying the column names; default of
    \code{NULL} means to use \code{p1},\code{p2} etc}
}
\value{
  Function \code{binmat(n)} returns an integer matrix of zeros and ones
  with \eqn{2^n} rows and \eqn{n} columns.  In binary, the rows count
  from \eqn{0} to \eqn{2^n-1}.
}
  
\author{Robin K. S. Hankin}
\seealso{\code{\link{print.hyperdirichlet}}}
\examples{
binmat(4)

binmat(3,alternatives=c(TRUE,FALSE))
}
\keyword{math}
