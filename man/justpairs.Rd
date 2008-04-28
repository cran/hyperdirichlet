\name{justpairs}
\alias{justpairs}
\title{Hyperdirichlet distribution with nonzero coefficients for just
  the pairs}
\description{
  Hyperdirichlet distribution with nonzero coefficients for terms
  consisting of a pair of \eqn{p}s. 
}
\usage{
justpairs(x)
}
\arguments{
  \item{x}{A square matrix.  Upper triangular elements correspond to
    pairs of included \eqn{p}s, and lower triangular elements correspond
    to pairs of excluded \eqn{p}s}
}
\details{
  The \acronym{PDF} is

  \deqn{
    \prod_{i<j}\left(p_i+p_j\right)^{M[i,j]}
    \left(1-p_i-p_j\right)^{M[j,i]}}
  
}
\value{
Returns a hyperdirichlet object (without normalizing factor) of
dimension \code{nrow(x)}.


}
\author{Robin K. S. Hankin}
\note{The case where \code{nrow(x)==4} is treated specially.  In this
  case, the lower triangular elements are discarded, with a warning.}
\seealso{\code{\link{paircomp}}}
\examples{
justpairs(matrix(1:25,5,5))
}
\keyword{math}

