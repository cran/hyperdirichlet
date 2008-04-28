\name{paircomp}
\alias{paircomp}
\title{Hyperdirichlet distribution for repeated Bernoulli trials}
\description{
  Hyperdirichlet distribution with nonzero coefficients for terms
  consisting of a pair of \eqn{p}s. 
}
\usage{
paircomp(x)
}
\arguments{
  \item{x}{A square matrix.  If players \eqn{i} and \eqn{j} play, with
    \eqn{i<j}, then player \eqn{j} wins \code{x[i,j]} trials and player
    \eqn{i} wins \code{x[j,i]} trials}
}
\details{
  The \acronym{PDF} is
  \deqn{\propto
    \prod_{i<j}
    \frac{
      p_i^{M[j,i]} p_j^{M[i,j]}
    }{
      \left(p_i+p_j\right)^{M[i,j]+M[j,i]}
    }
  }
}
\value{
Returns a hyperdirichlet distribution (without normalizing factor) of
dimension \code{nrow(x)}.

}
\author{Robin K. S. Hankin}
\note{
Elements on the diagonal of \code{x} are silently ignored.
  }
\seealso{\code{\link{justpairs}}}
\examples{
paircomp(matrix(1:9,6,6))
}
\keyword{math}

