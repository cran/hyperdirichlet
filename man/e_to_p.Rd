\name{e_to_p}
\alias{e_to_p}
\alias{p_to_e}
\alias{Jacobian}
\title{Transform from a simplex to a hypercube}
\description{
  Transform from a simplex (either regular or right-angled) to a
  hypercube.  
}
\usage{
e_to_p(e)
p_to_e(p)
Jacobian(e)
}
\arguments{
  \item{e}{Vector with all elements between 0 and 1 (hypercube)}
  \item{p}{Vector of positive elements whose sum is either equal to, or
    less than, one (simplex)}
}
\details{
  Function \code{e_to_p()} takes one from \code{e}-space to
  \code{p}-space.

  Function \code{p_to_e()} takes one from \code{p}-space to
  \code{e}-space.  This is useful when integrating over a simplex; use
  \code{Jacobian()} to evaluate the Jacobian of the transform.



  Forward transformation:
  
  \deqn{e_1=\sum_{i=1}^d p_i}

  \deqn{e_i=\frac{p_{i-1}}{\sum_{j=i-1}^d p_j},\qquad 2\leq i <=
  d}{e_i=\frac{p_{i-1}}{\sum_{j=i-1}^d p_j}, 2 <= i <= d}

  Backward transformation:

  \deqn{p_1=e_1 e_2}
  
  \deqn{p_i=e_1e_{i+1}\prod_{i=2}^{i}\left(1-e_i\right),\qquad 2\leq
  i\leq d}{p_i=e_1 e_{i+1} \prod_{i=2}^{i}(1-e_i), 2 <= i <= d}


  Jacobian:
  
  \deqn{J=\prod_{i=2}^{d-1}\left(1-e_i\right)^{d-i}}{J=\prod_{i=2}^{d-1}(1-e_i)^{d-i}}
  
}
\value{
  The functions documented here return a scalar.
}
\references{
  M. Evans and T. Swartz 2000. \emph{Approximating Integrals via Monte
    Carlo and Deterministic Methods}, Oxford University Press; page 28
  }
\author{Robin K. S. Hankin}
\note{
To do a regular simplex, use the \dQuote{di} of the right-angled
simplex; see the examples.
}
\seealso{\code{\link{dhyperdirichlet}}}
\examples{

\dontrun{
# First, try to calculate the volume of a regular 4-simplex:
adapt(5,rep(0,5),rep(1,5),functn=function(x){Jacobian(c(1,x))})
# Should be close to 1/5! = 1/120 ~= 0.008333
# (that was the 'di trick')

# Now, try to calculate the volume of a triangular-based pyramid:
adapt(3,rep(0,3),rep(1,3),functn=Jacobian)
# Should be close to 1/8=0.125
}


}

