\name{dhyperdirichlet}
\alias{dhyperdirichlet}
\alias{dhyperdirichlet_e}
\alias{dhyperdirichlet_p}
\alias{rhyperdirichlet}
\title{Probability density function for, and random sampling from, the hyperdirichlet distribution}
\description{
Probability density function for the hyperdirichlet distribution in
terms of either \eqn{p} or \eqn{e}; and random sampling using Metropolis-Hastings
}
\usage{
dhyperdirichlet_e(e, HD, include.Jacobian = TRUE)
dhyperdirichlet(p, HD, include.NC = FALSE, TINY = 1e-10, log = FALSE)
rhyperdirichlet(n, HD, start=NULL, sigma=NULL)
}
\arguments{
  \item{HD}{Object of class \code{hyperdirichlet}, or coerced thereto}
  \item{p}{Vector of length \code{dim(HD)}, notionally summing to one}
  \item{e}{Vector of length \code{dim(HD)} giving the point in \code{e}-space}
  \item{include.Jacobian}{In function \code{dhyperdiriclet_e()},
    Boolean with default \code{TRUE} meaning to include the Jabobian of
    the transform from \code{e} to \code{p}}
  \item{include.NC}{In function \code{dhyperdirichlet_e()}, Boolean with
    \code{TRUE} meaning to include the normalization factor and default
    \code{FALSE} meaning not to include it (it is expensive to
    calculate).  Note that if the normalizing factor is not known, the
    function will return \code{NA}}
  \item{TINY}{In function \code{dhyperdirichlet_p()}, numeric,
    specifying minimum size for elements of \code{p} via \code{p <-
      pmax(p , TINY)}}
  \item{log}{In function \code{dhyperdirichlet_p()}, Boolean with 
    default \code{FALSE} meaning to return the probability density and
    \code{TRUE} meaning to return its logarithm}
  \item{n,start,sigma}{In function \code{rhyperdirichlet()}, \code{n} is
    the number of observations to take, \code{start} is the start-point
    for the random walk (with default \code{NULL} meaning to use the
    neutral point), and \code{sigma}is the standard deviation for the
    (Gaussian) kernel, with default \code{NULL} meaning to use
    \eqn{1/d}}
}
\details{
  Function \code{dhyperdirichlet()} gives the density as a function of
  the \eqn{p_1,p_2,\ldots,p_d}{p_1, ..., p_d}.

  Function \code{dhyperdirichlet_e()} gives the density as a function of
  the \eqn{e_i}.  This is useful when integrating as the simplex (in
  p-space) transforms to a hypercube in e-space.
}
\value{
  Functions \code{dhyperdirichlet()} and \code{dhyperdirichlet_e()}
  return a scalar; function \code{rhyperdirichlet()} returns a matrix
  whose rows are \eqn{k}-tuples
}
  
\author{Robin K. S. Hankin}
\note{

  Function \code{rhyperdirichlet()} uses a Metropolis-Hastings algorithm
  to construct a Markov chain.  Note that successive observations are
  not independent of one another.  The details of this nonindependence
  are poorly understood (by me, at any rate).  The examples section
  below shows how to generate a single random observation from the
  hyperdirichlet distribution.
  
  Function \code{dhyperdirichlet()} silently normalizes \code{p} by
  \code{p <- p/sum(p)}.  Currently, no check for the elements being
  positive is made (negative elements are nullified by \code{TINY}).

  The relationship between \eqn{e} and \eqn{p} is given in
  \code{e_to_p.Rd}. 
  
}
\seealso{\code{\link{maximum.likelihood}},\code{\link{e_to_p}}}
\examples{

dhyperdirichlet(c(1,4,3,2)/10, dirichlet(1:4))

rhyperdirichlet(20, dirichlet(1:3))

diff(c(0,sort(runif(9)),1))  # random sample drawn from dirichlet(rep(1,10))

# how to draw a single observation from the hyperdirichlet:
f <- function(HD,n,...){rhyperdirichlet(n,HD=HD,...)[n,,drop=TRUE]}
f(dirichlet(1:3),n=100)
f(dirichlet(1:3),n=100)
f(dirichlet(1:3),n=100)
# Note, n=100 might not be enough burn-in.


}
