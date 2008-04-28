\name{maximum_likelihood}
\alias{maximum_likelihood}
\alias{maximum.likelihood}
\alias{mle}
\alias{mle_restricted}
\title{Maximum likelihood point for the hyperdirichlet distribution}
\description{
  Maximum likelihood point for the hyperdirichlet distribution as
  estimated using numerical maximization.
}
\usage{
maximum_likelihood(HD, start_p = NULL, give = FALSE, disallowed = NULL, zero=NULL, ...)
               mle(HD, start_p = NULL, give = FALSE, disallowed = NULL,            ...)
    mle_restricted(HD, start_p = NULL, give = FALSE, disallowed = NULL, zero=NULL, ...)
}
\arguments{
  \item{HD}{Object of class hyperdirichlet}
  \item{start_p}{Start value for the \code{p}s.  See details section}
  \item{give}{Boolean with default \code{FALSE} meaning to return just
    the point estimate and \code{TRUE} meaning to return all the output
    from \code{optim()}}
  \item{disallowed}{A function of \code{p} returning a Boolean to
    restrict the search for the MLE.  See examples}
  \item{zero}{In function \code{maxlike_restricted()}, a Boolean vector
    with \code{TRUE} elements corresponding to components that are
    constrained to be zero.  See details section}
  \item{\dots}{Further arguments sent to \code{optim()}}
}
\details{

The user should use function \code{maximum_likelihood()}, which is a
user-friendly wrapper for one of the two functions (\code{mle()} or
\code{mle_rst()}) depending on whether argument \code{zero} is or is not
\code{NULL}.
  
Argument \code{start_p} specifies the start point for the optimization;
default \code{NULL} is interpreted as \code{rep(1/n,n)} where \code{n}
is \code{dim(HD)} (ie neutral position).

It is not necessary to normalize \code{start_p}: this is done by
\code{dhyperdirichlet()}. 

Non-default values for this argument are interpreted by
\code{dhyperdirichlet()}.

Argument \code{zero}, if not default \code{NULL}, is Boolean in the
standard case; but if it is not Boolean, it is interpreted as a numeric
vector of integers indicating which components of the distribution are
restricted to zero.  An example is given below.
  
}
\value{
  Returns a \eqn{k}-tuple.
}
\note{
  The functions minimize \code{-dhyperdirichlet(...,log=TRUE)}; so there
  is no need to set \code{fnscale}.

  Be aware that the \pkg{aylmer} package includes a function
  \code{maxlike()}, which does something different.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{dhyperdirichlet_p}},\code{\link{optim}}}
\examples{
maximum_likelihood(dirichlet(1:4))      # Should be 0:3

jj.numerical <- maximum_likelihood(dirichlet(3:8), zero=2:3)$MLE

jj <- c(2,0,0,5,6,7)
jj.analytical <- jj/sum(jj)

jj.numerical - jj.analytical  # should be small


}
