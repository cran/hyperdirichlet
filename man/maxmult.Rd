\name{maxmult}
\Rdversion{1.1}
\alias{maxmult}
\title{
Estimation of the multivariate beta distribution
}
\description{
Gives a maximum likelihood estimate for the parameters of a Dirichlet
distribution, on the basis of datapoints drawn from a multivariate beta
distribution
}
\usage{
maxmult(M, start_a = NULL, give = FALSE, method = "nlm", ...)
}
\arguments{
  \item{M}{Integer matrix whose rows give multinomial observations}
  \item{start_a}{Start point for optimization, with default \code{NULL}
    being interpreted as Mosimann's formula 29}
  \item{give}{Boolean, with default \code{FALSE} meaning to return just
    the point estimate and \code{TRUE} meaning to return all the output
    from the optimization routine}
  \item{method}{Text string specifying the optimization routine to use.
    Two values coded:  default \code{nlm} means to use
    \code{nlm()} and \code{optim} meaning to use \code{optim()};
    anything else means to return Mosimann's estimate (equation 29)}
  \item{\dots}{Further arguments passed to  \code{nlm()} or \code{optim()}}
}
\details{
 Finds the maximum likelihood estimate from the equation 7 of Mosimann
 1962. 
}
\references{
  J. E. Mosimann 1962.  \dQuote{On the compound multinomial distribution,
    the multivariate \eqn{\beta}{beta}-distribution, and correlations
    among proportions}.  \emph{Biometrika}, volume 49, numbers 1 and 2,
  pp65-82.
}

\author{
Robin K. S. Hankin
}
\note{
The \code{nlm()} function appears to be better suited to this problem
than \code{optim()}.
}

\seealso{\code{\link{pollen}}}
\examples{
data(pollen)
maxmult(pollen, start_a=c(51.81, 0.987, 5.332, 1.961))
}
