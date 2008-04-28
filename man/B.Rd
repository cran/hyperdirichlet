\name{B}
\alias{B}
\alias{NC}
\alias{calculate_B}
\alias{probability}
\alias{NC,hyperdirichlet-method}
\alias{mgf}
\alias{mean}
\alias{mean,hyperdirichlet-method}
\alias{is.proper}
\alias{validated}
\alias{isvalidated}
\alias{validated,hyperdirichlet-method}
\title{Normalizing constant for the hyperdirichlet distribution}
\description{
Uses numerical techniques for calculating the normalizing constant for the hyperdirichlet distribution
}
\usage{
B(x, ...)
NC(x)
calculate_B(x, disallowed=NULL, give=FALSE, ...)
probability(x, disallowed, ...)
mgf(x, powers, ...)
mean(x, ...)
is.proper(x,irregardless)
validated(x)
}
\arguments{
  \item{x}{Object of class \dQuote{hyperdirichlet} (or coerced thereto)}
  \item{powers}{Vector of length \code{dim(x)} whose elements are the
    powers of the expectation; see details section}
  \item{irregardless}{Boolean; see details section}
  \item{disallowed}{Function specifying a subset of the simplex
    over which to integrate; default \code{NULL} means to integrate over
    the whole simplex.  The integration proceeds over \code{p} with
    \code{disallowed(p)} evaluating to \code{FALSE}}
  \item{give}{Boolean, with default \code{FALSE} meaning to return the
    value of the integral and \code{TRUE} meaning to return the full
    output of \code{adapt()}}
  \item{\dots}{Further arguments passed to \code{adapt()}}
}
\details{
  \itemize{
    \item Function \code{B()} is the user-friendly version.  It accesses
    the \code{NC} slot.  If not \code{NA}, the value is returned; if
    \code{NA}, the normalizing constant is calculated using the
    \code{adapt()} package, via \code{calculate_B()}.
    
    \item Function \code{NC()} is not intended for the user.  It is used
    internally as an accessor method for the \code{NC} slot, and this
    value is returned indiscriminately.

    \item Function \code{calculate_B()} is the engine which actually
    does the work.  Observe how \eqn{p} is converted to \code{e} (by
    \code{e_to_p()}) and the integral proceeds over a hypercube.
    Function \code{dirichlet()} and \code{gd()} do not use this as the
    normalizing constant has an analytical expression and this is used
    instead.

    \item Function \code{probability()} gives the probability of an
    observation from a hyperdirichlet distribution satisfying
    \code{!disallowed(p)}.
    
    \item Function \code{mgf()} is the moment generating function,
    taking an argument that specifies the powers of \code{p} needed: the
    expectation of \eqn{\prod_{i=1}^n {p_i}^{{\rm powers}[i]}}{prod
    p^powers} is returned.

    \item Function \code{mean()} returns the mean value of the
    hyperdirichlet distribution.  This is computationally slow (consider
    \code{maximum_likelihood()} for a measure of central tendency).  The
    function takes a \code{normalize} argument, not passed to
    \code{adapt()}: this is Boolean with \code{FALSE} meaning to return
    the value found by integration directly, and default \code{TRUE}
    meaning to normalize so the sum is exactly 1

    \item Function \code{is.proper()} checks a hyperdirichlet
    distribution for being normalizable: a \dQuote{proper}
    hyperdirichlet object has a finite integral and therefore can be
    normalized.  This function is quite time-consuming for
    hyperdirichlet distributions of large dimension.

    The \code{irregardless} argument to function \code{is.proper()} is
    Boolean, with \code{TRUE} meaning to carry out the checks whatever
    the value of slot \code{@validated} [that is, \code{validated(x)}].
    Default \code{FALSE} means that function \code{is.proper()} returns
    \code{TRUE} if \code{@validated} is \code{TRUE} and to carry out the
    check otherwise.  Use this argument to force \code{is.proper()} to
    carry out a check even if not strictly necessary.

    \item Function \code{validated()} is an accessor method for the
    \code{@validated} slot of hyperdirichlet object.  It returns a
    Boolean variable with \code{TRUE} meaning that the object is
    \strong{known} to be \dQuote{proper} (ie \code{is.proper(x)} returns
    \code{TRUE}), so it is normalizable, even if the normalization
    constant is not known.  This flag is present because many
    hyperdirichlet objects of interest are known \emph{a priori} to be
    proper, so executing \code{is.proper()} would be unnecessary.
  }
}
\value{
  Functions \code{B()}, \code{NC()}, \code{calculate_NC()} notionally
  return a scalar: the normalization constant

  Functions \code{mean()} and \code{mgf()} return a \eqn{k}-tuple

  Functions \code{is.proper()} and \code{validated()} return a Boolean

  Function \code{probability()} returns a scalar, a probability.
}
\author{Robin K. S. Hankin}

\note{

 The adapt package is no longer available on CRAN: so the \code{adapt()}
function is not available either. 

You may be able to install the adapt package notwithstanding its
availability on CRAN or is license.  If you are happy with this (I am),
install the adapt package and everything should work.

I am working on providing a replacement for adapt(), but this is low on
my list of priorities.  Sorry about this.

}

\seealso{\code{\link{hyperdirichlet}}}
\examples{

a <- hyperdirichlet(c(4,3,6,5,4,3,2,1))
\dontrun{
B(a)                                    # Not recommended
a <- as.hyperdirichlet(a,TRUE)          # Recommended

is.proper(a)

mgf(a,powers=1:3)    # expectation of p1^1 * p2^2 * p3^3
}

}
