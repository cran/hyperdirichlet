\name{hyperdirichlet}
\alias{hyperdirichlet}
\alias{as.hyperdirichlet}
\alias{is.hyperdirichlet}
\alias{as.hyperdirichlet,hyperdirichlet-method}
\alias{as.hyperdirichlet,numeric-method}
\alias{as.hyperdirichlet,matrix-method}
\alias{is.hyperdirichlet,hyperdirichlet-method}
\title{The Hyperdirichlet distribution}
\description{
Create, coerce to, or test for an object of class \code{hyperdirichlet}
}
\usage{
hyperdirichlet(x, NC, pnames = character(), validated=FALSE)
is.hyperdirichlet(x)
as.hyperdirichlet(x, calculate_NC = FALSE , ...)
}
\arguments{
  \item{x}{Object to be coerced or tested for}
  \item{NC}{Normalizing constant}
  \item{pnames}{names of the columns with length-0 default  resulting in
    the print method using column names \code{p1}, \code{p2}, etc}
  
  \item{validated}{Boolean.  Setting to \code{TRUE} is taken to mean
    that \code{x} is \strong{known} to be \sQuote{proper} (\emph{i.e.}
    \code{x} is normalizable), but the normalizing constant is not
    necessarily known.
    
    Default \code{FALSE} is taken to mean that \code{x} is not known to
    be proper: it is possible that \code{x} is not normalizable so
    cannot correspond to a \acronym{PDF}.

    Setting to \code{FALSE} means that the object will be passed to
    \code{is.proper()} for checking; this can be time-consuming.  The
    flag is set to \code{TRUE} \emph{ab initio} for \code{dirichlet()}
    and \code{gd()} because these distributions have an analytical
    expression for the normalizing constant}
  \item{calculate_NC}{Boolean, with default \code{FALSE} meaning not to
    calculate the normalizing constant and \code{TRUE} meaning to
    calculate it}
  \item{...}{Further arguments passed to \code{adapt()}}
}
\details{
  
  To determine the normalization constant, use something like \code{a <-
  as.hyperdirichlet(a,calculate_NC= TRUE)}.

  Matrices may be coerced to a hyperdichlet object using
  \code{as.hyperdirichlet()}: the call is dispatched to
  \code{matrix_to_HD()} (qv).

  If \code{x} is a matrix, be sure to specify the \code{bernoulli}
  argument, which is passed on to \code{matrix_to_HD()}
  
}
\value{
  Functions \code{hyperdirichlet()} and \code{as.hyperdirichlet()}
  return a hyperdirichlet object; function \code{is.hyperdirichlet()}
  returns a Boolean.
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{B}},\code{\link{extract}},\code{\link{matrix_to_HD}}}
\examples{

(a <- uniform(3))
a[c(TRUE,TRUE,FALSE)] <- 0.3
\dontrun{
(a <- as.hyperdirichlet(a, calculate_NC = TRUE))  # recommended way to calculate NC
}

(b <- dirichlet(1:3))
as.hyperdirichlet(b)          # "forgets" the normalizing constant

\dontrun{
as.hyperdirichlet(b, TRUE)    # recalculates NC; accuracy tolerable
                              # (analytic answer = 1/60)
}

\dontrun{  # takes a long time
op <- options()
options(warn = -1)
x <- dirichlet(rep(2,4)) + justpairs(matrix(1,4,4))
f <- function(p){p[1]>p[2]}
probability(x,f) # should be 0.5: distribution is symmetric
}

}


