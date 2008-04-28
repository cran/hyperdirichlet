\name{hd_add}
\alias{hd_add}
\title{Add two hyperdirichlet distributions}
\description{
Given two hyperdirichlet distributions, \dQuote{add} them, in the sense
of concatenating their information, assumed to be independent.
}
\usage{
hd_add(e1, e2, assume_validated = FALSE)
}
\arguments{
  \item{e1,e2}{Hyperdirichlet distributions of the same dimension}
  \item{assume_validated}{Boolean, with default \code{FALSE} meaning
    that the returned sum cannot be assumed to be proper; so the
    returned hyperdirichlet object is tested with \code{is.proper()}
    (which is time-consuming).  Set to \code{TRUE} only when you \emph{know}
    that the sum is proper}
}
\details{
  Think of this function as a computerized embodiment of Bayes's theorem
  with \code{e1} representing the prior and \code{e2} representing one
  or more informative trials.
  
  The basic guts of the function is \code{hyperdirichlet(powers(e1) +
  params(e2))}.  Note that this is equivalent to
  \code{hyperdirichlet(params(e1) + powers(e2))}.

  The functional form is not really intended for the end user; use
  \code{e1 + e2} instead (but observe that the sum will be validated
  using \code{is.proper()}, which may take a long time).

}
\value{
  Returns a hyperdirichlet distribution
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{hyperdirichlet}},\code{\link{is.proper}},\code{\link{Arith}}}
\examples{

dirichlet(1:4) + gd(c(0.1 , 0.3 , 0.5), c(0.2 , 0.4 , 0.9))

uniform(4) + single_bernoulli_obs(4,1,2)

data(chess)
ch <- as.hyperdirichlet(chess)
stopifnot(all(params(ch+ch+ch) == params(ch*3)))

}
