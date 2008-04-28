\name{volleyball}
\alias{volleyball}
\alias{vb}
\alias{volleyball_results}
\alias{vb_synthetic}
\docType{data}
\title{Results from the NOCS volleyball league}
\description{
  
Results from the \acronym{NOCS} volleyball league.  Object
\code{volleyball_results} is a matrix in which each column corresponds
to a player and each row corresponds to a volleyball set;
\code{vb} is the corresponding likelihood function in the form
of a hyperdirichlet distribution.

Object \code{vb_synthetic} is a hyperdirichlet object
corresponding to a synthetic dataset obtained from 4000 simulated
volleyball sets.

}
\usage{data(volleyball)}

\details{
A volleyball \dfn{set} is a Bernoulli trial between two disjoint subsets of
the players.  The two subsets are denoted (after the game) as the
\dQuote{winners} and the \dQuote{losers}: these are denoted by \code{1}
and \code{0} respectively.

Thus the first line reads of \code{volleyball_results} reads:
\preformatted{
 p1  p2  p3  p4  p5  p6  p7  p8  p9 
 1    0  NA   1   0   0  NA   1  NA
}

showing that the teams were \code{p1}, \code{p4} and \code{p8} against
\code{p2}, \code{p5} and \code{p6}; players \code{p3}, \code{p7} and
\code{p9} did not play.

Dataset \code{vb_synthetic} is the likelihood function of 4000 simulated
trials in which the skills are distributed according to Zipf's law:
\code{1/(1:9)/sum(1/(1:9))}.

These datasets illustrate the fact that such Bernoulli trials are only
weakly informative.  The synthetic dataset involves 4000 observations
because this was about the minimum number for which one could estimate
the probabilities reasonably reliably.  Even then, the lowest
probabilities are poorly identified.

}
\source{
 Volleyball games at \acronym{NOCS}, 2006-2008
}
\references{
  Robin K. S. Hankin (2010).
  \dQuote{A Generalization of the Dirichlet Distribution},
  \emph{Journal of Statistical Software}, 33(11), 1-18,
  \url{http://www.jstatsoft.org/v33/i11/}
}
\examples{
data(volleyball)
maximum_likelihood(vb , start_p = c(0.407, 0.091, 0.432, 1.73e-05,
2.24e-08, 1.9e-05, 1.8e-07, 0.03, 0.039) , control=list(maxit=100))


zipf <- 1/seq_len(9)
maximum_likelihood(vb_synthetic , start_p=zipf, control=list(maxit=100))

}
\keyword{datasets}
