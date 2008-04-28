\name{hyperdirichlet-package}
\alias{hyperdirichlet-package}
\docType{package}
\title{
The Hyperdirichlet package
}
\description{
A generalization of the Dirichlet distribution
}
\details{
\tabular{ll}{
Package: \tab hyperdirichlet\cr
Type: \tab Package\cr
Version: \tab 1.1-8\cr
Date: \tab 2008-03-26\cr
License: \tab GPL\cr
}

This package provides a generalization of the Dirichlet distribution
that is useful for analyzing multinomial trials with \emph{a priori}
restrictions.

As an example, consider six people (\dQuote{players}), numbered 1 to 6.
These players are members of a running club and regularly race one
another.

Each player has an associated number \eqn{p_1} to \eqn{p_6}, with
\eqn{0\leq p_i\leq 1}{0 <= p_i <= 1} for \eqn{i=1,\ldots,6}{1,...,6} and
\eqn{\sum_{i=1}^6 p_i=1}{p_1 + ... + p_6=1}.  If all six take part in a
race, then the probability that player \eqn{i} wins is simply \eqn{p_i}.

We wish to make inferences about the \eqn{p_i} from their performances.

If all six race and \eqn{p_i} wins \eqn{n_i}, then the likelihood
function is just

\deqn{
  {p_1}^{n_1}\cdot{p_2}^{n_2}\cdot{p_3}^{n_3}\cdot{p_4}^{n_4}\cdot{p_5}^{n_5}\cdot{p_6}^{n_6}.
}{
  p1^n1 * p2^n2 * p3^n3 * p4^n4 * p5^n5 * p6^n6.}

With a uniform prior, the posterior is Dirichlet.


The players now have a race but only \eqn{p_1}, \eqn{p_2} and \eqn{p_3}
take place, winning \eqn{r_1}, \eqn{r_2} and \eqn{r_3} respectively.
The likelihood function is then

\deqn{
  \frac{
    {p_1}^{n_1+r_1}\cdot{p_2}^{n_2+r_2}\cdot{p_3}^{n_3+r_3}\cdot{p_4}^{n_4}\cdot{p_5}^{n_5}\cdot{p_6}^{n_6}
  }{
    \left(p_1+p_2+p_3\right)^{r_1+r_2+r_3
    }
  }
}{
  p1^n1 * p2^n2 * p3^n3 * p4^n4 * p5^n5 * p6^n6 / (p1+p2+p3)^(r1+r2+r3)
}

This distribution is not a Dirichlet distribution but is representable
in this package; the R idiom would be

\code{
  jj <- dirichlet(powers = c(5,4,3,5,3,2))
}
\code{
  jj <- jj + mult_restricted_obs(6, 1:3, c(4,5,2))
}

where the first line specifies a Dirichlet distribution for the all-play
data and the second line augments the likelihood with the observations
from the restricted race.

}
\author{
Robin K. S. Hankin
}
\keyword{ package }
\examples{

  jj <- dirichlet(powers = c(5,4,3,5,3,2))
  jj <- jj + mult_restricted_obs(6, 1:3, c(4,5,2))

data(icons)
maximum_likelihood(as.hyperdirichlet(icons))

}
