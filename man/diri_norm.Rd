\name{diri_norm}
\Rdversion{1.1}
\alias{diri_norm}
\alias{gd_norm}
\title{
Normalization constants for the Dirichlet and generalized Dirichlet
distributions
}
\description{
Normalization constants for the Dirichlet and generalized Dirichlet
distributions using computationally efficient methods
}
\usage{
diri_norm(x)
gd_norm(a,b)
}
\arguments{
  \item{x}{Vector of parameters in the Dirichlet distribution}
  \item{a,b}{Vectors of parameters in the generalized Dirichlet distribution}
}
\value{
  Returns the normalization constant:

  \deqn{
    \frac{
      \prod_{i=1}^{k}\Gamma\left(\alpha_i\right)
    }{
      \Gamma\left(\sum_{i=1}^{k}\alpha_i\right)
    }
  }{omitted: see a LaTeXed file}  

  for the Dirichlet and

  \deqn{
    \frac{\prod_{i=1}^{k}\Gamma\left(a_i\right)\Gamma\left(b_i\right)
    }{
      \prod_{i=1}^{k}\Gamma\left(a_i+b_i\right)
    }
  }{omitted: see a LaTeXed file}
  
for the generalized Dirichlet.
    
}
\references{
R. J. Connor and J. E. Mosimann 1969. \dQuote{Concepts of independence
  for proportions with a generalization of the Dirichlet distribution}.
\emph{Journal of the American Statistical Association}, volume 64,
number 325, pp194-206
}
\author{
Robin K. S. Hankin
}
\note{
The functions make use of the logarithmic form of the gamma and beta
functions to avoid overflow
}
\seealso{
\code{\link{dhyperdirichlet}}}
\examples{
diri_norm(runif(9))
gd_norm(1:3 , 3:1)
}
