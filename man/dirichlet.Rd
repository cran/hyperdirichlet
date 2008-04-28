\name{gd}
\alias{gd}
\alias{Generalized Dirichlet}
\alias{dirichlet}
\alias{is.dirichlet}
\alias{dirichlet_params}
\alias{dirichlet_params<-}
\alias{Dirichlet}
\title{The Dirichlet and generalized Dirichlet distribution}
\description{
  Specify a Dirichlet or generalized Dirichlet distribution as a special
  case of the hyperdirichlet distribution
}
\usage{
dirichlet(params, powers, pnames)
is.dirichlet(x)
dirichlet_params(x)
dirichlet_params(x) <- value
gd(a, b, b0 = 0, pnames = NULL)
}
\arguments{
  \item{params,powers}{Numeric vectors (supply exactly one) specifying
    the parameters or the powers respectively of the Dirichlet
    distribution}
  \item{x}{Object of class hyperdirichlet}
  \item{value}{Numeric vector}
  \item{a,b}{Numeric vectors of the same length specifying the
    parameters of the generalized Dirichlet distribution}
  \item{b0}{Arbitrary constant for the generalized Dirichlet
    distribution}
  \item{pnames}{Character vector for name of the hyperdirichlet object}
}
\details{
  Function \code{dirichlet()} returns the hyperdirichlet distribution
  corresponding to the classical Dirichlet distribution.  If the vector
  \code{params|powers} is a named vector, then the hyperdirichlet object
  inherits the names (but the names are ignored if argument
  \code{pnames} is supplied).

  Function \code{is.dirichlet(x)} returns \code{TRUE} or \code{FALSE}
  according to whether the hyperdirichlet object \code{x} is a Dirichlet
  distribution.

  Function \code{dirichlet_params()} returns the Dirichlet parameters of
  a hyperdirichlet object.
  
  Function \code{gd()} returns the hyperdirichlet distribution
  corresponding to the generalized Dirichlet distribution of Connor and
  Mosimann.

  For convenience,  the generalized Dirichlet distribution is described
  here.  Connor and Mosimann 1969 give the \acronym{PDF} as
  \deqn{
    \left[\prod_{i=1}^{k-1}B\left(a_i,b_i\right)\right]^{-1}\,
    p_k^{b_{k-1}-1}\,
    \prod_{i=1}^{k-1}\left[p_i^{a_i-1}\left(\sum_{j=i}^k
    p_j\right)^{b_{i-1}-\left(a_i+b_i\right)}\right].
  }{ommitted...see a LaTeXed file}
  where \eqn{\sum_{i=1}^k p_i=1}{p_1+...+p_k=1} and \eqn{b_0} is
  arbitrary.  If \eqn{b_{i-1}=a_i+b_i} for \eqn{i=2,\ldots,
  k-1}{i=2,...,k-1} then the PDF reduces to a standard Dirichlet
  distribution with \eqn{\alpha_i=a_i}{alpha_i=a_i} for
  \eqn{i=1,\ldots,k-1}{i=1,...,k-1} and
  \eqn{\alpha_k=b_{k-1}}{alpha_k=b_{k-1}}. 

  Wong 1998 gives the algebraically equivalent form
  \deqn{
    \prod_{i=1}^k\frac{1}{B\left(\alpha_i,\beta_i\right)}
      x_i^{\alpha_i-1}
      \left(1-x_1-\cdots-x_i\right)^{\gamma_i}
    }{ommitted...see a LaTeXed file}
  for \eqn{x_1+x_2+\cdots+x_k\leq 1}{x_1+...+x_k <= 1} and \eqn{x_j\geq
  0}{x_j >= 0} for \eqn{j=1,2,\ldots,k}{j=1,2,...,k} and
  \eqn{\gamma_j=\beta_j-\beta_{j+1}}{gamma_j=beta_j-beta_{j+1}} for
  \eqn{j=1,2,\ldots,k-1}{j=1,2,...,k-1} and
  \eqn{\gamma_k=\beta_k-1}{gamma_k=beta_{k-1}}.  

  Here,
  \eqn{B(x,y)=\Gamma(x)\Gamma(y)/\Gamma(x+y)}{B(x,y)=G(x)G(y)/G(x+y)} is
  the beta function.
  
}
\value{
  Functions \code{dirichlet()} and \code{gd()} return a hyperdirichlet
  object; function \code{is.dirichlet()} returns a logical; and function
  \code{dirichlet_params()} returns a numeric vector.
}
\references{
  \itemize{
    \item R. J. Connor and J. E. Mosimann 1969.  \emph{Concepts of
      independence for proportions with a generalization of the
      Dirichlet distribution}.  Journal of the American Statistical
    Association, volume 64, number 325, pp194-206
    \item T-T Wong 1998. \emph{Generalized Dirichlet distribution in
      Bayesian analysis}.  Applied Mathematics and Computation, volume
    97, pp165-181
  }
}
\author{Robin K. S. Hankin}
\note{
  These functions have cheaply evaluated analytic expressions for the
  normalizing constant.

  If a hyperdirichlet object corresponds to a Dirichlet distribution,
  this is relatively easy to detect [using \code{is.dirichlet()}].
  However, the corresponding case for the generalized Dirichlet
  distribution is not yet coded up, owing to the non-neutrality of the
  GD.
  
}
\seealso{\code{\link{justpairs}}}
\examples{

a <- dirichlet(1:4 , pnames=letters[1:4])
is.dirichlet(a)  # should be TRUE
dirichlet(dirichlet_params(a)) # should be 'a'

gd(1:5,5:1)
}
\keyword{math}
