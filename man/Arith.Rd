\name{Arith-methods}
\docType{methods}
\alias{Arith-methods}
\alias{Arith,hyperdirichlet,missing-method}
\alias{Arith,hyperdirichlet,ANY-method}
\alias{Arith,ANY,hyperdirichlet-method}
\alias{Arith,hyperdirichlet,numeric-method}
\alias{Arith,numeric,hyperdirichlet-method}
\alias{Arith,hyperdirichlet,hyperdirichlet-method}
\title{Arith methods for hyperdirichlet objects}
\description{

  Methods for Arithmetic functions on hyperdirichlet objects.  There are
  only two operations:
  \itemize{
    \item Add two of the same dimensions, as in \sQuote{\code{a+b}}
    \item Multiply a hyperdirichlet object by a length-one scalar, as in
    \sQuote{\code{a*5}} 
  }
  Both these operations result in the normalization constant becoming
  unknown.

  No unary operations are defined.
}
\author{Robin K. S. Hankin}
\note{
  The package delegates \sQuote{\code{a+b}} to \code{hd_add()}, which
  the user should use if more control is desired.
}

\value{
  Arithmetic functions on hyperdirichlet objects return hyperdirichlet
  objects.  Multiplication operates on the powers (not parameters), thus
  it is defined so that \code{a+a = 2*a = a*2}; associativity means that
  \code{a+...+a} (\eqn{n} copies) equals \code{n*a}.
}

  
  
\seealso{\code{\link{hd_add}}}  
\examples{
gd(1:3,1:3) + dirichlet(1:4)

dirichlet(1:4) * 10

}
\keyword{methods}
\keyword{math}
