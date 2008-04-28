\name{hyperdirichlet-class}
\docType{class}
\alias{hyperdirichlet-class}
\title{Class ``hyperdirichlet'' }
\description{A generalization of the Dirichlet distribution}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{hyperdirichlet(...)}.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Numeric vector of length a power of two,
      holding the parameters (sic; not the powers) of the distribution}
    \item{\code{NC}:}{The normalizing constant, if known}
    \item{\code{pnames}:}{Character vector holding the names of the columns}
    \item{\code{validated}:}{Boolean, indicating whether the
      distribution is known to be normalizable}
  }
}
\section{Methods}{
  \describe{
    \item{NC}{\code{signature(object = "hyperdirichlet")}:
      normalizing constant}
    \item{params}{\code{signature(object = "hyperdirichlet")}: 
      accessor method for parameters}
    \item{pnames}{\code{signature(object = "hyperdirichlet")}: 
      accessor method for pnames}
    \item{validated}{\code{signature(x = "hyperdirichlet")}: 
      returns \code{TRUE} if the distribution is known to be normalized}
    \item{pnames<-}{\code{signature(object = "hyperdirichlet")}: 
      set method for pnames}
    \item{dim}{\code{signature(x = "hyperdirichlet")}: 
      Dimensionality of the distribution}
    \item{mean}{\code{signature(object = "hyperdirichlet")}: 
      returns the mean of the distribution}
    \item{as.hyperdirichlet}{\code{signature(object = "hyperdirichlet")}: 
      Coerce to hyperdirichlet form}
    \item{head}{\code{signature(object = "hyperdirichlet")}: 
      head method; prints the first few lines of the parameters}
    \item{tail}{\code{signature(object = "hyperdirichlet")}: 
      head method; prints the last few lines of the parameters}
    \item{show}{\code{signature(object = "hyperdirichlet")}:
      prints the object}
  }
}
\author{Robin K. S. Hankin; based on \code{vglm-class.Rd} from the VGAM package}
\seealso{\code{\link{hyperdirichlet}}}

\examples{
dput(uniform(3,pnames=letters[1:3]))
}
\keyword{classes}
