\name{Extract.brob}
\alias{[.hyperdirichlet}
\alias{[<-.hyperdirichlet}
\alias{[,hyperdirichlet-method}
\alias{[<-,hyperdirichlet-method}
\alias{extract}
\title{Extract or Replace parameters of a hyperdirichlet object}
\description{
   Methods for \code{"["} and  \code{"[<-"}, i.e., extraction or
   subsetting of hyperdirichlet objects.
 }
 \section{Methods}{
   \itemize{
     \item x[i]
     \item x[i] <- value
   }
 }
\arguments{
  \item{x}{Object of class hyperdirichlet}
  \item{i}{elements to extract or replace}
  \item{value}{replacement value}
}
\value{
  Always returns an object of class hyperdirichlet.
}
\author{Robin K. S. Hankin}
\note{
The replacement method \dQuote{\code{[<-}} invokes the validity checking
method \code{.hd.valid()} by default; this may be time-consuming for
hyperdirichlet objects of high dimension.

Users may use the functional form for replacement to override this
behaviour; the method takes \code{recalculate} and a \code{validated}
arguments, defaulting to \code{FALSE}, that may be set if desired.
}
\examples{
a <- uniform(4)
a[5] <- 1.2

a <- as.hyperdirichlet(a, TRUE)  # recommended way to calculate NC

}
\keyword{math}
