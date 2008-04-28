\name{serum}
\Rdversion{1.1}
\alias{serum}
\docType{data}
\title{
Serum data from Mosimann 1962
}
\description{
Data from Mosimann 1962 detailing serum proteins in white Pekin
ducklings.
}
\usage{data(serum)}
\format{
A matrix with three columns and 23 rows.
}
\details{
  The rows each sum to 1; the values are the relative frequencies of
  serum proteins in white Pekin ducklings as determined by
  electrophores. 
}
\references{
J. E. Mosimann 1962.  \dQuote{On the compound multinomial distribution,
  the multivariate \eqn{\beta}{beta}-distribution, and correlations
  among proportions}.  \emph{Biometrika}, volume 49, numbers 1 and 2,
pp65-82.
}
\examples{
data(serum)
dhyperdirichlet(serum , HD=dirichlet(4:6),log=TRUE)

f <- function(x){
  if(any(x<0)){
    return(Inf)
  } else {
    return(-sum(dhyperdirichlet(serum , HD=dirichlet(x),include.NC=TRUE,log=TRUE)))
  }
}

fish <- optim(c(3.21,20.38,21.68),f)
triplot(HD=dirichlet(fish$par),l=60,main="increase 'l' for better plot quality")
points(serum[,1]+serum[,2]/2,serum[,2]*sqrt(3)/2,pch=16)


}
\keyword{datasets}
