\name{pollen}
\Rdversion{1.1}
\alias{pollen}
\docType{data}
\title{
Serum data from Mosimann 1962
}
\description{
Data from Mosimann 1962 detailing forest pollen counts 
}
\usage{data(pollen)}
\format{
A matrix with four columns and 76 rows.
}
\details{
  The rows each sum to 100; the values are counts of four different
  types of pollen.  Each row corresponds to a different level in the
  core; the levels are in sequence with the first row being most
  recent and the last row being the oldest.
}
\references{
J. E. Mosimann 1962.  \dQuote{On the compound multinomial distribution,
  the multivariate \eqn{\beta}{beta}-distribution, and correlations
  among proportions}.  \emph{Biometrika}, volume 49, numbers 1 and 2,
pp65-82.
}
\seealso{\code{\link{serum}}}
\examples{
data(pollen)

func <- function(x,l){
ifelse(any(l<0),Inf,
 lfactorial(sum(x)) -sum(lfactorial(x))
    +lgamma(sum(l)) +sum(lgamma  (x+l))
    -sum(lgamma(l)) -lgamma(sum  (x+l))
)
}

start_vec <- c(51.6, 1, 5.3 , 2)

optim(start_vec , function(l){ -sum(apply(pollen , 1, FUN=func,l=l))})





}
\keyword{datasets}
