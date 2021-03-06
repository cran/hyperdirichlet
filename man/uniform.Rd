\name{uniform}
\alias{uniform}
\title{A uniform Dirichlet distribution}
\description{
A uniform Dirichlet distribution
}
\usage{
uniform(n, pnames=NULL)
}
\arguments{
  \item{n}{The dimension of the distribution}
  \item{pnames}{A character vector giving the names of the columns}
}
\value{
  Returns a hyperdirichlet object corresponding to a uniform
  distribution
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{dirichlet}}}
\examples{

uniform(4)

uniform(4) + dirichlet(1:4)   # identical (but the normalizing constant is lost)


# Two ways to do the same thing:
rhyperdirichlet(n=11, uniform(4))
t(replicate(11,diff(c(0,sort(runif(3)),1))))



}
