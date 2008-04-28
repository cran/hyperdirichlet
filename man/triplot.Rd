\name{triplot}
\alias{triplot}
\title{Plot density of a 3D hyperdirichlet distribution}
\description{
  Plot (log) density of a 3D hyperdirichlet distribution
}
\usage{
triplot(HD, l = 100, do_image=TRUE, do_contour=TRUE, discard = 0.05, ...)
}
\arguments{
  \item{HD}{Either a function of a three-element vector, or a hyperdirichlet object}
  \item{l}{Size of plot; larger values look much better but take longer}
  \item{do_image,do_contour}{Boolean, indicating whether to plot the
    image and contour respectively}
  \item{discard}{Numeric; default 0.05 means to disregard densities less than
    the fifth percentile (\emph{i.e.} quantile 0.05).  This makes the contour
    plot intervals prettier}
  \item{\dots}{Further arguments passed to \code{image()} \emph{and}
    \code{contour()}}
}
\value{
  The function returns (invisibly) an \code{l}-by-\code{l} matrix
  holding the support as a function of the two independent variables,
  with \code{NA} outside the domain.
}
\details{

  If argument \code{HD} is not a function it is interpreted as a
  hyperdirichlet object.  If \code{HD} is a function, \code{triplot()}
  will pass it a three-element vector.
  
  If argument \code{HD} is a hyperdirichlet object, then
  \code{triplot()} plots contours of the log of the density (ie
  support).  If you want likelihood instead, see examples section for
  how to do this.
}
  
\author{Robin K. S. Hankin}
\examples{

a <- dirichlet(1:3)
triplot(a,l=20)

f <- function(p){1-p[1]+p[2]+8*p[2]^2*p[3]}
triplot(f)

g <- function(p){dhyperdirichlet(p, a, log=FALSE)}
triplot(g,l=20)


}
