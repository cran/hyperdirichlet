\name{print}
\alias{print.hyperdirichlet}
\alias{print.hyperdirichlet}
\alias{dim}
\alias{show}
\alias{dim,hyperdirichlet-method}
\alias{show,hyperdirichlet-method}
\title{Print and various S4 methods for hyperdirichlet objects}
\description{
  Print method for hyperdirichlet objects
}
\usage{
 print.hyperdirichlet(x, n = 0, do_head = TRUE, ...)
 \S4method{dim}{hyperdirichlet}(x)
 \S4method{show}{hyperdirichlet}(object)
}
\arguments{
  \item{x,object}{Object of class hyperdirichlet}
  \item{n}{Integer passed to base \code{head()} or \code{tail()}}
  \item{do_head}{Boolean argument ignored if \code{n} takes its default
    value of \code{0}.  If \code{n<>0}, argument \code{do_head} taking
    default \code{TRUE} means to pass control to \code{head(x,n,...)} and
    \code{FALSE} means to pass control to \code{tail(x,n,...)}}
  \item{\dots}{Further arguments passed to \code{head()} or
    \code{tail()} as necessary}
}
\details{

The matrix itself is printed using internal functions
\code{.print_hyperdirichlet()} and
\code{.print_hyperdirichlet_worker()}, which are not intended for the
end user.

}
\author{Robin K. S. Hankin}
\seealso{\code{\link{binmat}}}
\examples{
gd(1:3,3:1)
}
