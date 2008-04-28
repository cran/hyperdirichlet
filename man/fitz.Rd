\name{fitz}
\alias{fitz}
\title{Fitzmaurice}
\description{
  Hyperdirichlet distribution corresponding to a dataset of Fitzmaurice et al
}
\usage{
fitz(dat , include.missing=TRUE , validated=NULL)
}
\arguments{
  \item{dat}{A vector corresponding to either male or female data}
  \item{include.missing}{Boolean, with default \code{TRUE} meaning to
    return the likelihood function for the data including the missing
    cases, and \code{FALSE} meaning to include only the data
    corresponding to complete cases}
  \item{validated}{Boolean, with \code{TRUE} meaning to omit the checks
    (OK if all elements of \code{dat} are non-negative) and \code{FALSE} meaning
    to check them all (time-consuming)}
}
\details{
Fitzmaurice considered childhood obesity.  See the reference for further details.
}
\value{
Returns a hyperdirichlet distribution (without normalizing factor)
corresponding to the observations, either male or female,  made by Fitzmaurice et al.
}
\references{
G. M. Fitzmaurice, N. M. Laird, and S. R. Lipsitz 1994.
\dQuote{Analysing incomplete longitudinal binary responses: a
  likelihood-based approach}. \emph{Biometrics}, volume 50, pp601-612.
  }
\author{Robin K. S. Hankin}
\note{
  Pat Altham originally spotted that this dataset could be represented
  using the hyperdirichlet distribution.
  
  The functional form for replacement \dQuote{\code{[<-}()} is used
  because it is possible to set the \code{validated} argument to
  \code{TRUE}: this suppresses the computationally intensive checking
  that the distribution is proper.

  \strong{warning.}  Setting \code{validated} to \code{TRUE} is not
  recommended in general.  It is OK here because the function knows that
  no elements of \code{dat} is negative.
    
  }
\examples{
  boys  <- c(20,7,9,8, 8, 8,15,150,13,3,2,42,3,1, 6,16,11,1,3,38,14,55,4,33,7,45)
  girls <- c(21,6,6,2,19,13,14,154, 8,1,4,47,4,0,16, 3,11,1,3,25,13,39,5,23,7,47)

  male <- fitz(boys)
  maximum_likelihood(male)

}

