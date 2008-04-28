\name{matrix_to_HD}
\alias{matrix_to_HD}
\alias{bernoulli_matrix_to_HD}
\alias{multinomial_matrix_to_HD}
\title{Coerce matrices to hyperdirichlet objects}
\description{
Coerce matrices to hyperdirichlet objects.  These functions are not
intended for the user (use \code{as.hyperdirichlet()} instead).
}
\usage{
matrix_to_HD(x, calculate_NC = FALSE, bernoulli = NULL, ...)
bernoulli_matrix_to_HD(x, calculate_NC = FALSE, ...)
multinomial_matrix_to_HD(x, calculate_NC = FALSE, ...)
}
\arguments{
  \item{x}{Matrix to be coerced}
  \item{bernoulli}{In function \code{matrix_to_HD()}, Boolean with
    \code{TRUE} meaning that the matrix rows are to be interpreted as
    repeated Bernoulli trials and  \code{FALSE} meaning that they
    are interpreted as multinomial trials.  Default \code{NULL} means to
  use a simple heuristic to infer the desired behaviour}
  \item{calculate_NC}{Boolean, with default \code{FALSE} meaning that
    the normalization constant is not to be calculated}
  \item{\dots}{Further arguments passed to \code{as.hyperdirichlet()}
    (thence to \code{adapt()})}
}
\details{
  These functions are not intended for the user; use
  \code{as.hyperdirichlet()} directly if at all possible.

  Function \code{bernoulli_matrix_to_HD()} operates on rows.  Each row
  has entries corresponding to the columns (the \dQuote{players}).  Each
  is a Bernoulli trial with three types of entry: \code{NA} for not
  playing, \code{1} for \sQuote{on the winning side} and \code{0} for
  \sQuote{on the losing side}.  Thus the Bernoulli trial is between
  \code{which(x==0)} and \code{which(x==1)}, with the latter winning.  A
  warning is given unless there is at least one \code{1} and at least one
  \code{0} on each row.

  Function \code{multinomial_matrix_to_HD()} also operates on rows.
  Each row corresponds to a series of restricted multinomial
  observations with likelihood given by \code{mult_restricted_obs()}
  (qv).
}
\author{Robin K. S. Hankin}
\seealso{\code{\link{mult_restricted_obs}}}
\examples{
data(icons)
matrix_to_HD(icons, bern=FALSE)
}
