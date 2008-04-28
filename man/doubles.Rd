\name{doubles}
\alias{doubles}
\alias{doubles_noghost}
\docType{data}
\title{Match outcomes from repeated doubles tennis matches}
\description{
  Match outcomes from repeated doubles tennis matches
}
\usage{data(doubles)}
\format{
A hyperdirichlet object corresponding to the match outcomes listed
below.
}
\details{

There are four players, $p_1$ to~$p_4$.  These players play doubles
tennis matches with the following results:

\tabular{cl}{
  match \tab score\cr\hline
  $\{p_1,p_2\}$ vs $\{p_3,p_4\}$ \tab 9-2\cr
  $\{p_1,p_3\}$ vs $\{p_2,p_4\}$ \tab 4-4\cr
  $\{p_1,p_4\}$ vs $\{p_2,p_3\}$ \tab 6-7\cr
  $\{p_1\}$     vs $\{p_3\}$     \tab 10-14\cr
  $\{p_2\}$     vs $\{p_3\}$     \tab 12-14\cr
  $\{p_1\}$     vs $\{p_4\}$     \tab 10-14\cr
  $\{p_2\}$     vs $\{p_4\}$     \tab 11-10\cr
  $\{p_3\}$     vs $\{p_4\}$     \tab 13-13\cr
}

It is suspected that p1 and p2 have some form of team cohesion and play
better when paired than when either solo or with other players.  As the
scores show, each player and, apart from p1-p2, each doubles
partnership, is of approximately the same strength.

Dataset \code{doubles_noghost} gives the appropriate likelihood function
for the players' strengths; and dataset \code{doubles} gives the
appropriate likelihood function if the extra strength due to 
p1-p2's team cohesion is represented by a ghost player.
}

\source{
 Doubles tennis matches at \acronym{NOCS}, Jan-May 2008
}
\examples{
data(doubles)
}
\keyword{datasets}
