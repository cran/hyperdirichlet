\encoding{UTF-8}
\name{paulino}
\alias{paulino}
\alias{Paulino}
\docType{data}
\title{Dataset used by Paulino}
\description{
A hyperdirichlet distribution for the dataset considered by Paulino and
de \enc{Bragança}{Braganca} Pereira (1995).
}
\usage{data(paulino)}

\details{
  Paulino and de \enc{Bragança}{Braganca} Pereiria considered 97
  subjects who each were one of three risk types: \dQuote{low},
  \dQuote{medium}, and \dQuote{high} (or 1, 2, 3).

  Of the 97 subjects, 51 were fully categorized: 14 were type 1, 17 type
  2, and 20 type 3; the remaining 46 were partly categorized: 28 were of
  types 1 or 2, and 18 were of types 2 or 3.

  The likelihood function is thus
  \deqn{
    p_1^{14}p_2^{17}p_3^{20}\left(p_1+p_2\right)^{28}\left(p_2+p_3\right)^{18}
  }{
    p1^14.p2^17.p3^20.(p1+p2)^28.(p2+p3)^18
  }
  
  and object \code{paulino} gives the \acronym{PDF}.

\strong{Note} the object gives the \emph{likelihood}, not the posterior
\acronym{PDF}.  Paulino and de \enc{Bragança}{Braganca} Pereira use
prior information of a type not readily representable in the
hyperdirichlet paradigm.

% The second author's name would be 'de Bragan\c{c}a Pererira' in LaTeX.
% It *might* be possible to use latin1 encoding but including c-cedilla
% in latin1 appears to be inimical to emacs.

}
\source{
  C. D. M. Paulino and C. A. de \enc{Bragança}{Braganca} Pereira 1995.
  \emph{Bayesian Methods for Categorical Data Under Informative General
  Censoring}, Biometrika, volume 82, number 2, pages 439-446
}
\examples{
data(paulino)
maximum_likelihood(paulino)
}
\keyword{datasets}
