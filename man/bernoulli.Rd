\name{bernoulli}
\alias{bernoulli}
\alias{bernoulli_obs}
\alias{mult_restricted_obs}
\alias{obs}
\alias{single_bernoulli_obs}
\alias{mult_bernoulli_obs}
\alias{single_multi_restricted_obs}
\alias{single_obs}
\title{Hyperdirichlet distributions for various types of informative trials}
\description{
Hyperdirichlet distributions for various types of informative trials
including Bernoulli and multinomial
}
\usage{
single_obs(d,n)
obs(x)
single_multi_restricted_obs(d,n,x)
mult_restricted_obs(d, a, nobs)
mult_bernoulli_obs(d,team1,team2,wins1,wins2)
single_bernoulli_obs(d,win,lose)
bernoulli_obs(d, winners, losers)
}
\arguments{
  \item{d}{Dimension of the distribution}
  \item{n}{Number of the winner}
  \item{x}{Summary statistic}
  \item{a,win,lose,winners,losers,nobs,team1,team2,wins1,wins2}{Arguments as detailed below}
}
\details{

  These functions give likelihood functions for various observations.
  In the following, the paradigm is \code{d} players and the object of
  inference is \eqn{p=(p_1,\ldots,p_d)}{p=(p_1...p_d)} (the
  \dQuote{skills}) with \eqn{\sum p_i=1}{sum(p_i)=1}.  Different types
  of observation are possible.

  The most informative is the unrestricted, uncensored case in which all
  \code{d} players play and the winner is identified unambiguously
  (\code{single_obs()}).  However, other observations are possible, as
  detailed below:
  
  \itemize{
    \item \code{single_obs(d,n)}.  Single multinomial trial: \code{d}
    players, and player \code{n} wins.
    \item \code{obs(x)}.  Repeated multinomial trials: \code{sum(x)}
    trials, each amongst \code{length(x)} players, with 
    player \code{i} winning \code{x[i]} games (which might be zero)
    \item \code{single_multi_restricted_obs(d,n,x)}.  Single restricted
    multinomial trial: \code{d} players, player \code{n} wins,
    conditional on the winner being one of \code{x[1]}, \code{x[2]}, etc
    \item \code{mult_restricted_obs(d,a,nobs)}.  Multiple restricted
    multinomial trials: \code{d} players, conditional on winners being 
    \code{a[1]}, \code{a[2]}, etc.  Player \code{a[i]} wins
    \code{nobs[i]} times for \eqn{1\leq i\leq d}{1 <= i <= d}
    \item \code{mult_bernoulli_obs(d,team1,team2,wins1,wins2)}.
    Multiple Bernoulli trials between \code{team1} and \code{team2} with
    \code{team1} winning \code{wins1} and \code{team2} winning
    \code{wins2}
    \item \code{single_bernoulli_obs(d,win,lose)}.  Single Bernoulli
    trial: \code{d} players, with two teams (\code{win} and
    \code{lose}).  The winning team comprises \code{win[1]},
    \code{win[2]}, etc and the losing team comprises \code{lose[1]},
    \code{lose[2]}, etc.
    \item \code{bernoulli_obs(d, winners, losers)} Repeated Bernoulli
    trials: \code{d} players.  Here \code{winners} and \code{losers} are
    lists of the same length; the elements are a team as in
    \code{single_bernoulli_obs()} above.   Thus game \code{i} was
    between \code{winners[[i]]} and \code{losers[[i]]} and, of course,
    \code{winners[[i]]} won.
  }

See examples section.

}
\author{Robin K. S. Hankin}
\note{The hyperdirichlet distributions returned by the functions
  documented here may be added (using \dQuote{\code{+}}) to concatenate
  independent observations. 

  
}
\examples{
# Five players, some results:

jj1 <- obs(1:5)                             # five players, player 'i' wins 'i' games.
jj2 <- single_obs(5,2)                      # open game, p2 wins
jj3 <- single_multi_restricted_obs(5,2,1:3) # match: 1,2,3; p2 wins
jj4 <- mult_restricted_obs(5,1:2,c(0,4))    # match: 1,2, p1 wins 2 games, p2 wins 3
jj5 <- single_bernoulli_obs(5,1:2,3:5)      # match: 1&2 vs 3&4&5; 1&2 win
jj6 <- mult_bernoulli_obs(6, 1:2,c(3,5), 7,8) # match: 1&2 vs 3&5; 1&2 win 7, 3&5 win 8
jj6 <- bernoulli_obs(5,list(1:2,1:2), list(3,3:5)) # 1&2 beat 3; 1&2 beat 3&4&5


# Now imagine that jj1-jj6 are independent observations:

ans <- jj1 + jj2 + jj3 + jj4 + jj5 + jj6  #posterior PDF with uniform prior likelihood

}
