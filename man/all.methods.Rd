% From ACTCompare, (c) 2010 Madeleine Thompson
% $Id: all.methods.Rd 1862 2010-10-24 13:13:36Z mthompson $

\name{all.methods}

\alias{all.methods}

\title{List of methods for computing ACT}

\description{A list of all \pkg{ACTCompare}-supplied methods for
  computing the autocorrelation time of a sequence.}

\usage{all.methods}

\details{
  \code{all.methods} contains four named elements: \code{AR process},
  \code{Spec. fit}, \code{ICS}, and \code{Batch means}, each implementing
  a method for computing an autocorrelation time (ACT) from a numeric
  vector representing a series.  It is intended to be passed as the
  \code{methods} argument to \code{\link{act.method.compare}}.

  \code{AR process} (Thompson 2010) models the series as an AR
  process.  \code{Spec. fit} (Plummer et al 2006, Heidelberger 1981)
  fits a linear regression to the log spectrum of the series.
  \code{ICS} (Geyer 1992, sec. 3.3) sums autocorrelation function
  values until the initial convex sequence goes to zero.  \code{Batch
  means} (Geyer 1992, sec. 3.2) estimates the ACT with the ratio
  of batch means to sample variance.
}

\references{
  Geyer, C. J. (1992). \dQuote{Practical Markov chain Monte Carlo.}
  \emph{Statistical Science}, 7(4):473-511.

  Heidelberger, P. and Welch, P. D. (1981). \dQuote{A spectral
  method for confidence interval generation and run length control
  in simulations.} \emph{Communications of the ACM}, 24(4):233-245.

  Plummer, M., Best, N., Cowles, K., and Vines, K. (2006). \dQuote{CODA:
  Convergence diagnosis and output analysis for MCMC.} \emph{R News},
  6(1):7-11.

  Thompson, M. B. (2010). \dQuote{Graphical comparison of MCMC performance.}
  In preparation.
}

\seealso{
  \code{\link{act.method.compare}}
}
