% From ACTCompare, (c) 2010 Madeleine Thompson
% $Id: ACTCompare-package.Rd 1847 2010-10-19 20:58:15Z mthompson $

\name{ACTCompare-package}

\alias{ACTCompare-package}
\alias{ACTCompare}

\docType{package}

\title{A package for comparing methods for computing ACTs}

\description{A package for comparing methods for computing ACTs.}

\details{
  \pkg{ACTCompare} is a package for comparing methods for computing
  autocorrelation times (ACTs).  It includes seven test cases and
  four common methods.  It provides the \code{\link{act.method.compare}}
  function for comparing methods on a test set and the
  \code{\link{act.compare.plot}} and \code{\link{act.ci.plot}} functions
  for visualizing the results from \code{\link{act.method.compare}}.

  For a discussion of the included methods and test series, see
  \file{act-methods.pdf}, included in this package's \file{doc}
  directory.
}

\examples{
\dontrun{
all.series <- generate.act.series(1e6)
save(all.series, file='act-series.rda')
act.sim <- act.method.compare(all.methods, series, 10^1:5)
print(act.compare.plot(act.sim))
print(act.ci.plot(subset(act.sim, method=='AR process')))}
}
