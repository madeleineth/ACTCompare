% From ACTCompare, (c) 2010 Madeleine Thompson

\name{generate.act.series}

\alias{generate.act.series}

\title{Generate all series defined by ACTCompare}

\description{Generates seven series included in \pkg{ACTCompare}
  as a test suite for methods for computing autocorrelation times.}

\usage{generate.act.series(series.length=1e6)}

\arguments{
  \item{series.length}{The length of the series to generate.
    \code{1e6} is a reasonable value for a real test; \code{1000} is
    a reasonable value for a test run.}
}

\details{
  \code{generate.act.series} generates the seven series comprising
  the test suite provided by \pkg{ACTCompare} and returns them in
  a list.  This list can be passed as the \code{series} paramater
  to \code{\link{act.method.compare}}.  The series are described
  in more detail in \dQuote{A Comparison of Methods for Computing
  Autocorrelation Time} (available in \file{doc/act-methods.pdf}).
}

\value{
  A list of seven series in the form expected by
  \code{\link{act.method.compare}}.

  The return value of this function is always the same, and the
  function runs slowly, but the data takes up 45M compressed when
  \code{series.length} is the default value, so it is not reasonable
  to include it with the package as a data set.
}

\seealso{
  \code{\link{act.method.compare}}
}
