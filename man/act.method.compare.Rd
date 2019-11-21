% From ACTCompare, (c) 2010 Madeleine Thompson

\name{act.method.compare}

\alias{act.method.compare}

\title{Compare methods for computing ACTs}

\description{Compares a collection of methods for computing
  autocorrelation times on a collection of series with known ACT.}

\usage{act.method.compare(methods, series, lengths, trace=TRUE)}

\arguments{
  \item{methods}{A list of methods to compare; see details for specifics.}

  \item{series}{A list of series on which to evaluate the methods;
    see details for specifics.}

  \item{lengths}{A vector of subsequence lengths of \code{series}
    to evaluate the methods on.  Each element must be less than or
    equal to the shortest series in \code{series}.}

  \item{trace}{A logical indicating whether to print status messages.}
}

\details{
  Each element of the \code{methods} argument should be a function
  of a single vector argument representing a series.  These functions
  should return either a scalar autocorrelation time estimate or a
  list containing elements \code{act} with a scalar ACT estimate
  and \code{act.025} and \code{act.975} with a 95\% confidence
  interval for the ACT.  The names of the elements of \code{methods}
  are used to distinguish the methods in the returned data.  The
  predefined variable \code{\link{all.methods}} is a valid value
  to pass for this parameter, and includes all the methods included
  in the \pkg{ACTCompare} package.

  Each element of the \code{series} argument should itself be a
  list with named elements \code{series}, \code{act}, and \code{name}.
  The \code{series} element should be a numeric vector.  The true
  autocorrelation time of this series should be stored as a scalar
  in the \code{act} element.  A name for the series should be stored
  in the \code{name} element.  For the full test suite, pass the
  results of \code{\link{generate.act.series}}.
}

\value{
  This function returns a data frame where each row corresponds to
  a single estimate of an autocorrelation time.  It has columns
  \code{method}, \code{series}, \code{length}, \code{true.act},
  \code{act}, \code{act.025}, and \code{act.975}.  \code{method}
  is the name of the method, taken from the list element name of
  the \code{methods} argument.  \code{series} is the \code{name}
  element of the series the method was run on.  \code{length} is
  the length of the subsequence of the series the method was passed.
  \code{true.act} is the true ACT of the full series, taken from
  the \code{act} element of the \code{series} argument.  \code{act}
  is the method's estimate of the ACT.  If the method returned a
  confidence interval, its endpoints are stored in \code{act.025}
  and \code{act.975}; otherwise, these are set to \code{NA}.

  \code{\link{act.compare.plot}} and \code{\link{act.ci.plot}} can
  visualize two aspects of this data frame; see their help pages
  for more information.
}

\seealso{
  \code{\link{act.compare.plot}},
  \code{\link{act.ci.plot}},
  \code{\link{generate.act.series}},
  \code{\link{all.methods}}
}
