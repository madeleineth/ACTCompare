% From ACTCompare, (c) 2010 Madeleine Thompson

\name{act.ci.plot}

\alias{act.ci.plot}

\title{Visualize ACT method confidence intervals}

\description{Plots ACT estimates and confidence intervals from a
  single method.}

\usage{act.ci.plot(data)}

\arguments{
  \item{data}{A subset of ACT comparison data frame in the form
    returned by \code{\link{act.method.compare}}.  This subset
    should only contain results from one method.}
}

\details{
  This function is used to determine whether confidence intervals
  from a particular ACT estimation method are reasonable.

  It creates a set of plots where each plot cell represents a single
  series, the subsequence length is plotted on the horizontal axis,
  the ACT estimate is plotted on the vertical axis, and the true
  ACT is plotted as a dashed horizontal line.  95\% confidence
  intervals are represented as vertical bars extending from the
  symbols.  The bars for proper confidence intervals will cover the
  dashed line 95\% of the time.  The plot symbols are generated the
  same way as with \code{\link{act.compare.plot}}.
}

\value{
  A \pkg{ggplot2} plot object.  If \code{act.ci.plot} is called
  in a script, this will need to be passed to \code{print}.
}

\seealso{
  \code{\link{act.method.compare}},
  \code{\link{act.compare.plot}}
}

\examples{
  \dontrun{
act.sim <- act.method.compare(...)
print(act.ci.plot(subset(act.sim, method=='AR process')))}
}
