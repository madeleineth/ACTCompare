% From ACTCompare, (c) 2010 Madeleine Thompson

\name{act.compare.plot}

\alias{act.compare.plot}

\title{Visualize ACT method comparison}

\description{Plots ACT estimates against series, subsequence length,
  and method.}

\usage{act.compare.plot(data)}

\arguments{
  \item{data}{An ACT comparison data frame in the form returned by
    \code{\link{act.method.compare}}.}
}

\details{
  This function is used to compare the accuracy of various methods
  for estimating the autocorrelation time.

  It creates a set of plots where each plot cell represents a single
  series, the subsequence length is plotted on the horizontal axis,
  the ACT estimate is plotted on the vertical axis, and the true
  ACT is plotted as a dashed horizontal line.  Each estimate is
  plotted as a connected set of symbols.  Lines corresponding to
  more accurate methods will approach the dashed line fastest.

  By default, methods are represented by the first character of
  their name.  If two methods have the same first letter, one can
  use the \code{+} operator with \code{ggplot2::scale_shape_manual} to
  specify an alternate coding.
}

\value{
  A \pkg{ggplot2} plot object.  If \code{act.compare.plot} is called
  in a script, this will need to be passed to \code{print}.
}

\seealso{
  \code{\link{act.method.compare}},
  \code{\link{act.ci.plot}}
}

\examples{
  \dontrun{
act.sim <- act.method.compare(...)
print(act.compare.plot(act.sim))}
}
