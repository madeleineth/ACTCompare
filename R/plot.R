# $Id: plot.R 3150 2012-01-29 03:06:40Z mthompson $
# From ACTCompare, (c) 2010 Madeleine Thompson

# This file contains act.compare.plot and act.ci.plot, two functions
# for visualizing the results from act.method.compare.

# scale_x_log_act and scale_y_log_act generate nice-looking x and y
# log scales for act.compare.plot and act.ci.plot.

scale_x_log_act <- function(lengths) {
  x.breaks <- 10^(min(ceiling(log10(lengths))) : 
                  max(floor(log10(lengths))))
  x.breaks <- as.integer(x.breaks)
  scale_x_log10(name='series length', breaks=x.breaks,
                labels=prettyNum(x.breaks, big.mark=','))
}

scale_y_log_act <- function(acts) {
  y <- log10(ifelse(is.finite(acts), acts, NA))
  y.breaks <- 10^(min(ceiling(y), na.rm=TRUE) : max(floor(y), na.rm=TRUE))
  scale_y_log10(name='autocorrelation time', breaks=y.breaks,
                labels=prettyNum(y.breaks, big.mark=",", drop0trailing=TRUE))
}

# Generates a shape scale for method using the first character of
# the method name.  Should not be used if there is more than one
# method with the same first character.  methods should be a factor,
# ... is passed on to scale_shape_manual.  The current use of ... is
# to allow act.ci.plot to turn off the legend associated with this
# scale.

scale_shape_method <- function(methods, ...) {
  method.names <- as.character(levels(methods))    # names of methods
  shapes <- tolower(substr(method.names, 1, 1))    # first char of name
  names(shapes) <- method.names                    # label shapes with method
  if (length(shapes) != length(unique(shapes)))
    warning('Some methods have common first letter; not all shapes unique.')
  scale_shape_manual(values=shapes, ...)            # build a scale object
}

# Generates a plot for comparing point estimates of ACTs.  See
# ?act.compare.plot for more details.

act.compare.plot <- function(data) {
  qplot(length, act, data=data, facets=~series, colour=method,
        geom='line', xlab='series length', ylab='autocorrelation time') +
    geom_point(aes(x=length, y=act, shape=method), colour='black') +
    scale_x_log_act(data$length) +
    scale_y_log_act(data$act) +
    scale_shape_method(data$method) +
    theme_bw(base_size=10) +
    opts(panel.grid.minor=theme_blank()) +
    opts(axis.text.x=theme_text(angle=30, vjust=1)) +
    geom_abline(aes(intercept=log(true.act, base=10)),
                slope=0, col='grey50', lty='dashed')
}

# Generates a plot for evaluating ACT method CIs.  Use subset to
# pick one method from the data.  See ?act.ci.plot for more details.

act.ci.plot <- function(data) {
  stopifnot(length(unique(data$method))==1)
  ggplot(data) +
    theme_bw(base_size=10) +
    opts(panel.grid.minor=theme_blank()) +
    opts(axis.text.x=theme_text(angle=30, vjust=1)) +
    geom_abline(aes(intercept=log(true.act, base=10)), slope=0, col='grey50',
                lty='dashed') +
    geom_linerange(aes(x=length, ymin=act.025, ymax=act.975), col='grey50') +
    geom_point(aes(x=length, y=act, shape=method)) +
    scale_x_log_act(data$length) +
    scale_y_log_act(c(data$act.025, data$act.975)) +
    scale_shape_method(data$method, guide="none") +
    facet_wrap(~series)
}
