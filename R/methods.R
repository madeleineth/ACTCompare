# From ACTCompare, (c) 2010 Madeleine Thompson

# This file primarily serves to define all.methods, a list of all
# ACTCompare-supplied methods to be passed to act.method.compare.

# Initial positive sequence estimator, see ?ics.act for hints about
# what's going on.  See ?initseq or Geyer 1992 for discussion.  Takes
# a numeric vector of observations and returns a scalar ACT estimate.

ips.act <- function(x) {
  is <- initseq(x)
  is$var.pos / is$gamma0
}

# Initial monotone sequence estimator, see ?ics.act for hints about
# what's going on.  See ?initseq or Geyer 1992 for discussion.  Takes
# a numeric vector of observations and returns a scalar ACT estimate.

ims.act <- function(x) {
  is <- initseq(x)
  is$var.dec / is$gamma0
}

# Compute the ACT of the vector x, which is assumed to be de-meaned.
# Uses the initial convex sequence to choose a lag.  See documentation
# for mcmc::initseq for details.

ics.act1 <- function(x) {
  is <- initseq(x)
  is$var.con / is$gamma0
}

# Given a matrix of Markov chain states, return the longest of their
# autocorrelation times as computed with the initial convex sequence.

ics.act <- function(X) {
  A <- apply(as.matrix(X), 2, ics.act1)
  return(max(A))
}

# Heidelberger spectrum ACT estimator, see coda::spectrum0 or Heidelberger
# 1981.  Changing the order does not appear to change performance.
# Takes a numeric vector of observations and an integer polynomial
# model order and returns a scalar ACT estimate.

spec.act <- function(x, order = 1) {
  spec <- spectrum0(x, order = order, max.length = max(200, sqrt(length(x))))$spec
  spec / var(x)
}

# Batch means.  See Neal 1993 or Geyer 1992 for discussion.  act.pdf
# has derivations of CI formulae.

batch.act <- function(x, nbatch = max(4, floor(length(x)^0.33))) {
  batch.len <- floor(length(x) / nbatch)
  stopifnot(batch.len >= 1)

  # Compute batch means.

  means <- sapply(1:nbatch,
    function(i) { mean(x[((i-1)*batch.len+1):(i*batch.len-1)]) } )

  # Estimate ACT with variance of means.

  act <- var(means) / var(x) * batch.len
  return(act)
}

# Collect all methods together.  Omit ips.act and ims.act because
# they perform indistinguishably from ics.act.  ar.act is defined in
# SamplerCompare.

all.methods <- list(
  `ICS` = ics.act,
  `AR process` = ar.act,
  `Spec. fit` = spec.act,
  `Batch means` = batch.act)
