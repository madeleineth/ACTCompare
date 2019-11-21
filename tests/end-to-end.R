# From ACTCompare, (c) 2010 Madeleine Thompson
# $Id: end-to-end.R 1832 2010-10-19 14:51:04Z mthompson $

# This is an end-to-end test of the ACTCompare package.  It simulates
# all of the chains (for a sample size of only 100), calls
# act.method.compare with all the methods, and calls both plotting
# functions.  Look at ACTCompare.Rcheck/tests/Rplots.pdf to see the
# results.

library(ACTCompare)

series <- generate.act.series(100)
act.sim <- act.method.compare(all.methods, series, c(10,100))
print(act.compare.plot(act.sim))
print(act.ci.plot(subset(act.sim, method=='AR process')))
