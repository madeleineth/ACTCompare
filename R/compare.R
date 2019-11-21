# From ACTCompare, (c) 2010 Madeleine Thompson

# This function compares a set of ACT methods on a set of series.
# See ?act.method.compare for more information.

act.method.compare <- function(methods, series, lengths, trace = TRUE) {

  # Methods must be named.

  stopifnot(!is.null(names(methods)))

  # Data to be returned.

  acts <- data.frame()

  # Loop over all combinations of methods, series, and subsequence lengths.

  for (method in names(methods)) {
    for (ser in 1:length(series)) {
      for (len in 1:length(lengths)) {

        # Select a subsequence of the series of the specifed length.

        y <- series[[ser]]$series[1:lengths[len]]

        # Create an empty record.

        new.row <- data.frame(method=method,
                              series=series[[ser]]$name,
                              length=lengths[len],
                              true.act=series[[ser]]$act,
                              act=NA, act.025=NA, act.975=NA)
        row.names(new.row) <- NULL

        # Call the method on the subsequence.

        act <- try(methods[[method]](y), silent=TRUE)

        # Fill in the new record with the returned data if the estimate
        # was successful.

        if (class(act) == "try-error") {          # error
          # Leave everything NA
          warning(act)
        } else if (is.list(act)) {              # ACT with CI
          new.row$act <- act$act
          new.row$act.025 <- act$act.025
          new.row$act.975 <- act$act.975
        } else {                                # just an ACT
          stopifnot(is.numeric(act) && length(act)==1)
          new.row$act <- act
        }

        # Add the new record to the data to be returned, and print
        # out a summary if requested.

        acts <- rbind(acts, new.row)
        if (trace)
          with(new.row, cat(sprintf("%s %s length=%.2g act=%.3g\n",
                                    method, series, length, act)))
      }
    }
  }
  return(acts)
}
