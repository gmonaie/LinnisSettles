suppressMessages(library(LinnisSettles))
args <- commandArgs(trailingOnly = TRUE)
dryRun <- as.logical(last(args))

# grab the last args parameter and set that to dryRun

dwidth <- getOption("width")
options(width = 1000)

uploadAllSettles(dryRun = dryRun)

options(width = dwidth)