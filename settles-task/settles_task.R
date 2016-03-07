suppressMessages(library(LinnisSettles))

dwidth <- getOption("width")
options(width = 1000)

uploadAllSettles(dryRun = dryRun)

options(width = dwidth)