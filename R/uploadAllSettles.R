#' uploadAllSettles
#' @param dryRun
#' @export
uploadAllSettles <- function(dryRun = FALSE) {
	linnisOpenDryRun(dryRun = dryRun)

	upsertAllQuandl()
	upsertAllFX()

	linnisCloseDryRun(dryRun = dryRun)
}