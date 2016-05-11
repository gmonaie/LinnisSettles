#' uploadAllSettles
#' @param dryRun
#' @export
uploadAllSettles <- function(dryRun = FALSE) {
	linnisOpenDryRun(dryRun = dryRun)

	upsertAllQuandl()
	upsertECBrates()
	# upsertAllFX()

	linnisCloseDryRun(dryRun = dryRun)
}