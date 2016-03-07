#' uploadAllSettles
#' @param dryRun
#' @export
uploadAllSettles <- function(dryRun = FALSE) {
	upsertAllQuandl()
	upsertAllFX()
}