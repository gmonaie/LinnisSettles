#' @export
retQuandlIDs <- function() {

	query <- retQuandlIDsQuery()
  query <- paste("SET SEARCH_PATH=marketdata;", query, "SELECT * FROM tempquandlids ORDER BY instrumentid;")

  fr <- linnisExec(query)

  return(fr)
}

#' @importFrom LinnisCommons mstr
#' @export
retQuandlIDsQuery <- function() {
	query <- mstr("
		DROP TABLE IF EXISTS tempquandlids;

		SELECT iql.*, fs.futureseriesid, 
		iql.qlid || left(fs.suffix, 1) || '20'::text || right(fs.suffix,2) AS quandlid
		INTO TEMP tempquandlids
		FROM instruments_ql iql
		JOIN futureseries fs
		ON iql.instrumentid = fs.rootid
		ORDER BY instrumentid, fs.rollover;
	")

	return(query)
}

#' @importFrom dplyr %>% filter do rowwise
#' @export
loadFromAllQuandl <- function() {
	df <- retQuandlIDs()

	df %>% rowwise() %>% do(dump = loadQuandl(symbol = .$futureseriesid, quandlid = .$quandlid))
}

#' importFrom Quandl Quandl
#' @export
loadQuandl <- function(symbol, quandlid) {
	fr <- NULL

	tryCatch(fr <- Quandl(quandlid), silent = TRUE, 
						error = function(e) print(paste("bad ID:", quandlid)))

	if (!is.null(fr)) {
		assign(symbol, fr, .GlobalEnv)
	}
	# now upsert the Quandl data into the database
}

#' importFrom Quandl Quandl
#' @export
retQuandl <- function(symbol, quandlid) {
	fr <- NULL

	tryCatch(fr <- Quandl(quandlid), silent = TRUE, 
						error = function(e) print(paste("bad ID:", quandlid)))

	return(fr)
}

#' @export
upsertQuandl <- function(symbol, quandlid) {
	fr <- retQuandl(symbol, quandlid)

	if (!is.null(fr)) { # only upsert if we have a dataframe to upsert
		fr$instrumentid <- symbol
		fr$settledate <- fr$Date
		fr$settletype <- "EXCHANGE"
		fr$settlepx <- fr$Settle

		# we can't insert NA settlement prices
		fr <- na.omit(fr[, c("instrumentid", "settledate", "settletype", "settlepx")])

		linnisUpsert(schema = "marketdata", name = "settles", value = fr, 
			keys = c("instrumentid", "settledate", "settletype"))
	}
}

#' @importFrom dplyr %>% filter do rowwise
#' @importFrom Quandl Quandl.api_key
#' @export
upsertAllQuandl <- function() {
	library(Quandl)
	library(dplyr)

	df <- retQuandlIDs()

	linnisConnect(write = TRUE)

	Quandl.api_key("axqxmeeLBQZXUU8Qf1sX")

	df %>% rowwise() %>% 
		do(dump = upsertQuandl(symbol = .$futureseriesid, quandlid = .$quandlid))

	linnisConnect()
}