#' @importFrom quantmod getFX
#' @importFrom lubridate year month day hms
#' @importFrom dplyr %>% group_by summarize select do inner_join
#' @export
upsertAllFX <- function(startdate = "2000-01-01", enddate = Sys.Date()) {

	# we'll be getting our settlement FX data from oanda

	# we'll want all the years since 2000 to present
	# then we want to expand the years

	date <- seq(as.Date(startdate), as.Date(enddate), by = 1)

	# now get the first and last date in every year
	dates <- data.frame(date, stringsAsFactors = F)

	params <- dates %>% group_by(year = year(date)) %>% summarize(from=first(date), to=last(date)) %>%
					select(-year)

	params$dummy <- 1

	n_years <- nrow(params)

	fxrates <- retFXRates()
	fxrates <- fxrates %>% mutate(fxrate=paste(currency,countercurrency, sep="/"))
	fxrates$dummy <- 1

	df <- inner_join(fxrates, params)

	# get the exchange rate data
	df <- df %>% group_by(exchangerateid, fxrate, from, to) %>%
				do(ts=getFX(Currencies = .$fxrate, from = .$from, to = .$to, auto.assign = FALSE))

	upsertRow <- function(exchangerateid, ts) {
		fr <- data.frame(coredata(ts))
		colnames(fr) <- "settlepx"

		fr$instrumentid <- exchangerateid
		fr$settledate <- index(ts)
		fr$settletype <- "EXCHANGE"

		# we can't insert NA settlement prices
		fr <- na.omit(fr[, c("instrumentid", "settledate", "settletype", "settlepx")])

		linnisUpsert(schema = "marketdata", name = "settles", value = fr,
								 keys = c("instrumentid", "settledate", "settletype"))
	}

	# create the upsert tables for each of these one row at a time
	# make sure the fxrate is a valid fxrate from our database

	linnisConnect(write = TRUE)
	df <- df %>% rowwise() %>% do(res=upsertRow(exchangerateid = .$exchangerateid, ts = .$ts))
	linnisConnect()
}

#' @export
retFXRates <- function() {
	query <- retFXRatesQuery()
	fr <- linnisExec(query)
	return(fr)
}

#' @export
retFXRatesQuery <- function(cols = c("exchangerateid", "currency", "countercurrency", "ticksize")) {
	query <- mstr("
						SELECT XCOLSX
						FROM marketdata.exchangerates
								")

	query <- str_replace_all(query, 'XCOLSX', paste(cols, collapse = ', '))

	return(query)
}