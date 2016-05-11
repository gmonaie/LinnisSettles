#' @importFrom dplyr mutate %>% select arrange summarize group_by
#' @importFrom jsonlite fromJSON
#' @export
retECBrates <- function(base_currency = 'USD', date) {
	#	http://api.fixer.io/latest?base=USD

	if (missing(date)) {
		date = "latest"
	} else {
		date = format.Date(date, "%Y-%m-%d")
	}

	fixer.io.endpoint <- "http://api.fixer.io/"

	url <- paste0(fixer.io.endpoint, date, "?base=", base_currency)

	res <- fromJSON(url)

	# convert the results into a table of date, fx name, and the rate
	rates <- data.frame(settlepx=unlist(res$rates))
	rates$countercurrency <- row.names(rates)
	rates$currency 		<- res$base

	# now also create the inversions of these, flip the base and countercurrencies
	inverted_rates 									<- rates
	inverted_rates$settlepx 				<- 1/rates$settlepx
	inverted_rates$currency 				<- rates$countercurrency
	inverted_rates$countercurrency 	<- rates$currency

	allrates <- rbind(rates, inverted_rates)

	# now merge against our exchangerates table to find the ones we are actually going to insert for
	exchangerates <- retExchangeRates()

	upsertrates <- inner_join(exchangerates, allrates)
	upsertrates$settledate <- res$date
	upsertrates$settletype <- 'ECB'
	upsertrates$instrumentid <- upsertrates$exchangerateid
	# instrumentid, settledate, settletype, settlepx

	fr <- upsertrates[, c("instrumentid", "settledate", "settletype", "settlepx")]

	return(fr)
}

#' @export
upsertECBrates <- function(base_currency = 'USD', date) {
	fr <- retECBrates()

	linnisUpsert(schema = "marketdata", name = "settles", value = fr,
						 keys = c("instrumentid", "settledate", "settletype"))
}

#' @export
retExchangeRates <- function(exchangerateid = NULL, ...) {
	query <- retExchangeRatesQuery(exchangerateid=exchangerateid, ...)

	fr <- linnisExec(query)

	return(fr)
}

#' @export
retExchangeRatesQuery <- function(exchangerateid = NULL, cols = c("exchangerateid", "currency", "countercurrency", "ticksize")) {
  query <- mstr("
    SELECT XCOLSX
    FROM marketdata.exchangerates e
    XWHEREX
    XCLAUSESX
  ")

  columns <- c("e.exchangerateid")
  ops     <- c("in")
  strings <- list(exchangerateid)

  clauselist  <- unlist(Map(f = opClause, column = columns, op = ops, string = strings))
  clauses     <- paste(unlist(clauselist), collapse = " AND ")

  if (!is.null(exchangerateid)) {
    query <- str_replace_all(query, 'XWHEREX', 'WHERE')
    query <- str_replace_all(query, 'XCLAUSESX', clauses)
  } else {
    query <- str_replace_all(query, 'XWHEREX', '')
    query <- str_replace_all(query, 'XCLAUSESX', "")
  }

  query <- str_replace_all(query, 'XCOLSX', paste((paste0("e.", cols)), collapse = ", "))

  return(query)
}