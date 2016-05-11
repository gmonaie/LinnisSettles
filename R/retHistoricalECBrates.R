#' @importFrom tidyr gather
#' @importFrom readr read_csv
#' @importFrom dplyr select rename summarize mutate_each %>% mutate inner_join
#' @export
retHistoricalECBrates <- function(filename = "~/Downloads/eurofxref-hist.csv") {
	eurofxref <- read_csv(filename)
	eurofxref <- eurofxref %>% mutate_each(funs(as.numeric(.)), -Date)

	# convert this data which is in wide format
	# date, USD, JPY, BGN, CYP etc
	# take the EURUSD rate and since everything is EURJPY to convert to JPYUSD divide EURJPY / EURUSD = JPYUSD
	# EURGBP / EURUSD = USDGBP
	# to long form by gathering the items into one column with tidyr
	# then set the 

	rates <- eurofxref %>% mutate_each(funs(ifelse(is.na(.), ., (USD / .))), -Date, -USD)
	rates <- rename(rates, EUR = USD)
	rates <- rates %>% gather(currency, settlepx, -Date) %>% mutate(countercurrency = "USD")

	inverted_rates <- rates %>% mutate(settlepx = ifelse(is.na(settlepx), settlepx, 1 / settlepx)) %>% 
											select(-countercurrency) %>% rename(countercurrency = currency)
	inverted_rates$currency <- "USD"

	allrates <- rbind(rates, inverted_rates)

	# now merge against our exchangerates table to find the ones we are actually going to insert for
	exchangerates <- retExchangeRates()

	upsertrates <- inner_join(exchangerates, allrates)
	upsertrates$settledate <- upsertrates$Date
	upsertrates$settletype <- 'ECB'
	upsertrates$instrumentid <- upsertrates$exchangerateid
	# instrumentid, settledate, settletype, settlepx

	fr <- upsertrates[, c("instrumentid", "settledate", "settletype", "settlepx")]

	return(fr)
}

#' @export
upsertHistoricalECBrates <- function(filename = "~/Downloads/eurofxref-hist.csv") {
	fr <- retHistoricalECBrates(filename=filename)

	linnisUpsert(schema = "marketdata", name = "settles", value = fr,
						 keys = c("instrumentid", "settledate", "settletype"))
}