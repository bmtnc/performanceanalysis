###############################################################################
# Example script: build long-form adjusted-close tibble for selected tickers  #
###############################################################################

# make magrittr pipe available without loading the package -------------------
`%>%` <- magrittr::`%>%`

# 1) Define the tickers you want ---------------------------------------------
tickers <- c("AAPL", "MSFT", "GOOG")   # <-- edit as desired

# 2) Fetch the data using helper functions -----------------------------------
all_data <- fetch_adjusted_prices(tickers)

all_data <- all_data %>% 
  dplyr::arrange(ticker, date)