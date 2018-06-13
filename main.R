# install.packages("coinmarketcapr")

# install.packages("devtools")
devtools::install_github("amrrs/coinmarketcapr")

library(coinmarketcapr)

#get the global market cap details and assign it to a dataframe
latest_marketcap <- get_global_marketcap('EUR')
latest_marketcap

all_coins <- get_marketcap_ticker_all()
head(all_coins)
