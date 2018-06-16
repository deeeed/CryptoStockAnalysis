
rm(list=ls(all=TRUE)) # Remove everything from environment

# To automatically install require packages
if (!require(DBI)) install.packages("DBI")
if (!require(RSQLite)) install.packages("RSQLite")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(grid)) install.packages("grid")
if (!require(corrplot)) install.packages("corrplot")
if (!require(zoo)) install.packages("zoo")
if (!require(magrittr)) install.packages("magrittr")

# Check if you have universal installer package, install if not
if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")
} 

devtools::install_github("PMassicotte/gtrendsR")
#Check, and if needed install the necessary packages
pacman::p_load("TTR","xts","gtrendsR","caret","ROCR","lift","glmnet","MASS", "partykit", "tidyverse", "scales", "xts", "grid", "gridExtra", "smooth", "Mcomp", "psych", "plyr","ggplot2", "forecast","knitr","kableExtra","rpart","e1071","lubridate", "magrittr", "DBI","corrplot", "zoo","gtable") 

# Make sure to use identitcal seed for reproductible results
set.seed(1234)

source("tools.R")
source("scrapper.R")

### 1. Import and clean daily closing prices for each currency

# Import from sqlite
con <- dbConnect(RSQLite::SQLite(), dbname='database.db') # Database connection
currencies <- dbGetQuery(con, "SELECT * FROM currency") # Import currencies
vals <- dbGetQuery(con, "SELECT * FROM val") # Import values
rm(con) # Close database connection

# Clean and prepare data
vals$id = NULL # Drop database IDs
currencies$id = NULL # Drop database IDs
vals$datetime <- as.Date(vals$datetime) # Format dates
vals <- vals[!duplicated(vals[,6:7]),] # Remove duplicates/one price per day
vals <- interpolate.missing.data(vals) # For missing dates, insert fields and interpolate values (takes some time)

### 2. Calculate overall market statistics

## Calculate market statistics
# returns: return(t) = (price(t) - price(t-1)) / price(t-1)
# logreturns: logreturn(t) = ln(price(t)/price(t-1))
# annualized volatility: sd(logreturns per x days)*sqrt(trading days=365)
# herfindahl: sum of squares of competitor market shares
market.data <- function(data) {
  dates <- sort(unique(data$datetime))
  cap <- sapply(dates, FUN=function(date) sum(data[data$datetime==date,4]))
  returns <- c(0,diff(cap)/cap[-length(cap)])
  logreturns <- c(0,log(cap[-1]/cap[-length(cap)]))
  volatility.7d <- sapply(1:length(logreturns), FUN=function(i) sd(logreturns[(max(i-7,0):i)]))*sqrt(365)
  volatility.30d <- sapply(1:length(logreturns), FUN=function(i) sd(logreturns[(max(i-30,0):i)]))*sqrt(365)
  volatility.90d <- sapply(1:length(logreturns), FUN=function(i) sd(logreturns[(max(i-90,0):i)]))*sqrt(365)
  herfindahl <- sapply(dates, FUN=function(date) sum((data[vals$datetime==date,4]/sum(data[data$datetime==date,4]))^2))
  data.frame(datetime=dates, cap=cap, return=returns, logreturn=logreturns, volatility.7d=volatility.7d, volatility.30d=volatility.30d, volatility.90d=volatility.90d, herfindahl=herfindahl)
}
market <- market.data(vals)

plot.market(market)

### 3. Calculate individual currency statistics

# Fetch latest market capitalisation per currency
latestMarketCapPerCurrency = function(x) {
  vals[vals$currency_slug==x & vals$datetime==max(vals[vals$currency_slug==x,]$datetime),]$market_cap_usd
}

# Sort the currencies by market value
currencies$mcap = NULL
currencies$mcap <- sapply(currencies$slug, FUN=latestMarketCapPerCurrency)
currencies <- currencies[order(currencies$mcap,currencies$slug, decreasing=TRUE),]; 
order(currencies$mcap,currencies$slug, decreasing=TRUE)
rownames(currencies) <- 1:nrow(currencies) # Sort

currencies$beta <- sapply(currencies$slug, FUN=currency.beta, vals[vals$datetime>as.Date("2016-12-31"),], market)
# display our sorted currencies
currencies

# Calculate returns for all values
vals$return <- Reduce(c,sapply(unique(vals$currency_slug), FUN=function(x) c(0,diff(vals[vals$currency_slug==x,]$price_usd)/(vals[vals$currency_slug==x,]$price_usd)[-length(vals[vals$currency_slug==x,]$price_usd)])))
vals$logreturn <- Reduce(c,sapply(unique(vals$currency_slug), FUN=function(x) c(0,log(vals[vals$currency_slug==x,]$price_usd[-1]/vals[vals$currency_slug==x,]$price_usd[-length(vals[vals$currency_slug==x,]$price_usd)]))))


######################################
## Coin Analysis
######################################
slugs = c("bitcoin","ethereum", "ripple", "litecoin", "eos")
plot.currencies(vals, slugs)
plot.beta.vs.mcap.num(20, currencies)
plot.beta.timeline(slugs, 30, 90, vals, market)

# Scrap google trends or load directly from a previously downloaded data
# scrapGTrendsForKeywords(c("BTC","ETH","XRP","EOS","LTC"), "gtrends.csv")
google.trends = read.csv("gtrends.csv")
google.trends$datetime = as.Date(google.trends$datetime)

plotGTrends(google.trends)

btcValues = coinDataEngineering("BTC")
ethValues = coinDataEngineering("ETH")
xrpValues = coinDataEngineering("XRP")
ltcValues = coinDataEngineering("LTC")
eosValues = coinDataEngineering("EOS")

btcResults = doLogisticReg(btcValues)
ethResults = doLogisticReg(ethValues)
xrpResults = doLogisticReg(xrpValues)
ltcResults = doLogisticReg(ltcValues)
eosResults = doLogisticReg(eosValues)

plotCoinData(btcValues)
plotLogisticReg(btcResults)

plotCoinData(ethValues)
plotLogisticReg(ethResults)

plotCoinData(xrpValues)
plotLogisticReg(xrpResults)

plotCoinData(eosValues)
plotLogisticReg(eosResults)

plotCoinData(ltcValues)
plotLogisticReg(ltcResults)

results = list(btcResults, ethResults, xrpResults, ltcResults, eosResults)
compareResults(results)






{
  ######################################
  ## Logistic Regression 
  ######################################
  str(btc_values)
  nrow(fullDataset)
  # Ignore NAs from our data using a subset.
  data = subset(fullDataset, buy.7==TRUE | buy.7==FALSE)
  data$buy.7[which(data$buy.7==TRUE)] = "1"
  data$buy.7[which(data$buy.7==FALSE)] = "0"
  
    factor_vars <- c('volatility.7','volatility.14','volatility.21','volume.7','volume.14','volume.21','momentum.7','momentum.14','momentum.21','gtrend.7','gtrend.14','gtrend.21')
  
  nrow(data)  
  # Cleanup our data first
  data = na.omit(data, cols=c(factor_vars))
  data
  data[factor_vars] <- lapply(data[factor_vars], as.numeric)
  str(data)
  data$buy.7 = as.factor(data$buy.7)
  data
  # 80% of data goes into Training
  inTrain <- createDataPartition(y = data$buy.7, p = 0.8, list = FALSE)
  training <- data[ inTrain,]
  testing <- data[ -inTrain,]
  
  summary(data$volume.7)
  str(data)
  
  model = buy.7 ~ volume.7 + volume.14 + volume.21 + volatility.7 + volatility.14 + volatility.21 + momentum.7 +  momentum.14 + momentum.21 + gtrend.7 + gtrend.14 + gtrend.21
  logistic_reg = glm(model, data=training, family="binomial"(link="logit"))
  
  logistic_probabilities<-predict(logistic_reg, newdata=testing, type="response") #Predict probabilities/responses
  logistic_classification<-rep("1", nrow(testing)) # Default ALL Value to 1
  length(logistic_classification)
  nrow(logistic_probabilities)
  nrow(testing)
  threshold = length(which(testing$buy.7 == 1)) / length(testing$buy.7) 
  
  nrow(testing)
  nrow(training)
  logistic_probabilities
  length(logistic_probabilities)
  
  logistic_classification[which(logistic_probabilities<threshold)]=0 #Predict classification using average default threshold.
  as.numeric(logistic_classification)
  testing$buy.7
  logistic_classification<-as.factor(logistic_classification)
  
  logistic_classification
  length(testing)
  matrix = confusionMatrix(logistic_classification,testing$buy.7) #Display confusion matrix
  matrix
  
  logistic_ROC_prediction <- prediction(logistic_probabilities, testing$buy.7)
  logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
  
  ####AUC (area under curve)
  auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
  logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC
  logistic_auc_testing
  
  par(mfrow=c(1,4))
  # Last plot is used to find outliers
  # QQ Plot is the important one
  plot(logistic_reg) #Error plots: similar nature to lm plots
  par(mfrow=c(1,1))
  plot(logistic_ROC) #Plot ROC curve
  #### Lift chart
  plotLift(logistic_probabilities, testing$buy.7, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
}
{
# ctree_tree<-ctree(model, data=training) #Run ctree on training data
# ctree_probabilities<-predict(ctree_tree, newdata=testing,type="prob") #Predict probabilities
# 
# ctree_probabilities
# nrow(ctree_probabilities)
# ctree_classification<-rep("1",nrow(ctree_probabilities))
# 
# # threshold is the average probability of being retained in the data.
# ctree_classification[ctree_probabilities[,2]<threshold]=0
# ctree_classification<-as.factor(ctree_classification)
# 
# matrix = confusionMatrix(ctree_classification,testing$buy.7)
# matrix
# 
# # ROC curve
# ctree_pred <- prediction(ctree_probabilities[,2], testing$buy.7) #Calculate errors
# ctree_ROC <- performance(ctree_pred,"tpr","fpr") #Create ROC curve data
# ctree_ROC
# 
# # AUC (area under curve)
# auc.tmp <- performance(ctree_pred,"auc") #Create AUC data
# ctree_auc <- as.numeric(auc.tmp@y.values) #Calculate AUC
# ctree_auc
}


