
rm(list=ls(all=TRUE)) # Remove everything from environment

# To automatically install require packages
if (!require(DBI)) install.packages("DBI")
if (!require(RSQLite)) install.packages("RSQLite")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(grid)) install.packages("grid")
if (!require(corrplot)) install.packages("corrplot")
if (!require(zoo)) install.packages("zoo")
if (!require(magrittr)) install.packages("magrittr")

library(DBI)
library(ggplot2)
library(grid)
library(corrplot)
library(zoo)
library(magrittr)
library(TTR)
library(xts)
library(gtrendsR)
library(reshape2)
require(lubridate)
devtools::install_github("PMassicotte/gtrendsR")

# Check if you have universal installer package, install if not
if("pacman" %in% rownames(installed.packages()) == FALSE){
  install.packages("pacman")
} 

source("tools.R")
#Check, and if needed install the necessary packages
pacman::p_load("TTR","xts","gtrendsR","caret","ROCR","lift","glmnet","MASS", "partykit", "tidyverse", "scales", "xts", "grid", "gridExtra", "smooth", "Mcomp", "psych", "plyr","ggplot2", "forecast","knitr","kableExtra","rpart","e1071") 


### 1. Import and clean daily closing prices for each currency

# Import from sqlite
con <- dbConnect(RSQLite::SQLite(), dbname='database.db') # Database connection
currencies <- dbGetQuery(con, "SELECT * FROM currency") # Import currencies
vals <- dbGetQuery(con, "SELECT * FROM val") # Import values
rm(con) # Close database connection

# Clean and prepare data
vals$id <- NULL # Drop database IDs
currencies$id <- NULL # Drop database IDs
vals$datetime <- as.Date(vals$datetime) # Format dates
vals <- vals[!duplicated(vals[,6:7]),] # Remove duplicates/one price per day
interpolate.missing.data <- function(data) {
  currencies <- unique(data$currency_slug)
  newrows <- do.call("rbind", lapply(currencies, FUN=missing.date.rows, data))
  data <- rbind(data, newrows)
  data <- data[order(data$currency_slug,data$datetime),]; rownames(data) <- 1:nrow(data) # Sort
  for (currency in currencies) {
    idx <- colSums(!is.na(data[data$currency_slug==currency,1:5])) > 1
    data[data$currency_slug==currency,c(idx,FALSE,FALSE)] <- na.approx(data[data$currency_slug==currency,c(idx,FALSE,FALSE)], na.rm=FALSE)
  }
  return(data)
}
missing.date.rows <- function(currency, data) {
  dates <- unique(data[data$currency_slug==currency,6])
  alldates <- seq(dates[1],dates[length(dates)],by="+1 day")
  missingdates <- setdiff(alldates, dates)
  return(data.frame(price_usd=rep(NA, length(missingdates)),
                    price_btc=rep(NA, length(missingdates)),
                    volume_usd=rep(NA, length(missingdates)),
                    market_cap_usd=rep(NA, length(missingdates)),
                    available_supply=rep(NA, length(missingdates)),
                    datetime=as.Date(missingdates, origin="1970-01-01"),
                    currency_slug=rep(currency, length(missingdates))))
}
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

# Plot market cap, market return, market volatility and herfindahl index
plot.market <- function(market) {
  p1 <- ggplot(market, aes(datetime, cap)) +
    geom_line() +
    labs(x="Date", y="Market cap", title="Overall market") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p2 <- ggplot(market, aes(datetime, logreturn)) +
    geom_line() +
    labs(x="Date", y="Log return") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p3 <- ggplot(market, aes(datetime, volatility.30d)) +
    geom_line() +
    labs(x="Date", y="Annualized volatility") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p4 <- ggplot(market, aes(datetime, herfindahl)) + geom_line() + labs(x="Date", y="Herfindahl index")
  ## convert plots to gtable objects
  library(gtable)
  library(grid) # low-level grid functions are required
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g4 <- ggplotGrob(p4)
  g <- rbind(g1, g2, g3, g4, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
  # ggsave("Market-statistics.png", g, width=8, height=6, dpi=100, units="in")
}
plot.market(market)


### 3. Calculate individual currency statistics

# Fetch latest market capitalisation per currency
f = function(x) {
  vals[vals$currency_slug==x & vals$datetime==max(vals[vals$currency_slug==x,]$datetime),]$market_cap_usd
}

# Sort the currencies by market value
currencies$mcap = NULL
currencies$mcap <- sapply(currencies$slug, FUN=f)
currencies <- currencies[order(currencies$mcap,currencies$slug, decreasing=TRUE),]; 
order(currencies$mcap,currencies$slug, decreasing=TRUE)
rownames(currencies) <- 1:nrow(currencies) # Sort


# Calculate returns for all values
vals$return <- Reduce(c,sapply(unique(vals$currency_slug), FUN=function(x) c(0,diff(vals[vals$currency_slug==x,]$price_usd)/(vals[vals$currency_slug==x,]$price_usd)[-length(vals[vals$currency_slug==x,]$price_usd)])))
vals$logreturn <- Reduce(c,sapply(unique(vals$currency_slug), FUN=function(x) c(0,log(vals[vals$currency_slug==x,]$price_usd[-1]/vals[vals$currency_slug==x,]$price_usd[-length(vals[vals$currency_slug==x,]$price_usd)]))))

###################################################
##################################################
### Google Trend data Steps
# Step1 Download data from 2016
beginDate = as.Date('2016-01-01')
limitDate = as.Date('2018-05-23')
numberOfPeriods = ceiling( (limitDate - beginDate) / 90 )
google.trends = data.frame()
cat(sprintf("numberofPeriods %s", numberOfPeriods))
# Initialize to -1 to avoid rescaling on first loop
for(l in 1:numberOfPeriods) {
  endDate = beginDate + 90
  if(endDate > limitDate) {
    endDate = limitDate
  }
  time = sprintf("%s %s", as.Date(beginDate), as.Date(endDate))
  # cat(sprintf("Test Fetching trend data for %s\n", time))
  rangeTrends = gtrends(c("btc"), gprop = c("web"), time = time)[[1]]
  
  firstRow = rangeTrends[1,]
  rescaleRatio = 1
  
  if(l==1) {
    rescaleRatio = 1
  } else {
    rescaleRatio = lastValue$hits / firstRow$hits  
  }
  
  cat(sprintf("new range %s, ratio=%s\n", time, rescaleRatio))
  # Rescale the latest range with the ratio
  rangeTrends$hits = rangeTrends$hits * rescaleRatio
  lastValue = rangeTrends[nrow(rangeTrends), ] # save the last row for next loop

  # cat(sprintf("backup last row (date=%s) for next loop=%s\n", lastValue$datetime, lastValue$hits))
  rangeTrends = rangeTrends[-nrow(rangeTrends), ] # remove last row before to merge
  google.trends = rbind(google.trends, rangeTrends)

  beginDate = endDate
}
google.trends
google.trends$datetime = as.Date(google.trends$date)

google.trends

# Step 2 Merge bitcoins data and google trends data
btc_values = subset(vals, currency_slug=="bitcoin")
# Limit our subset data to 2016
btc_values = subset(btc_values, datetime > "2016-01-01")

fullDataset = merge(google.trends, btc_values, by="datetime") %>%
  arrange(datetime)

write.csv(google.trends, file="gtrends.csv")

google.trends$hits
btc_values$datetime
fullDataset

# fullDataset$hits_trend = fullDataset$hits - lag(fullDataset$hits) 
# fullDataset$loghits = log(pmax(fullDataset$hits, 1))
# fullDataset$log_hits_trend =  fullDataset$loghits - lag(fullDataset$loghits)
# index = which(ll$datetime=="2018-04-26")
# r = ll[(index-21):index,]
# trend3weeks = coef(lm(datetime ~ hits, data=r))[2]
# ll


# Rescale google infos 'hits' to plot on same scale as the price
rescaling_lm <- lm(price_usd ~ hits, data = fullDataset)
fullDataset$rescaled_hits = predict(rescaling_lm)

# Zoom on latest data
temp = subset(fullDataset, datetime > "2018-03-01")
ggplot() + geom_line(data=temp, aes(x=datetime, y=hits))


######################################
######################################

fullDataset$sma20 = SMA(btc_values$price_usd,n=20)
fullDataset = computeVolatility(fullDataset, c(7,14,21))
fullDataset = computeVolume(fullDataset, list(7,14,21))
fullDataset = computeMomentum(fullDataset, list(7,14,21))
fullDataset = computeBuyResult(fullDataset, list(7,14,21), 0.02)
fullDataset = computeGoogleTrends(fullDataset, c(7,14,21))

ggplot() +  
  geom_line(data = fullDataset, aes(y=price_usd, x=datetime)) +
  geom_line(data=fullDataset, aes(y=sma20, x=datetime, color="SMA 20 Days")) 

ggplot(data = fullDataset, aes(y=return, x=datetime))  + geom_line()

ggplot() + 
  geom_line(data = fullDataset, aes(y=volume_usd, x=datetime)) 

ggplot() + geom_line(data=fullDataset, aes(x=datetime, y=hits)) + geom_line(data = fullDataset, aes(y=price_usd, x=datetime)) 

ggplot() +  
  geom_line(data = fullDataset, aes(y=price_usd, x=datetime)) +
  #geom_line(data=fullDataset, aes(y=sma20, x=datetime), color="red")  +
  geom_line(data=fullDataset, aes(x=datetime, y=rescaled_hits), color="blue") + 
  geom_line(data=fullDataset, aes(x=datetime, y=hits), color="green") 


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


