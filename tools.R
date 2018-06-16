computeVolume = function(data, days) {
  if(missing(days)) {
    days = list(7,14,21) # 1week, 2weeks, 3weeks
  }
  
  copy = data
  
  for(day in days) {
    copy[[sprintf("volume.%s",day)]] = rep("0",  nrow(copy)) # Initialize with 0  
  }
  
  for(i in 1:nrow(data)) {
    
    for(day in days) {
      row_mean = mean(data$volume_usd[max(i+1-day, 0):i])  
      # cat(sprintf("i=%s day=%s  i-day=%s  -> %s\n", i, day, i-day, row_mean))
      copy[i,][[sprintf("volume.%s",day)]] = row_mean
    }
  }
  return(copy)
}

computeVolatility = function(data, days) {
  if(missing(days)) {
    days = list(7,14,21) # 1week, 2weeks, 3weeks
  }
  
  copy = data
  
  for(day in days) {
    copy[[sprintf("volatility.%s",day)]] = rep("0",  nrow(copy)) # Initialize with 0  
  }
  
  for(i in 1:nrow(data)) {
    
    for(day in days) {
      row_sd = sd(data$return[max(i+1-day, 0):i])  
      # cat(sprintf("i=%s day=%s  i-day=%s  -> %s\n", i, day, i-day, row_mean))
      copy[i,][[sprintf("volatility.%s",day)]] = row_sd
    }
  }
  return(copy)
}

computeMomentum = function(data, days) {
  if(missing(days)) {
    days = list(7,14,21) # 1week, 2weeks, 3weeks
  }
  
  copy = data
  
  for(day in days) {
    copy[[sprintf("momentum.%s",day)]] = rep("0",  nrow(copy)) # Initialize with 0  
  }
  
  for(i in 1:nrow(copy)) {
    
    for(day in days) {
      up_days = length(which(copy$return[max(i+1-day, 0):i] >= 0))
      down_days = length(which(copy$return[max(i+1-day, 0):i] < 0))
      
      # cat(sprintf("i=%s day=%s  i-day=%s  -> %s && %s\n", i, day, i-day, up_days, down_days))
      copy[i,][[sprintf("momentum.%s",day)]] = up_days - down_days
    }
  }
  return(copy)
}

computeBuyResult = function(data, days, threshold) {
  if(missing(days)) {
    days = list(7,14,21) # 1week, 2weeks, 3weeks
  }
  
  copy = data
  
  for(day in days) {
    copy[[sprintf("buy.%s",day)]] = rep("0",  nrow(copy)) # Initialize with 0  
  }
  
  for(i in 1:nrow(data)) {
    
    for(day in days) {
      buy = copy[i+day-1,]$price_usd / copy[i,]$price_usd - 1 > threshold
      # cat(sprintf("i=%s i+day=%s  -> %s / %s = %s\n", i, i+day, copy[i+day,]$price_usd, copy[i,]$price_usd, buy))
      copy[i,][[sprintf("buy.%s",day)]] = buy
    }
  }
  return(copy)
}

plotGTrendsIssue = function(trends) {
  # First plot total trends than break it down by main currencies
  pAll = ggplot() + geom_line(data=trends, aes(x=datetime, y=hits_backup)) + ggtitle("Scrapped Google Trends (Unscaled)")
  pAll_unscaled = ggplot() + geom_line(data=trends, aes(x=datetime, y=hits)) + ggtitle("Google Trends after scaling")
  grid.arrange(grobs=list(pAll_unscaled, pAll), ncol=1)   
}

plotGTrends = function(trends) {
  # #######################
  # # BUG cannot create the grobs from a loop
  # N <- length(selection)
  # # x <- vector("list", N)
  # allPlots = list()
  # for(i in 1:N) {
  #   coin = selection[i]
  #   coinTrends = subset(trends, keyword==coin)
  #   cat(sprintf("[%d] Look for coin %s\n", i, coin))
  # 
  #   pCoin = ggplot() + geom_line(data=coinTrends, aes(x=datetime, y=hits)) + ggtitle(coin)
  # #   # allPlots = list(allPlots, pCoin)
  # #   x[[i]] = pCoin
  #   allPlots = rbind(allPlots, pCoin)
  # #   # allPlots = append(allPlots, pCoin)
  # }
  # cat(sprintf("List of coins done %s", length(allPlots)))
  # grid.arrange(grobs=allPlots, ncol=2)
  
  btcTrends = subset(trends, keyword=="BTC")
  ethTrends = subset(trends, keyword=="ETH")
  eosTrends = subset(trends, keyword=="EOS")
  xrpTrends = subset(trends, keyword=="XRP")
  ltcTrends = subset(trends, keyword=="LTC")
  pBTC = ggplot() + geom_line(data=btcTrends, aes(x=datetime, y=hits)) + ggtitle("BTC")
  pETH = ggplot() + geom_line(data=ethTrends, aes(x=datetime, y=hits)) + ggtitle("ETH")
  pEOS = ggplot() + geom_line(data=eosTrends, aes(x=datetime, y=hits)) + ggtitle("EOS")
  pXRP = ggplot() + geom_line(data=xrpTrends, aes(x=datetime, y=hits)) + ggtitle("XRP")
  pLTC = ggplot() + geom_line(data=ltcTrends, aes(x=datetime, y=hits)) + ggtitle("LTC")
  grid.arrange(grobs=list(pBTC, pETH, pEOS, pXRP, pLTC), ncol=2)
}

plotCoinData = function(coinDataset) {
  slug = coinDataset[1,]$currency_slug
  
  # ts_coin <- ts(coinDataset$price_usd, start=c(2016, 1, 1), frequency=365.25) 
  # ts_coin <- na.omit(ts_coin)
  # fit_drift <- rwf(ts_coin, drift=TRUE, h=7) # 7days horizon 
  # pred_drift <- forecast(fit_drift)
  # drift <- data.frame(datetime=time(pred_drift$mean), price_usd=fit_drift$mean)
  # 
  # plot_forecast = ggplot() +
  #   geom_line(data=coinDataset, aes(y=price_usd, x=datetime)) +
  #   geom_line(data=drift, aes(y=price_usd, x=datetime, color="RWF forecasts")) +
  #   ggtitle(sprintf("%s price forecast", slug))
  
  plot_price = ggplot() +  
    geom_line(data=coinDataset, aes(y=price_usd, x=datetime)) +
    geom_line(data=coinDataset, aes(y=sma20, x=datetime, color="SMA 20 Days")) +
    geom_line(data=coinDataset, aes(y=sma50, x=datetime, color="SMA 50 Days")) +
    geom_line(data=coinDataset, aes(y=sma200, x=datetime, color="SMA 200 Days")) +
    ggtitle(sprintf("%s History with Simple Moving Average",slug))
  
  plot_return = ggplot(data = coinDataset, aes(y=return, x=datetime))  + geom_line()
  
  # Rescale google infos 'hits' to plot on same scale as the price
  rescaling_lm <- lm(price_usd ~ hits, data = coinDataset)
  coinDataset$rescaled_hits = predict(rescaling_lm)
  
  plot_volume = ggplot() +
    geom_line(data = coinDataset, aes(y=volume_usd, x=datetime))
  
  plot_trends = ggplot() +
    geom_line(data=coinDataset, aes(x=datetime, y=price_usd)) +
    geom_line(data=coinDataset, aes(x=datetime, y=rescaled_hits), color="green") +
    ggtitle(sprintf("Comparing Google Trends to %s Price",slug))
  
  lay <- rbind(c(1,1),
               c(2,3),
               c(4))
  grid.arrange(grobs=list(plot_price, plot_return, plot_volume, plot_trends), layout_matrix=lay)
  
  plot.currency(vals, slug)
  
  p1 = plot.beta.timeline(c(slug), 30, 90, vals, market)
  p2 = plot.return.vs.market(slug, vals[vals$datetime>as.Date("2017-07-01"),], market)
  grid.arrange(grobs=list(p1,p2), ncol=2)
}

plotLogisticReg = function(lres) {
  # cat(sprintf("[PLOT results] %s\n", toString(lres$slug)))
  par(mfrow=c(1,4))
  # Last plot is used to find outliers
  # QQ Plot is the important one
  plot(lres$log_model) #Error plots: similar nature to lm plots
  par(mfrow=c(1,1))
  plot(lres$ROC) #Plot ROC curve
  #### Lift chart
  plotLift(lres$prob, lres$newdata[["buy.7"]], cumulative = TRUE, n.buckets = 10) # Plot Lift chart
}

doLogisticReg = function(fullDataset, stepwise) {
  if(missing(stepwise)) {
    stepwise<-FALSE  
  }
  
  # Ignore NAs from our data using a subset.
  data = subset(fullDataset, buy.7==TRUE | buy.7==FALSE)
  data$buy.7[which(data$buy.7==TRUE)] = "1"
  data$buy.7[which(data$buy.7==FALSE)] = "0"
  slug = fullDataset[1,]$currency_slug
  
  factor_vars <- c('volatility.7','volatility.14','volatility.21','volume.7','volume.14','volume.21','momentum.7','momentum.14','momentum.21','gtrend.7','gtrend.14','gtrend.21')
  
  # Make sure to remove NAs
  data = na.omit(data, cols=c(factor_vars))
  data[factor_vars] <- lapply(data[factor_vars], as.numeric)
  data$buy.7 = as.factor(data$buy.7)
  
  # 80% of data goes into Training
  inTrain <- createDataPartition(y = data$buy.7, p = 0.8, list = FALSE)
  training <- data[ inTrain,]
  testing <- data[ -inTrain,]
  
  model = buy.7 ~ volume.7 + volume.14 + volume.21 + volatility.7 + volatility.14 + volatility.21 + momentum.7 +  momentum.14 + momentum.21 + gtrend.7 + gtrend.14 + gtrend.21
  # Train our model first
  logistic_reg = glm(model, data=training, family="binomial"(link="logit"))
  
  if(stepwise) {
    #AIC stepwise 
    logistic_reg = stepAIC(model, direction = c("both"), trace = 0) 
  } 
  
  logistic_probabilities<-predict(logistic_reg, newdata=testing, type="response") #Predict probabilities/responses
  logistic_classification<-rep("1", nrow(testing)) # Default ALL Value to 1
  threshold = length(which(testing$buy.7 == 1)) / length(testing$buy.7) 
  
  logistic_classification[which(logistic_probabilities<threshold)]=0 #Predict classification using average default threshold.
  logistic_classification<-as.factor(logistic_classification)

  matrix = confusionMatrix(logistic_classification,testing$buy.7) #Display confusion matrix
  umatrix = unlist(matrix)
  matrix_accuracy = umatrix[["overall.Accuracy"]]
  matrix_sensitivity = umatrix[["byClass.Sensitivity"]]
  matrix_specificity = umatrix[["byClass.Specificity"]]

  logistic_ROC_prediction <- prediction(logistic_probabilities, testing$buy.7)
  logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
  
  ####AUC (area under curve)
  auc.tmp <- performance(logistic_ROC_prediction,"auc") #Create AUC data
  logistic_auc_testing <- as.numeric(auc.tmp@y.values) #Calculate AUC

  result = list("slug"=slug, "matrix"=matrix, "accuracy"=matrix_accuracy, "sensitivity"=matrix_sensitivity, "specificity"=matrix_specificity, "auc"=logistic_auc_testing, "log_model"=logistic_reg,"prob"=logistic_probabilities, "ROC"=logistic_ROC, "newdata"=testing)
  return(result)
}

compareResults = function(results) {
  slugs = c()
  accuracies = c()
  sensitivities = c()
  specifities = c()
  aucs = c()
  
  for(res in results) {
    slug = res$slug
    matrix_accuracy = res$accuracy
    matrix_sensitivity = res$sensitivity
    matrix_specificity = res$specificity
    auc = res$auc
    
    slugs = append(slugs, slug)
    accuracies = append(accuracies, matrix_accuracy)
    sensitivities = append(sensitivities, matrix_sensitivity)
    specifities = append(specifities, matrix_specificity)
    aucs = append(aucs, auc)
    
    # cat(sprintf("Name=%s && auc=%s && accuracy=%s && specificity=%s && sensitivities=%s\n", slug, auc, matrix_accuracy, matrix_specificity, matrix_sensitivity))
  }
  
  df = data.frame("Name"=slugs,"Accuracy"=accuracies, "Sensitivities"=sensitivities, "Specificities"=specifities,"AUC"=aucs)
  return(df)
}

computeGoogleTrends = function(data, days) {
  if(missing(days)) {
    days = list(7,14,21) # 1week, 2weeks, 3weeks
  }
  
  copy = data
  
  for(day in days) {
    copy[[sprintf("gtrend.%s",day)]] = rep("0",  nrow(copy)) # Initialize with 0  
  }
  
  for(i in 1:nrow(copy)) {
    for(day in days) {
      period = copy[max(i-day+1,1):i,]
      gtrend = coef(lm(datetime ~ hits, data=period))[2]
      # cat(sprintf("i=%s day=%s  i-day=%s  -> %s len=%s\n", i, day, i-day+1, gtrend, nrow(period)))
      copy[i,][[sprintf("gtrend.%s",day)]] = gtrend
    }
  }
  return(copy)
}

coinDataEngineering = function(coinName) {
  # Extract subset of data related to the selected coin
  
  # Limit our search from a specific date
  fromDate = "2016-01-01"
  
  # Find currency_slug from coin name
  slug = subset(currencies, name==coinName)$slug
  
  # Step 2 Merge bitcoins data and google trends data
  values = subset(vals, currency_slug==slug)
  # Limit our subset data to 2016
  values = subset(values, datetime > fromDate)
  
  # Use similar class before to merge
  trends = subset(google.trends, keyword==coinName)
  
  fullDataset = merge(trends, values, by="datetime") %>%
    arrange(datetime)
  
  cat(sprintf("[%s] Feature engineering on full dataset for\n",coinName))
  
  fullDataset$sma20 = SMA(fullDataset$price_usd,n=20)
  fullDataset$sma50 = SMA(fullDataset$price_usd,n=50)
  fullDataset$sma200 = SMA(fullDataset$price_usd,n=200)
  cat(sprintf("\t[%s] SMA20 [Done]\n",coinName))
  
  fullDataset = computeVolatility(fullDataset, c(7,14,21))
  cat(sprintf("\t[%s] Volatility [Done]\n",coinName))
  
  fullDataset = computeVolume(fullDataset, list(7,14,21))
  cat(sprintf("\t[%s] Volume [Done]\n",coinName))
  
  fullDataset = computeMomentum(fullDataset, list(7,14,21))
  cat(sprintf("\t[%s] Momemtum [Done]\n",coinName))
  
  fullDataset = computeBuyResult(fullDataset, list(7,14,21), 0.02)
  cat(sprintf("\t[%s] BuyResult [Done]\n",coinName))
  
  fullDataset = computeGoogleTrends(fullDataset, c(7,14,21))
  cat(sprintf("\t[%s] GoogleTrends [Done]\n",coinName))
  
  return(fullDataset)
}

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
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g4 <- ggplotGrob(p4)
  # Skip Herfindahl index for now
  g <- rbind(g1, g2, g3, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths, g4$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
}

# Plot currency cap, return and volatility
plot.currency <- function(data, slug) {
  data <- data[data$currency_slug==slug,]
  data$volatility.30d <- sapply(1:nrow(data), FUN=function(i) sd(data$logreturn[(max(i-30,0):i)]))*sqrt(365)
  p1 <- ggplot(data, aes(datetime, market_cap_usd)) +
    geom_line() +
    labs(x="Date", y="Market cap", title=slug) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p2 <- ggplot(data, aes(datetime, logreturn)) +
    geom_line() + labs(x="Date", y="Log return") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank())
  p3 <- ggplot(data, aes(datetime, volatility.30d)) + geom_line() + labs(x="Date", y="Annualized volatility")
  ## convert plots to gtable objects
  library(gtable)
  library(grid) # low-level grid functions are required
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g <- rbind(g1, g2, g3, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
}

# Plot currency cap, return and volatility for multiple currencies
plot.currencies <- function(data, slugs) {
  data <- data[data$currency_slug %in% slugs,]
  data$volatility.30d <- Reduce(c,sapply(unique(data$currency_slug), FUN=function(x) sapply(1:length(data[data$currency_slug==x,]$logreturn), FUN=function(i) sd(data[data$currency_slug==x,]$logreturn[(max(i-30,0):i)]))))*sqrt(365)
  p1 <- ggplot(data, aes(datetime, market_cap_usd, color=factor(currency_slug))) +
    geom_line() +
    labs(x="Date", y="Market cap", title=paste(slugs, collapse=", ")) +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title=element_blank())
  p2 <- ggplot(data, aes(datetime, logreturn, color=factor(currency_slug))) +
    geom_line() +
    labs(x="Date", y="Log return") +
    theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), legend.title=element_blank())
  p3 <- ggplot(data, aes(datetime, volatility.30d, color=factor(currency_slug))) +
    geom_line() +
    labs(x="Date", y="Annualized volatility")
  ## convert plots to gtable objects
  library(gtable)
  library(grid) # low-level grid functions are required
  g1 <- ggplotGrob(p1)
  g2 <- ggplotGrob(p2)
  g3 <- ggplotGrob(p3)
  g <- rbind(g1, g2, g3, size="first") # stack the plots
  g$widths <- unit.pmax(g1$widths, g2$widths, g3$widths) # use the largest widths
  # center the legend vertically
  g$layout[grepl("guide", g$layout$name),c("t","b")] <- c(1,nrow(g))
  grid.newpage()
  grid.draw(g)
}

# Generates a dataframe with complete daily information for a set of currencies
analysis.data <- function(currencies, data, market=NULL) {
  temp <- lapply(currencies, FUN=function(x) subset(data, currency_slug==x))
  temp <- Reduce(function(df1, df2) merge(df1, df2, by="datetime"), temp)
  if (length(currencies) > 1)
    colnames(temp) <- c("datetime", sapply(currencies, function(slug) sapply(colnames(data)[c(1:5,7:9)], function(x) paste(x, slug, sep="_"))))
  if (!is.null(market))
    temp <- merge(temp, market, by="datetime")
  data.frame(temp)
}

# Generates a dataframe with daily returns for a set of currencies
analysis.return.data <- function(currencies, data) {
  data <- reshape(data[data$currency_slug %in% currencies,c(6,7,9)], direction="wide", idvar="datetime", timevar="currency_slug")
  colnames(data) <- c("datetime", sort(currencies))
  data <- data[,c("datetime", currencies)]
  return(data)
}

# Plot return against weighted market return
plot.return.vs.market <- function(currency, data, market) {
  data <- analysis.data(currency, data, market)
  cor_ <- cor(data$logreturn.x, data$logreturn.y)
  p <- ggplot(data, aes(x=logreturn.x, y=logreturn.y)) + geom_point() +
    labs(title=paste("Returns: ",currency," vs Market (cor = ",round(cor_, digits=4),")",sep=""), x=paste(currency, "return"), y="Market return") +
    theme(legend.title=element_blank())
  return(p)
}

# Plot betas of top currencies against latest market cap
plot.beta.vs.mcap.num <- function(num, currencies) {
  data <- currencies[order(currencies$mcap, decreasing=TRUE),] # Sort
  data <- data[0:num,]
  p <- ggplot(data, aes(x=mcap, y=beta)) + geom_point() + scale_x_log10() +  geom_text(aes(label=name),hjust=0, vjust=0) +
    labs(title="Beta vs Market capitalisation", x="Market capitalisation [USD] (log scale)", y="Beta") 
  # # ggsave("Beta-vs-mcap.png", width=8, height=5, dpi=100, units="in")
  return(p)
}

# Plot betas over time
plot.beta.timeline <- function(currencies, mindays, maxdays, data, market) {
  data <- data[data$currency_slug %in% currencies,]
  dates <- intersect(data$datetime, market$datetime)
  result <- data.frame(datetime=as.Date(rep(dates, times=length(currencies)), origin="1970-01-01"), currency=rep(currencies,each=length(dates)))
  result$beta <- Reduce(c, sapply(currencies,
                                  function(currency) sapply(dates,
                                                            function(date) if(nrow(data[data$currency_slug==currency & date-maxdays<data$datetime & data$datetime<=date,])<mindays) return(NA) else currency.beta(currency, data[data$currency_slug==currency & date-maxdays<data$datetime & data$datetime<=date,], market))))
  p <- ggplot(result, aes(datetime, beta, color=factor(currency))) +
    geom_line() + labs(x="Date", y="Beta", title=paste("Beta timeline: ", paste(currencies, collapse=", "))) +
    theme(legend.position="bottom")
  
  return(p)
}

# Calculate betas
currency.beta <- function(currency, data, market) {
  dates <- intersect(data[data$currency_slug==currency,]$datetime, market$datetime)
  return(cov(data[data$currency_slug==currency & data$datetime %in% dates,]$logreturn,
             market[market$datetime %in% dates,]$logreturn)/var(market[market$datetime %in% dates,]$logreturn))
}

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