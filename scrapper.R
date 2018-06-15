library(gtrendsR)

devtools::install_github("PMassicotte/gtrendsR")

##################################################
### Google Trend data Steps
# Step1 Download data from 2016
# Backup the data to an external CSV to avoid reloading everytime.
# - Difficulty in rescaling daily data on every 3months range.
scrapGTrendsForKeywords = function(keywords, filename) {
  beginDate = as.Date('2016-01-01')
  limitDate = as.Date('2018-05-23')
  
  # Initialize our frame which will be built dynamically.
  df_full_trends = data.frame()
  
  for(key in keywords) {
    trendsForCoin = scrapGTrendsForKey(key, beginDate, limitDate)
    df_full_trends = rbind(df_full_trends, trendsForCoin)
  }
  
  # Rename date to datetime and convert to similar type so we can merge with our market data.
  df_full_trends$datetime = as.Date(df_full_trends$date)
  
  # backup to an external file to avoid re-downloading data every time.
  write.csv(df_full_trends, file=filename)
  
  return(df_full_trends)
}

scrapGTrendsForKey = function(key, beginDate, limitDate) {
  df_google_trends = data.frame()
  
  # Download daily data by 90days chunks.
  # 90 days seem to be the maximum range before google automatically switch to weekly trends.
  numberOfPeriods = ceiling( (limitDate - beginDate) / 90 )
  cat(sprintf("[%s %s] %s Fetching trend data (%d periods)\n", beginDate, limitDate, key, numberOfPeriods))
  
  # Initialize to -1 to avoid rescaling on first loop
  for(l in 1:numberOfPeriods) {
    endDate = beginDate + 90
    if(endDate > limitDate) {
      endDate = limitDate
    }
    time = sprintf("%s %s", as.Date(beginDate), as.Date(endDate))
    rangeTrends = gtrends(c(key), gprop = c("web"), time = time)[[1]]
    
    firstRowInCurrentRange = rangeTrends[1,]
    rescaleRatio = 1
    
    if(l==1) {
      rescaleRatio = 1
    } else {
      # Since google sends 'scaled' data on each query going from 0-100, the number of hits for 1 days could be
      # different depending on which range it is returned.
      # for example:
      # 2018-01-01 will return 10 hits when obtained through the range "2017-12-01 2018-02-01"
      # 2018-01-01 will return 15 hits when obtained through the range "2017-12-25 2018-03-14"
      # Hence we need to rescale all the values when changing from one range to another.
      rescaleRatio = lastValueFromPreviousRange$hits / firstRowInCurrentRange$hits  
    }
    
    cat(sprintf("\t- Downloaded range %s && transform ratio=%s\n", time, rescaleRatio))
    # Save a copy of the hits value for comparison
    rangeTrends$hits_backup = rangeTrends$hits
    # Rescale the latest range with the ratio
    rangeTrends$hits = rangeTrends$hits * rescaleRatio
    lastValueFromPreviousRange = rangeTrends[nrow(rangeTrends), ] # save the last row for next loop
    
    # cat(sprintf("backup last row (date=%s) hits=%s\n", lastValueFromPreviousRange$date, lastValueFromPreviousRange$hits))
    rangeTrends = rangeTrends[-nrow(rangeTrends), ] # remove last row before to merge
    df_google_trends = rbind(df_google_trends, rangeTrends)
    
    beginDate = endDate
  }
  return(df_google_trends)
}
