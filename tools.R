
# Creta a function to fix missing values ("NAs") and preserve the NA info as surrogate variables
fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"FIXED_NA"
  character_reac<-"FIXED_NA"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    if (class(data_frame[,i]) %in% c("numeric","integer")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
          as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
        data_frame[is.na(data_frame[,i]),i]<-integer_reac
      }
    } else
      if (class(data_frame[,i]) %in% c("factor")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,i]<-as.character(data_frame[,i])
          data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-factor_reac
          data_frame[,i]<-as.factor(data_frame[,i])
          
        }
      } else {
        if (class(data_frame[,i]) %in% c("character")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-character_reac
          }
        } else {
          if (class(data_frame[,i]) %in% c("Date")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_surrogate")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-date_reac
            }
          }
        }
      }
  }
  return(data_frame)
}

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
      cat(sprintf("i=%s day=%s  i-day=%s  -> %s\n", i, day, i-day, row_mean))
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
      cat(sprintf("i=%s day=%s  i-day=%s  -> %s len=%s\n", i, day, i-day+1, gtrend, nrow(period)))
      copy[i,][[sprintf("gtrend.%s",day)]] = gtrend
    }
  }
  return(copy)
}
