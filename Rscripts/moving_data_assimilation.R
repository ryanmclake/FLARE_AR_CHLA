# script to subset training dataset to different window lengths

# inputs to function:
#   folder <- forecasting working director
#   data_file <- output from data_assimilation_AR.R (training dataset updated to forecast_start_day timeframe)
#   forecast_start_day <- day on which the forecast in being run
#   window_length <- how many days of data assimilation do you want; if no window subsetting is wanted, window_length = 'all'

moving_data_assimilation <- function(folder, data_location, data_file, forecast_start_day, window_length){
  
  setwd(folder)
  
  # read in the original training dataset from 2013-2016
  # this is the format that the end file should have so that it can read in to the jags code
  
  
  if(window_length=='all'){ print('using all training data')}else{
    data <- read.csv(data_file)
    data$Date <- as.Date(data$Date)
    limit <- as.Date(forecast_start_day) - as.numeric(window_length)
    data <- data[data$Date >= limit, ]
    write.csv(data, 'data_arima_working.csv', row.names = FALSE)
    print(paste0('training data limited to ', window_length, ' days before forecast_start_day'))
  }
  
  
  
}