# determine summary stats (median, standard deviation, max values) for observed chl-a during training period and during forecast period

library(tidyverse)


folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
timestep <- 'daily'
setwd(folder)

# training dataset May-Oct 2013-2016
train <- read.csv('./data_arima_WW.csv')
train <- train %>% mutate(Chla = Chla_sqrt^2) %>% 
  mutate(Chla_EXO = ((Chla + 0.0308)/0.55))
median(train$Chla_EXO)
sd(train$Chla_EXO)
plot(train$Date, train$Chla_EXO)

# and over the forecasting period, Jan 2019-Dec 2019
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/relhum_training_spinup_Feb0420')
setwd(forecast_folder)

# code to read in the individual forecast files named for the day on which the forecast is made
myfiles_forecast <- list.files(path = forecast_folder, pattern = paste0('*', timestep, ".csv"))
dataset_forecast <- read.csv(paste0(forecast_folder, "/", myfiles_forecast[1]))

# read in files
for (i in 2:length(myfiles_forecast)) {
  temp_2 <- read.csv(paste0(forecast_folder,"/", myfiles_forecast[i]))
  dataset_forecast <- rbind(dataset_forecast, temp_2)
}

# some data arranging
stuff <- dataset_forecast 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$day_in_future)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")
#remove spin up dates, so anything before Dec 31, 2018
stuff <- stuff[stuff$forecast_run_day>'2018-12-31',]

median(stuff$obs_chl_EXO)
sd(stuff$obs_chl_EXO)



