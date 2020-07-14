# create fortnightly training dataset for running 2-week forecasts

folder <- 'C:/Users/wwoel/Desktop/FLARE_AR_CHLA'

# create file with weekly data through 2019 that I will later subset for fortnightly
# run once, shouldn't need to be updated
#folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
#source(paste0(folder,"/","Rscripts/data_assimilation_AR.R"))
outfile <- 'data_arima_weekly_through_2019.csv'
#forecast_start_day <-"2019-12-30 00:00:00"
#met_obs_fname <- "FCRmet.csv"
#data_location = "C:/Users/wwoel/Desktop/FLARE_AR_CHLA/SCCData"
#data_assimilation(folder = folder, 
#                  data_location =  data_location,
#                  hist_file = paste0(folder, '/', 'data_arima_updated.csv'),
#                  forecast_start_day = forecast_start_day,
#                  timestep = 7, 
#                  outfile = outfile,
#                  met_obs_fname = met_obs_fname)


train <- read.csv(paste0(folder,'/', outfile))
train$Date <- as.Date(train$Date)
# make 14 day lag instead of 7 day
train <- train %>% mutate(Chla_ARlag_timestep_sqrt = lag(Chla_sqrt, 2L))

# remove every other datapoint to get on ~fortnightly timestep
remove <- seq(2, nrow(train), 2)

train_fortnight <- train[-remove,]
train_fortnight <- na.omit(train_fortnight)

# file with fortnightly timestep through the forecast period (Dec 2019)
write.csv(train_fortnight, paste0(folder, '/data_arima_fortnightly_through_2019.csv'), row.names = FALSE)

