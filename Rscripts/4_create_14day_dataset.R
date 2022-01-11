###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 05 ---- 
# create fortnightly training dataset for running 2-week forecasts

train <- read.csv(paste0(folder, '/training_datasets/data_arima_7day.csv'))
train$Date <- as.Date(train$Date)
# make 14 day lag instead of 7 day
train <- train %>% mutate(Chla_ARlag_timestep_sqrt = lag(Chla_sqrt, 2L))

# remove every other datapoint to get on ~fortnightly timestep
remove <- seq(2, nrow(train), 2)

train_fortnight <- train[-remove,]
train_fortnight <- na.omit(train_fortnight)

# file with fortnightly timestep through the forecast period (Dec 2019)
write.csv(train_fortnight, paste0(folder, '/training_datasets/data_arima_14day.csv'), row.names = FALSE)

