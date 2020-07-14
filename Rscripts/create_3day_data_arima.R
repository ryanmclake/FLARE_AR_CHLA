# script to create dataframe that is on a 4-day timestep for training forecasts which use SW and discharge as drivers

folder <- 'C:/Users/wwoel/Desktop/FLARE_AR_CHLA'

train <- read.csv(paste0(folder, '/data_arima_highfrequency_through_2019.csv'))
train$Date <- as.Date(train$Date)
# make 3 day lag instead of one day?
train <- train %>% mutate(Chla_ARlag_timestep_sqrt = lag(Chla_sqrt, 3L))
days <- seq(as.Date('2018-08-15'), as.Date('2019-12-22'), by = '3 days')

train <- train[train$Date %in% as.Date(days), ]
train <- na.omit(train)


# there is a gap from 2018-08-19 until 2018-08-31 because of missing exo data from some days in that time period
write.csv(train, paste0(folder, '/data_arima_3day_through_2019.csv'), row.names = FALSE)
