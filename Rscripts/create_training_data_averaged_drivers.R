# script to gather dicharge and met data for the 2013-2016 period and calculate averages, rather than point observations for each time step

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
data <- read.csv(paste0(folder, './data_arima_WW.csv'))
colnames(data) <- c('Date', 'Chla_sqrt', 'Chla_ARlag1_sqrt', 'daily_mean_flow', 'daily_mean_shortwave')
data$Date <- as.Date(data$Date)
data$weekly_mean_flow <- NA
data$weekly_mean_shortwave <- NA
data <- data %>% distinct(Date, .keep_all = TRUE)

flow <- read.csv(paste0(folder, '/SCCData/manual-data/inflow_for_EDI_2013_06Mar2020.csv'))
flow <- flow %>% mutate(Date = date(DateTime)) %>% 
  group_by(Date) %>% 
  mutate(Daily_Avg_cms = mean(WVWA_Flow_cms)) %>%  
  distinct(Date, .keep_all = TRUE) %>% 
  select(Date, Daily_Avg_cms)


met <- read.csv('C:/Users/wwoel/Dropbox/Thesis/Data/MET/Met_FCR_daily.csv')
met <- met %>% select(Date, ShortWave_mean)
met$Date <- as.Date(met$Date)

drivers <- left_join(flow, met)

for (i in 1:nrow(data)) {
  range <- seq(data$Date[i], data$Date[i+1]-1, by = 'day')
  temp <- drivers[drivers$Date %in% range,]
  data$weekly_mean_flow[i] <-  mean(temp$Daily_Avg_cms)
  data$weekly_mean_shortwave[i] <- mean(temp$ShortWave_mean)
}

data <- na.omit(data)

plot(data$daily_mean_flow, data$weekly_mean_flow)
abline(a = 0, b = 1)
plot(data$daily_mean_shortwave, data$weekly_mean_shortwave)
abline(a = 0, b = 1)

########################################################################################################################################################################
# run a model for each set of driver data (daily and weekly)

model_daily <- lm(Chla_sqrt ~ Chla_ARlag1_sqrt + daily_mean_flow + daily_mean_shortwave, data = data)
model_avg <- lm(Chla_sqrt ~ Chla_ARlag1_sqrt + weekly_mean_flow + weekly_mean_shortwave, data = data)

summary(model_daily)
summary(model_avg)

pred_daily <- predict.lm(model_daily)
pred_avg <- predict.lm(model_avg)

plot(data$Date, data$Chla_sqrt)
points(data$Date, pred_daily, col = 'blue', type= 'l')
points(data$Date, pred_avg, col = 'red', type= 'l')

##########################################################################################################################################################################
# output the dataframe
write.csv(data, paste0(folder, '/data_arima_driver_average.csv'), row.names = FALSE)
