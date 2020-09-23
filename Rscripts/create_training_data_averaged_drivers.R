# script to gather dicharge and met data for the 2013-2016 period and calculate averages, rather than point observations for each time step
library(tidyverse)
library(lubridate)

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
data <- data %>% mutate(residual_discharge = daily_mean_flow - weekly_mean_flow) %>% 
  mutate(residual_SW = daily_mean_shortwave - weekly_mean_shortwave)

par(mfrow = c(1,1))
plot(data$daily_mean_flow, data$weekly_mean_flow, ylab = 'Discharge averaged over Week', xlab = 'Discharge on predicted day')
abline(a = 0, b = 1)
plot(data$daily_mean_shortwave, data$weekly_mean_shortwave, ylab = 'Shortwave averaged over week', xlab = 'Shortwave on predicted day')
abline(a = 0, b = 1)

hist(data$residual_discharge)
hist(data$residual_SW)
plot(data$Date, data$daily_mean_flow, ylim = c(0, 0.14))
points(data$Date, data$residual_discharge, col = 'red')
plot(data$Date, data$daily_mean_shortwave, ylim = c(0, 350))
points(data$Date, data$residual_SW, col = 'red')
########################################################################################################################################################################
# run a model for each set of driver data (daily and weekly)

model_daily <- lm(Chla_sqrt ~ Chla_ARlag1_sqrt + daily_mean_flow + daily_mean_shortwave, data = data)
model_avg <- lm(Chla_sqrt ~ Chla_ARlag1_sqrt + weekly_mean_flow + weekly_mean_shortwave, data = data)

summary(model_daily)
summary(model_avg)

pred_daily <- predict.lm(model_daily)
pred_avg <- predict.lm(model_avg)

plot(data$Date, (data$Chla_sqrt)^2, ylab = 'Chla (ug/L, CTD units)', xlab = 'Date')
points(data$Date, (pred_daily^2), col = 'blue', type= 'l')
points(data$Date, (pred_avg^2), col = 'red', type= 'l')
legend('topright', c('drivers day of', 'drivers averaged over week'), col = c('blue', 'red'), lty = c(1,1), bty = 'n', cex = 0.75)

###########################################################################################################################################################################
## SI Figure
png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/averaged_vs_point_drivers_SI.png'), width = 1100, height = 800)
par(mar = c(5,5,4,2), mfrow = c(2,2))
plot(data$Date, (data$Chla_sqrt)^2, ylab = 'Chla (ug/L, CTD units)', xlab = 'Date', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 2)
points(data$Date, (pred_daily^2), col = 'blue', type= 'l', cex = 2)
points(data$Date, (pred_avg^2), col = 'red', type= 'l', cex = 2)
legend('topright', c('drivers day of', 'drivers averaged over week'), col = c('blue', 'red'), lty = c(1,1), bty = 'n', cex = 1.6)

par(mar = c(5,5,4,2))
plot(data$daily_mean_flow, data$weekly_mean_flow, ylab = 'Discharge averaged over Week', xlab = 'Discharge on predicted day', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 2)
abline(a = 0, b = 1)
par(mar = c(5,5,4,2))
plot(data$daily_mean_shortwave, data$weekly_mean_shortwave, ylab = 'Shortwave averaged over week', xlab = 'Shortwave on predicted day', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 2)
abline(a = 0, b = 1)
dev.off()
##########################################################################################################################################################################
# output the dataframe
write.csv(data, paste0(folder, '/data_arima_driver_average.csv'), row.names = FALSE)
