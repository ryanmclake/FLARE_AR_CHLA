# script to look at the difference in predictive performance between a dataset with a few interpolated chl datapoints and dataset with no interpolated chl data
library(tidyverse)
library(lubridate)

data <- read.csv("./historical_model_selection/ARIMA_data/FCR_VT_data_2013_2017.csv")
data$Date <- as.Date(data$Date)
data <- data[data$Depth==1.0,]
data <- data %>% select(Date, Chla_ugL)

met <- read.csv("./historical_model_selection/Data/MET/Met_FCR_daily.csv")
met <- met %>% select(Date, ShortWave_mean)
met$Date <- as.Date(met$Date)

flow <- read.csv('./historical_model_selection/Data/Inflow/FCR_inflow_WVWA_2013_2019.csv')
flow <- flow %>% mutate(Date = date(DateTime)) %>% 
  group_by(Date) %>% 
  mutate(mean_flow = mean(Flow_cms))
flow <- flow %>% select(Date, mean_flow)
flow <- flow[!duplicated(flow$Date),]
flow$Date <- as.Date(flow$Date)
flow <- as.data.frame(flow)

data <- left_join(data, met)
data <- left_join(data, flow)

data <- data %>% mutate(Chla_sqrt = sqrt(Chla_ugL)) %>% 
  mutate(Chla_ARlag1_sqrt = lag(Chla_sqrt, n = 1L))

data <- data %>% mutate(month = month(Date))
data <- data[data$month>4 & data$month < 11,]
data <- data[data$Date < '2016-12-31',]
data <- data[data$Date > '2013-05-15',]

# randomly subset so that both the interpolated data and original dataset have the same number of observations
data <-   sample_n(data, 107)
data <- data[order(data$Date),]
data <- na.omit(data)
# and to compare to the dataset which I have been using which includes n = 18 interpolated data points
data_interp <- read.csv('.//historical_model_selection/ARIMA_data/data_arima_WW.csv')
data_interp <- na.omit(data_interp)

data_lm <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt + mean_flow + ShortWave_mean, data = data)
data_lm_pred <- predict(data_lm, newdata = data)
summary(data_lm)
summary(data_lm_pred)


data_interp_lm <- glm(Chla_sqrt ~ Chla_ARlag1_sqrt + mean_flow + ShortWave_mean, data = data_interp)
data_interp_lm_pred <- predict(data_interp_lm, newdata = data)

png('C:/Users/wwoel/Dropbox/Thesis/Writing/Ch 1 AR Forecasts/SI Materials/Interpolation_Fig.png', width = 1200, height = 785)
plot(  data$Date, (data$Chla_sqrt)^2, xlab = 'Date', ylab = 'Chla (µg/L)', cex = 1.5, cex.lab = 1.5, cex.axis = 1.5)
points(data$Date, (data_lm_pred)^2, type = 'l', col = 'red')
points(data$Date, (data_interp_lm_pred)^2, type = 'l', col = 'blue', cex = 1.5)
legend('topleft', c('no interpolation', '18 data points interpolated'), col = c('red', 'blue'), lty = c(1,1), bty = 'n', cex = 1.5)

dev.off()

library(rsq)
rsq(data_lm)
rsq(data_interp_lm)
