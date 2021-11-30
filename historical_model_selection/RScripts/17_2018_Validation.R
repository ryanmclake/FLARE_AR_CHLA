# run the ARIMA predictable model on 2018 driver data for validation

library(rsq)
library(tidyverse)
library(Metrics)

# read in driver data for the model (2018 inflow, met, and chla data)
met <- read.csv("./historical_model_selection/Data/MET/NLDAS_2018_daily.csv")
met$Date <- as.Date(met$Date)

inflow <- read.csv("./historical_model_selection/Data/Inflow/FCR_inflow_WVWA_2013_2019.csv")

# some data wrangling to get inflow in the proper shape
inflow$DateTime <- as.POSIXct(inflow$DateTime, format = "%Y-%m-%d %H:%M:%S")
inflow <- inflow[inflow$DateTime>"2017-12-31 23:45:00",]
inflow <- inflow %>% select(DateTime, Flow_cms) 
inflow$Date <- as.Date(inflow$DateTime, format = "%Y-%m-%d")
inflow <- inflow %>% group_by(Date) %>%
  mutate(mean_flow = mean(Flow_cms, na.rm = TRUE))
inflow <- inflow %>% select(Date, mean_flow)
inflow <- distinct(inflow)

met_flow <- left_join(inflow, met )

ctd <- read.csv("./historical_model_selection/Data/CTD/FCR_CTD_50_1m_2018.csv")
ctd$Date <- as.Date(ctd$Date, format = "%Y-%m-%d")

data <- left_join(ctd, met_flow)
data <- data %>% select(Date, Chla_ugL, mean_flow, ShortWave_mean)

# create the AR1 lag
data$Chla_lag1 <- lag(data$Chla_ugL)

# transformations
data$Chla_sqrt <- sqrt(data$Chla_ugL)
data$Chla_ARlag1_sqrt <- sqrt(data$Chla_lag1)

# now the model equation!!!!!!
data <- data %>% mutate(pred = 1.65 + 0.45*Chla_ARlag1_sqrt - 3.05*mean_flow - 0.0025*ShortWave_mean)

# also make a prediction using the 13-16 lm
data1316 <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")
data1316$Date <- as.Date(data1316$Date)
data1316 <- data1316[data1316$Date>"2013-05-09",]

mod1_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow +ShortWave_mean, 
                 data = data1316, family = gaussian, na.action = 'na.fail')
pred1_1316 <- predict(mod1_1316, newdata = data)


plot(data$pred, data$Chla_sqrt)
abline(0, 1)
plot(pred1_1316, data$Chla_sqrt)

plot(data$pred, pred1_1316)
abline(0,1)

plot(data$Date, (data$Chla_sqrt)^2, type = 'l', lwd = 2, xlab = "Date", ylab = 'Chlorophyll a (ug/L)')
points(data$Date, (data$pred)^2, col = 'orangered3', type = 'l', lwd = 2)
legend('topleft', c('Observed', 'ARIMA Modeled'), lty = c(1,1), col = c('black', 'orangered3'), bty = 'n')
#points(data_plot$Date, (pred1_1316)^2, col = 'purple', type = 'l')
#points(data$Date, data$Chla_ugL, col = 'purple')
par(mfrow=c(2,2))
plot(data$Date, (data$Chla_sqrt)^2, type = 'l', lwd = 2, xlab = "Date", ylab = 'Chlorophyll a (ug/L)')
points(data$Date, (data$pred)^2, col = 'orangered3', type = 'l', lwd = 2)
plot(data$Date, data$ShortWave_mean, col = 'red')
plot(data$Date, data$mean_flow, col = 'blue')
plot(data$Date, data$pred, type = 'l')
plot(data$Date, data$Chla_lag1, col = 'green')
# use these to calculate r2 and rmse
round((rsq(mod1_1316, type = 'sse')), digits = 3)
rmse((pred1_1316)^2, data$Chla_ugL)

# subset these two so there are no NA's/they are the same length
pred1_1316_narm <- pred1_1316[2:39]
data_rmse <- data[2:39,]
rmse((pred1_1316_narm)^2, data_rmse$Chla_ugL)
