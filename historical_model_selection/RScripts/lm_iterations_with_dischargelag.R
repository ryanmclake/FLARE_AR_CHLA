


library(MuMIn)
library(knitr)
library(rsq)
library(tidyverse)
library(Metrics)

#all data
data <- read.csv("./Data/ARIMA_data/data_arima_WW.csv")
data$Date <- as.Date(data$Date)
data$mean_flow_lag1week <- lag(data$mean_flow) # create a one-week lag of discharge data
data <- na.omit(data) #because the first datapoint has an NA for the lag, get rid of that entry

#create a linear model with discharge data from one week prior
mod_1weeklag <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow_lag1week +ShortWave_mean, 
                 data = data, family = gaussian, na.action = 'na.fail')

hist(mod_1weeklag$residuals)
confint(mod_1weeklag)
sd(mod_1weeklag$residuals)
summary(mod_1weeklag)
mod_1weeklag
pred_1weeklag <- predict(mod_1weeklag)

# and create a linear model without the lag (i.e., discharge is on the forecasted timestep, rather than the t-1 timestep)
mod1_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow +ShortWave_mean, 
                 data = data, family = gaussian, na.action = 'na.fail')

hist(mod1_1316$residuals)
confint(mod1_1316)
sd(mod1_1316$residuals)
summary(mod1_1316)
pred_mod1_1316 <- predict(mod1_1316)

plot(data$Date, data$Chla_sqrt, type = 'l')
points(data$Date, pred_1weeklag, col = 'red', type = 'l')
points(data$Date, pred_mod1_1316, col = 'blue', type = 'l')

# some model diagnostics
round((rsq(mod1_1316, type = 'sse')), digits = 3)
round((rsq(mod_1weeklag, type = 'sse')), digits = 3)

rmse((pred_mod1_1316)^2, (data$Chla_sqrt)^2)
rmse((pred_1weeklag)^2, (data$Chla_sqrt)^2)

plot(data$mean_flow_lag1week, data$mean_flow)

data <- data %>% select(-mean_flow)
write.csv(data, './Data/ARIMA_data/data_arima_dischargelag.csv', row.names = FALSE)
