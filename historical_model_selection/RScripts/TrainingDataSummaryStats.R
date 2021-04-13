# some summary info on the training dataset for chl-a forecasts

library(tidyverse)
library(lubridate)

# 2013-2016 dataset
data <- read.csv("./historical_model_selection/ARIMA_data/data_arima_WW.csv")
data$Date <- as.Date(data$Date)

plot(data$Date, data$Chla_sqrt)

# un-sqrt and convert to 'exo units'
data <- data %>% mutate(chl_EXO = (((Chla_sqrt^2) + 0.0308)/0.55))
plot(data$Date, data$chl_EXO)

min(data$chl_EXO)
max(data$chl_EXO)
median(data$chl_EXO)

# now the max values for each year
data$year <- year(data$Date)
years <- data %>% group_by(year) %>% mutate(max_chl = max(chl_EXO))
