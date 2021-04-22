library(tidyverse)

# take the weekly dataset and add in meteorological data

data <- read.csv("./historical_model_selection/ARIMA_data/interpolated_weeks_2013_2016.csv")
met <- read.csv("./historical_model_selection/Data/MET/Met_FCR_daily.csv")
met <- met %>% select(-X)

all <- left_join(data, met)
all <- all %>% select(-(week_julian:week_cum), everything())

write.csv(all, "./historical_model_selection/ARIMA_data/variables_all_2013_2016.csv", row.names = FALSE)
