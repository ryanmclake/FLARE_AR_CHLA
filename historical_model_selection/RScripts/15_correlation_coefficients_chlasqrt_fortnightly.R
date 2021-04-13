# script to calculate correlation coefficients between variables in the model and to get a first look at 
# what data transformations are necessary

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("Hmisc")
library(Hmisc)
library(tidyverse)

data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)

# create 14 day timestep
# make 14 day lag instead of 7 day
data <- data %>% mutate(Chla_ARlag_timestep_sqrt = lag(Chla_sqrt, 2L))

# remove every other datapoint to get on ~fortnightly timestep
remove <- seq(2, nrow(data), 2)

data_fortnight <- data[-remove,]
write.csv(data_fortnight, './historical_model_selection/ARIMA_data/arima_data_fortnightly.csv', row.names = FALSE)


# get rid of desciptor columns (depth, week identifiers, etc.), so only possible drivers are left
data_fortnight <- data_fortnight %>%
  select(-Depth,-Date, -(week_julian:week_cum))


##############################################################################################################################################
####################  create correlation matrices for each year #########################################################################
# not using chart.Correlation because there are too many variables to assess in a table like this
#x <- chart.Correlation(data, method = "spearman", histogram = TRUE)

cor <- rcorr(as.matrix(data_fortnight), type = "spearman")
spear <- cor$r
write.csv(spear, "./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_2016_fortnightly_data.csv", row.names = FALSE)



