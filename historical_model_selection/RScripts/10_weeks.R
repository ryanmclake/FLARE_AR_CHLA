# script to select one date only from within each weekly timeframe for the entire 2013-2017 dataset

library(tidyverse)

# read in the dataset that includes only the timeframe of the model (May-Oct 2013-2016) and includes the weeks that have been
# interpolated
data <- read.csv("./historical_model_selection/ARIMA_data/data_interpolated_plusinflowcalcs_MayOct13_16.csv")
data$Date <- as.Date(data$Date)

# create a week number
data$week_julian <- as.POSIXlt(data$Date)$yday %/% 7
look <- select(data, Date, week_julian)

# these is currently starting at the nth week of the year
# subtract by 16 to standardize to nth week of the time series
data <- data %>%
  group_by(Date)%>%
  mutate(week_series = week_julian -16)

# these sequences start over each year
# so add 27 to 2014 week_num_series, 54 to 2015, and 81 to 2016
data <- data %>%
  group_by(Date)%>%
  mutate(week_num_1 = (ifelse(Date > "2013-12-31" , week_series+27, week_series))) %>% #2014
  mutate(week_num_2 = (ifelse(Date > "2014-12-31" , week_series+54, week_num_1))) %>% #2015
  mutate(week_num_3 = (ifelse(Date > "2015-12-31" , week_series+81, week_num_2))) #2016
look2 <- select(data, Date, week_num_3)  

# clean up 
data <- data %>%
  select (-week_num_1, -week_num_2) %>%
  rename(week_cum = week_num_3)

##################################################################################################################################################
# there are too many datapoints within a single week in some cases
# in those cases, randomly select 1 out of n dates

timestep <- data %>%
  group_by(Depth, week_cum)%>%
  sample_n(1)

write.csv(timestep, "./historical_model_selection/ARIMA_data/interpolated_weeks_2013_2016.csv", row.names = FALSE)  















