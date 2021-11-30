#### Demo for summarizing met station data by day using tidyverse ####
#install.packages('tidyverse')
library(tidyverse)

raw <- read.csv('./historical_model_selection/Data/MET/FCR_GLM_met_NLDAS2_010113_010118_GMTadjusted.csv')
head(raw) # View data preview with auto-parsed structures 

# Calculate all statistics for all varaibles, then drop the ones you don't need
met <- raw %>%                                                 # start with the raw data
  mutate(Date = as.Date(time, format="%Y-%m-%d")) %>% # create a column of just the date
  select(-time) %>%                                # drop the datetime column
  group_by(Date) %>%                                           # group by date (for daily statistics)
  summarise_all(c("mean", "median", "max","sum")) %>%           # get min, median, max, and sum for all variables each day
  select(-(ShortWave_sum:WindSpeed_sum), -Rain_median, -Rain_max)# %>% # drop calcs you don't want
  select(Date, everything())                    # arrange columns alphabetically after Date

#met <- met %>%
#  select(-(LongWave_max:Rain_mean), -ShortWave_median)

View(met) # Look at your output

# Reshape to "long" data for ease of plotting
met_long <- met %>% 
  gather(Metric, Value, AirTemp_max:WindSpeed_median) %>%
  separate(Metric, c('Variable','Statistic'), sep='_')
View(met_long)

ggplot(met_long, aes(x = Date, y = Value, group = Variable, col = Statistic)) +
  geom_line() +
  facet_grid(Variable ~ ., scales = "free_y")

write.csv(met, "./historical_model_selection/Data/Met_FCR_daily.csv")
