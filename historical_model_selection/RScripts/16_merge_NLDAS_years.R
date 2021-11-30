# merge NLDAS data to get 2013-2018 in one dataframe

library(tidyverse)

met1 <- read.csv("./historical_model_selection/Data/MET/FCR_GLM_met_NLDAS2_010113_010118_GMTadjusted.csv")
met2 <- read.csv("./historical_model_selection/Data/MET/FCR_GLM_met_NLDAS2_Dec14_Dec18.csv")
met1$time <- as.POSIXct(met1$time, format = "%Y-%m-%d %H:%M:%S")
met2$time <- as.POSIXct(met2$time, format = "%Y-%m-%d %H:%M:%S")

# met 2 has repeat data that is already in met1 (eg 2014 data)
# so subset met2 to only be 2018 data and then join with met1
met2 <- met2[met2$time > "2018-01-01 18:00:00",]

metall <- rbind(met1, met2)
plot(metall$time, metall$ShortWave)

write.csv(metall, "./historical_model_selection/Data/MET/FCR_GLM_NLDAS_010113_123118.csv", row.names = FALSE)

met_summ <- metall %>%                                                 # start with the raw data
  mutate(Date = as.Date(time, format="%Y-%m-%d hh:mm:ss")) %>% # create a column of just the date
  select(-time) %>%                                # drop the datetime column
  group_by(Date) %>%                                           # group by date (for daily statistics)
  summarise_all(c("mean", "median", "max","sum")) %>%           # get min, median, max, and sum for all variables each day
  select(-(ShortWave_sum:WindSpeed_sum), -Rain_median, -Rain_max) %>% # drop calcs you don't want
  select(Date, noquote(order(colnames(.))))                    # arrange columns alphabetically after Date

# subset out 2018 only for the validation dataset
met_18 <- met_summ[met_summ$Date>"2017-12-31",]
write.csv(met_18, "./historical_model_selection/Data/MET/NLDAS_2018_daily.csv", row.names = FALSE)
