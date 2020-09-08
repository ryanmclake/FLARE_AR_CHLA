# create obs chl-a vector from forecast time period to compare to forecasts
# read in the daily forecasts 

library(tidyverse)

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"

timestep <- '1day' # character definition of the timestep
timestep_numeric <- 1 # maybe timestep_numeric and timestep_interval are actually the same thing and not both needed -_-
timestep_interval <- 1 # the interval in between timesteps, e.g. 4day would be 4; daily would be 1; weekly would be 7
max_timestep <- 16 #maximum number of timesteps that can be propagated to the max time horizon
max_horizon <- 16 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 16)
sim_name <- '17Jul2020'
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/', sim_name)


obs <- read.csv(paste0(forecast_folder, '/day_', 14, '.csv'))
obs <- obs %>% select(forecast_date, forecast_run_day, obs_chl_EXO)
colnames(obs) <- c('forecast_date','forecast_run_day', 'obs_chl_EXO_on_forecast_date')

obs_2 <- read.csv(paste0(forecast_folder, '/day_', 1, '.csv'))
obs_2 <- obs_2 %>% select(forecast_date, forecast_run_day, obs_chl_EXO)
colnames(obs_2) <- c('forecast_date','forecast_run_day', 'obs_chl_EXO_on_forecast_date')

obs_2$forecast_date <- as.Date(obs_2$forecast_date)
obs$forecast_date <- as.Date(obs$forecast_date)
obs_2 <- obs_2[obs_2$forecast_date < '2019-01-16',]

obs <- rbind(obs_2, obs)

write.csv(obs,paste0(folder, '/obs_chla_02Jan2019_15Aug2020.csv'), row.names = FALSE)
