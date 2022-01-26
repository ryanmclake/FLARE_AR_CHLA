###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 06b ---- >
# Forecast with null persistence (i.e. Random walk) model

forecast_start_day <-"2019-01-02 00:00:00" # day the forecast initialized

#set up ensembles
n_ds_members <- 1
n_met_members <- 21
n_discharge_members <- 21
nmembers <- n_ds_members*n_met_members*n_discharge_members

timestep <- '14day'
timestep_numeric <- 14
timestep_interval <- 14
max_timestep <- 1 #max propagations of the timestep: daily = 14, weekly = 2, fortnightly = 1
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 14)
sim_name <- 'null_fortnightly_01Mar21'
forecast_location <- paste0(folder, '/FCR_forecasts/', timestep, '/', sim_name)

start_day <- forecast_start_day 
start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S")


source(paste0(folder, "/", "Rscripts/run_null_forecast.R"))


forecast_day_count <- 1
num_forecast_periods <- num_forecast_periods

#ALL SUBSEQUENT DAYS
repeat{
  
  startTime <- Sys.time()
  hist_days <- 1
  spin_up_days <- 0
  
  
  forecast <- run_null(
    folder,
    forecast_location,
    forecast_start_day,
    nmembers,
    timestep,
    timestep_numeric,
    timestep_interval,
    max_timestep )
  
  forecast_day_count <- forecast_day_count + 1
  
  restart_file <- unlist(forecast)[1]
  
  
  
  #ADVANCE TO NEXT DAY
  start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S") + days(1)
  forecast_start_day <- start_day
  if(!is.na(num_forecast_periods)){
    if(forecast_day_count > num_forecast_periods){
      break
    }
  }
  
}