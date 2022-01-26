###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 07a ---- 
# Forecast using SS-model structure

#############################################################################################################
#### code from 'automate_forecast_example.R' and modified for FCR total chlorophyll ARIMA  model ############
#############################################################################################################

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,lubridate,mvtnorm,
               RCurl,testit,imputeTS,modelr,
               RcppRoll,PerformanceAnalytics,
               rjags,LaplacesDemon,scales,
               Metrics,rsq)


folder <- getwd()
data_location <- paste0(getwd(),"/","SCCData")


timestep <- '7day' # character definition of the timestep
timestep_numeric <- 7
timestep_interval <- 7 # the interval in between timesteps, e.g. 4day would be 4; daily would be 1; weekly would be 7
max_timestep <- 2 #maximum number of timesteps that can be propagated to the max time horizon (e.g., daily is 14, weekly is 2)
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 14)
sim_name <- 'FCR_scalability_Aug2020-2021'
forecast_location <- paste0(folder, "/FCR_forecasts", '/', timestep, '/', sim_name)

site_id <- 'fcre'
restart_file <- NA
spin_up_days <- 0
push_to_git <- FALSE
pull_from_git <- TRUE
reference_tzone <- "GMT"
forecast_days <-max_horizon
DOWNSCALE_MET <- FALSE # if FALSE<-not accounting for uncertainty in meteorological downscaling of NOAA forecasts
met_ds_obs_start = as.Date("2018-04-06")
met_ds_obs_end = Sys.Date()
uncert_mode = 1
data_assimilation = TRUE
local_tzone <- "EST5EDT"
include_wq <<- FALSE
use_future_inflow <<- TRUE

#set up ensembles
n_ds_members <- 1
n_met_members <- 21
n_discharge_members <- 21
nmembers <- n_ds_members*n_met_members*n_discharge_members
num_forecast_periods <- 2 # number of times the script will loop through automation


# initialize forecast time
forecast_start_day <-"2020-08-01 00:00:00" # day the forecast initialized
start_day <- forecast_start_day 
start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S")
hist_days <- 1

source(paste0(folder, "/", "Rscripts/run_arima_any_timestep.R"))

forecast_day_count <- 1
#ALL SUBSEQUENT DAYS
repeat{
  
  startTime <- Sys.time()
  
  hist_days <- 1
  spin_up_days <- 0

  
  forecast <- run_arima(
    start_day= start_day,
    sim_name = sim_name, 
    hist_days = hist_days,
    forecast_days = forecast_days,  
    spin_up_days = spin_up_days,
    restart_file = restart_file,
    folder = folder, 
    forecast_location = forecast_location,
    push_to_git = push_to_git,
    pull_from_git = pull_from_git, 
    data_location = data_location, 
    nmembers = nmembers,
    n_ds_members = 1,
    uncert_mode = uncert_mode,
    reference_tzone = reference_tzone,
    downscaling_coeff = NA,
    DOWNSCALE_MET = DOWNSCALE_MET,
    data_assimilation = data_assimilation,
    timestep = timestep,
    timestep_numeric = timestep_numeric,
    timestep_interval = timestep_interval,
    max_timestep = max_timestep,
    max_horizon = max_horizon,
    site_id = site_id)
  
  forecast_day_count <- forecast_day_count + 1
  
 # restart_file <- unlist(forecast)[1]
  
  #ADVANCE TO NEXT DAY
  start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S") + days(1)
  forecast_start_day <- start_day
  if(!is.na(num_forecast_periods)){
    if(forecast_day_count > num_forecast_periods){
      break
    }
  }
  
}
