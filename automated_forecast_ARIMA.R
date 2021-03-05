
#############################################################################################################
#### code from 'automate_forecast_example.R' and modified for FCR total chlorophyll ARIMA  model ############
#############################################################################################################

if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")
if (!"RcppRoll" %in% installed.packages()) install.packages("RcppRoll")
if (!"rjags" %in% installed.packages()) install.packages("rjags")
if (!"PerformanceAnalytics" %in% installed.packages()) install.packages("PerformanceAnalytics")



library(mvtnorm)
library(lubridate)
library(RCurl)
library(testit)
library(imputeTS)
library(tidyverse)
library(modelr)
library(RcppRoll)




data_location <-  "C:/Users/wwoel/Desktop/FLARE_AR_CHLA/SCCData"
folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
timestep <- '1day' # character definition of the timestep
timestep_numeric <- 1
timestep_interval <- 1 # the interval in between timesteps, e.g. 4day would be 4; daily would be 1; weekly would be 7
max_timestep <- 14 #maximum number of timesteps that can be propagated to the max time horizon (e.g., daily is 14, weekly is 2)
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 14)
sim_name <- 'Mar2021_UC'
forecast_location <- paste0(folder, "/FCR_forecasts", '/', timestep, '/', sim_name)


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

#set up ensemblesa
n_ds_members <- 1
n_met_members <- 21
n_discharge_members <- 21
nmembers <- n_ds_members*n_met_members*n_discharge_members
num_forecast_periods <- 365 # number of times the script will loop through automation


# initialize forecast time
forecast_start_day <-"2019-11-15 00:00:00" # day the forecast initialized
start_day <- forecast_start_day 
start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S")
hist_days <- 1

source(paste0(folder, "/", "Rscripts/run_arima_any_timestep_test_UC_all.R"))

forecast_day_count <- 1
#ALL SUBSEQUENT DAYS
repeat{
  
  startTime <- Sys.time()
  
  
  #LOOP TO KEEP CHECKING FOR A NOAA FORECAST
  forecast_avialable = FALSE
  while(forecast_avialable == FALSE){
    forecast_start_time <- start_day + days(1)
    if(day(forecast_start_time) < 10){
      forecast_day <- paste0('0',day(forecast_start_time))
    }else{
      forecast_day <- paste0(day(forecast_start_time))
    }
    if(month(forecast_start_time) < 10){
      forecast_month <- paste0('0',month(forecast_start_time))
    }else{
      forecast_month <- paste0(month(forecast_start_time))
    }
    forecast_base_name <- paste0('fcre_', year(forecast_start_time),forecast_month,forecast_day,'_gep_all_00z.csv')
    
    noaa_location <- paste0(data_location,'/','noaa-data')
    #setwd(noaa_location)
    #system(paste0('git pull'))
    
    if(!file.exists(paste0(noaa_location,'/',forecast_base_name))){
      print('Waiting for NOAA forecast')
      Sys.sleep(wait_time)
    }else{
      forecast_avialable = TRUE
    }
  }
  
  
  
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
    max_horizon = max_horizon    )
  
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






