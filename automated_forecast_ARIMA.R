
#############################################################################################################
#### code from 'automate_forecast_example.R' and modified for FCR total chlorophyll ARIMA  model ############
#############################################################################################################

if (!"mvtnorm" %in% installed.packages()) install.packages("mvtnorm")
if (!"ncdf4" %in% installed.packages()) install.packages("ncdf4")
if (!"lubridate" %in% installed.packages()) install.packages("lubridate")
if (!"glmtools" %in% installed.packages()) install.packages("glmtools",
                                                            repos=c("http://cran.rstudio.com",
                                                                    "http://owi.usgs.gov/R"))
if (!"RCurl" %in% installed.packages()) install.packages("RCurl")
if (!"testit" %in% installed.packages()) install.packages("testit")
if (!"imputeTS" %in% installed.packages()) install.packages("imputeTS")
if (!"tidyverse" %in% installed.packages()) install.packages("tidyverse")


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
timestep_numeric <- 1 # maybe timestep_numeric and timestep_interval are actually the same thing and not both needed -_-
timestep_interval <- 1 # the interval in between timesteps, e.g. 4day would be 4; daily would be 1; weekly would be 7
max_timestep <- 14 #maximum number of timesteps that can be propagated to the max time horizon (e.g., daily is 16)
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 16)
sim_name <- '17Jul2020'
forecast_location <- paste0("C:/Users/wwoel/Desktop/FLARE_AR_CHLA/FCR_forecasts", '/', timestep, '/', sim_name)


restart_file <- NA
spin_up_days <- 0
push_to_git <- FALSE
pull_from_git <- TRUE
reference_tzone <- "GMT"
forecast_days <-16
DOWNSCALE_MET <- FALSE # should this be TRUE???
FLAREversion <- "v1.0_beta.1"
met_ds_obs_start = as.Date("2018-04-06")
met_ds_obs_end = Sys.Date()
uncert_mode = 4
null_model = TRUE
data_assimilation = TRUE

#Note: this number is multiplied by 
# 1) the number of NOAA ensembles (21)
# 2) the number of downscaling essembles (50 is current)
# get to the total number of essembles
#n_enkf_members <- 1
n_ds_members <- 1
# SET UP NUMBER OF ENSEMBLE MEMBERS
n_met_members <- 21

num_forecast_periods <- 365


source(paste0(folder, "/", "Rscripts/run_arima_any_timestep.R"))


forecast_start_day <-"2020-01-02 00:00:00"
# the forecast start day is the day that the forecast is initialized, the two days of 'forecasts' are produced for 1 week and 2 weeks into 
# the future from this day
start_day <- forecast_start_day 
start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S")
#hist_days <- as.numeric(difftime(as.POSIXct(forecast_start_day, tz = reference_tzone),
#                                 as.POSIXct(start_day, tz = reference_tzone)))
hist_days <- 1

local_tzone <- "EST5EDT"
include_wq <<- FALSE
use_future_inflow <<- TRUE


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
    forecast_days = 16,  
    spin_up_days = 0,
    restart_file = NA,
    folder = folder, 
    forecast_location = forecast_location,
    push_to_git = FALSE,
    pull_from_git = TRUE, 
    data_location = data_location, 
    #nmembers = NA,
    n_ds_members = 1,
    uncert_mode = uncert_mode,
    reference_tzone = reference_tzone,
    downscaling_coeff = NA,
    DOWNSCALE_MET = FALSE,
    FLAREversion = FLAREversion,
    null_model = TRUE,
    data_assimilation = data_assimilation,
    timestep = timestep,
    timestep_numeric = timestep_numeric,
    timestep_interval = timestep_interval,
    max_timestep = max_timestep,
    max_horizon = max_horizon    )
  
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






