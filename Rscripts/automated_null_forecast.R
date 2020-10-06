


folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
data_location <-  "C:/Users/wwoel/Desktop/FLARE_AR_CHLA/SCCData"
forecast_start_day <-"2019-01-02 00:00:00"
nmembers <- 420
timestep <- '1day'
timestep_numeric <- 1
timestep_interval <- 1
max_timestep <- 14 #max propagations of the timestep: daily = 14, weekly = 2, fortnightly = 1
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 14)
sim_name <- 'null_daily'
forecast_location <- paste0(folder, '/FCR_forecasts/', timestep, '/', sim_name)

start_day <- forecast_start_day 
start_day <- as.POSIXct(start_day, format = "%Y-%m-%d %H:%M:%S")


source(paste0(folder, "/", "Rscripts/generate_null_model.R"))


forecast_day_count <- 1
num_forecast_periods <- 750

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






