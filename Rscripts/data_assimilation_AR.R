# script to pull weekly driver data to update training dataset for AR model
# to be consistent with 2013-2016 dataset, use observed data from Mondays only

# assimilated data includes:
# chla response           source: from EXO sonde but converted into CTD units
# chla one week lag,      source: from EXO sonde but converted into CTD units
# shortwave radiation     source: FCR met station
# discharge               source: EDI through 2018 or diana pressure transducer


data_assimilation <- function(folder, data_location, hist_file, forecast_start_day, timestep, outfile, met_obs_fname){

library(lubridate)
library(tidyverse)


setwd(folder)

# read in the original training dataset from 2013-2016
# this is the format that the end file should have so that it can read in to the jags code
data <- read.csv(hist_file)
data$Date <- as.Date(data$Date)

# remove dates between Apr 2018 and Aug 2018 in weekly dataset to make equal with daily training dataset
data <- data[data$Date<"2018-04-01" | data$Date>'2018-08-13',]
data <- data[!duplicated(data$Date),]

if(tail(data$Date, n=1)<= as.Date(forecast_start_day)-timestep){
  #######################################################################################################################################################################################
  ### gather EXO data for chl response and lag driver data 
  #######################################################################################################################################################################################
  
  source(paste0(folder,"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) #this is the original file modified to take a daily avg rather than the midnight reading
  
  temperature_location <- paste0(data_location, "/", "mia-data")
  
  temp_obs_fname <- "Catwalk.csv"
  temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
  setwd(folder)
  #download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','./SCCData/mia-data/Catwalk.csv')
  
  
  observed_depths_chla_fdom <- 1
  reference_tzone <- "EST"
  full_time = seq(tail(data$Date, n=1), as.Date(forecast_start_day) , by = timestep)
  #full_time = seq(as.Date(forecast_start_day), as.Date(forecast_start_day)+16 , by = timestep)
  
  
  source(paste0(folder,"/","Rscripts/temp_oxy_chla_qaqc.R")) 
  observed_depths_chla_fdom <- 1
  temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
  cleaned_temp_oxy_chla_file <- paste0(working_arima, "/Catwalk_postQAQC.csv")
  temp_oxy_chla_qaqc(data_file = temp_obs_fname_wdir[1], 
                     maintenance_file = paste0(data_location, '/mia-data/CAT_MaintenanceLog.txt'), 
                     output_file = cleaned_temp_oxy_chla_file)
  
  new_temp_obs_fname_wdir <- temp_obs_fname_wdir
  new_temp_obs_fname_wdir[1] <- cleaned_temp_oxy_chla_file
  
  
  # gather chl data from exo sonde up to the current day
  chl_hist <- extract_chla_chain_dailyavg(fname = new_temp_obs_fname_wdir,
                                          full_time = full_time,
                                          depths = 1.0,
                                          observed_depths_chla_fdom = observed_depths_chla_fdom,
                                          input_tz = "EST5EDT", 
                                          output_tz = reference_tzone)
  
  date_vector <- data.frame(full_time)
  chl_update <- data.frame(cbind(date_vector, chl_hist[[1]][,1])  )
  colnames(chl_update) <- c('Date', 'Chla_EXO')
  chl_update <- chl_update %>% mutate(Chla_sqrt = sqrt(Chla_EXO*0.55 - 0.0308)) %>%  #convert into CTD units and then take the square root
    select(-Chla_EXO) %>% 
    mutate(Chla_ARlag1_sqrt = 0) # put in zero here because will calculate the lag later so top most entry takes it lagged value from the earlier dataset
  chl_update$Date <- as.Date(chl_update$Date)
  
  #######################################################################################################################################################################################
  ########### gather FCR met data up to the current day  
  #######################################################################################################################################################################################
  
 
  met_station_location <- paste0(data_location, "/", "carina-data")
  met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
  working_arima <- paste0(folder, "/", "ARIMA_working")  
  met_update_outfile <- paste0(working_arima, "/", "update_met.csv")
 # download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','./SCCData/carina-data/FCRmet.csv')
  
  
  full_time_hour_obs <- seq(  as.POSIXct(paste0((tail(data$Date, n=1)), " 00:00:00")), 
                              (as.POSIXct(forecast_start_day)), 
                              by = "1 hour")

  source(paste0(folder,"/","Rscripts/create_obs_met_input_DA.R"))
  
  create_obs_met_input_DA(fname = met_obs_fname_wdir,
                       outfile= met_update_outfile,
                       full_time_hour_obs = full_time_hour_obs, 
                       input_tz = "EST5EDT", 
                       output_tz = 'EST')
  # read in the hourly data that was created and summarize to daily mean
  setwd(folder)
  met_hist <- read.csv('./ARIMA_working/update_met.csv')
  met_hist_daily <- met_hist %>% mutate(Date = date(time)) %>% 
    group_by(Date) %>% 
    mutate(RelHum_mean = mean(RelHum)) %>% 
    mutate(ShortWave_mean = mean(ShortWave)) %>% 
    select(Date, RelHum_mean, ShortWave_mean)
  met_hist_daily <- met_hist_daily[!duplicated(met_hist_daily$Date),]
  
  if(timestep>6){ #weekly model needs SW mean as a driver
    
    met_hist_daily <- met_hist_daily %>% select(Date, ShortWave_mean)
    met_hist_daily <- as.data.frame(met_hist_daily)
  }else{  #but daily model needs RelHum as a driver
    met_hist_daily <- met_hist_daily %>% select(Date, RelHum_mean)
    
    
  }
  
  if(timestep>6){
  #######################################################################################################################################################################################
  ####### gather discharge driver data #######################################################
  ##########################################################################################################################################################################
  # two data sources for discharge data
  # for 2018-07-05 through 2019-06-03 (this is the most recent file for wvwa flow data)
  # after 2019-06-03, use diana data but convert to wvwa units
  
  
    source(paste0(folder,"/","Rscripts/create_inflow_outflow_file_arima.R"))
    source(paste0(folder,"/","Rscripts/inflow_qaqc.R"))
    
    
    cleaned_inflow_file <- paste0(working_arima, "/FCRinflow_postQAQC.csv")
    inflow_file1 <- c(paste0(data_location,"/diana-data/FCRweir.csv"),
                       paste0(folder,"/sim_files/FCR_inflow_WVWA_2013_2019.csv"),
                       paste0(data_location,"/manual-data/inflow_working_2019.csv"))
    
    inflow_qaqc(fname = inflow_file1,
                cleaned_inflow_file ,
                local_tzone, 
                input_file_tz = 'EST',
                working_arima)
    
    full_time = seq(tail(data$Date, n=1), as.Date(forecast_start_day) , by = timestep)
    create_inflow_outflow_file(folder = folder,
                               full_time = full_time ,
                               working_arima = working_arima, 
                               input_tz = "EST5EDT",
                               output_tz = reference_tzone)
    # read in function output as driver data
    Discharge <- read.csv(paste0(folder,"/", "ARIMA_working/FCR_inflow.csv"))
    Discharge$Date <- as.Date(Discharge$time)  
    Discharge <- Discharge %>% select(Date, FLOW)  
    colnames(Discharge) <- c('Date', 'mean_flow')
  update <- left_join(chl_update, Discharge)
  update <- left_join(update, met_hist_daily)
  }else{
    
    update <- left_join(chl_update, met_hist_daily)
  }
  
  # now join with the original training data
  data_assimilate <- rbind(data, update)
  data_assimilate <- data_assimilate[order(data_assimilate$Date),]
  data_assimilate <- data_assimilate[!duplicated(data_assimilate$Date),]
  
  # create the lag for new datapoints
  data_assimilate <- data_assimilate %>% mutate(Chla_ARlag1_sqrt = ifelse(Chla_ARlag1_sqrt>0, Chla_ARlag1_sqrt, lag(Chla_sqrt, n = 1L)))
  
  
  #get rid of na's
  data_assimilate <- na.omit(data_assimilate)
  #data_assimilate <- data[data$Date<forecast_start_day,]
  write.csv(data_assimilate,'data_arima_working.csv' , row.names = FALSE)
  print('newly updated')
  
}else{
  
  
  data_assimilate <- data[data$Date<forecast_start_day,]
  write.csv(data_assimilate, outfile, row.names = FALSE)
  print('limited to forecast day range')
}



}
