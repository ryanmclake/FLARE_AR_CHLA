# script to pull weekly driver data to update training dataset for AR model
# to be consistent with 2013-2016 dataset, use observed data from Mondays only

# assimilated data includes:
# chla response           source: from EXO sonde but converted into CTD units
# chla one week lag,      source: from EXO sonde but converted into CTD units
# shortwave radiation     source: FCR met station
# discharge               source: EDI through 2018 or diana pressure transducer


data_assimilation <- function(folder, data_location, hist_file, forecast_start_day){

library(lubridate)
library(tidyverse)

folder <- "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3"
data_location <-  "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3/SCCData"
#setwd(folder)

# read in the original training dataset from 2013-2016
# this is the format that the end file should have so that it can read in to the jags code
data <- read.csv(hist_file)
data$Date <- as.Date(data$Date)

if(tail(data$Date, n=1)<= as.Date(forecast_start_day)-7){
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
  full_time = seq(tail(data$Date, n=1)+7, as.Date(forecast_start_day) , by = '7 day')
  
  # gather chl data from exo sonde up to the current day
  chl_hist <- extract_chla_chain_dailyavg(fname = temp_obs_fname_wdir,
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
  
  met_obs_fname <- "FCRmet.csv"
  met_station_location <- paste0(data_location, "/", "carina-data")
  met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
  working_arima <- paste0(folder, "/", "ARIMA_working")  
  met_update_outfile <- paste0(working_arima, "/", "update_met.csv")
  #download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','./SCCData/carina-data/FCRmet.csv')
  
  
  full_time_hour_obs <- seq(  as.POSIXct(paste0((tail(data$Date, n=1)+7), " 00:00:00")), 
                              as.POSIXct(forecast_start_day),
                              by = "1 hour")
  
  source(paste0(folder,"/","Rscripts/create_obs_met_input_DA.R"))
  
  create_obs_met_input_DA(fname = met_obs_fname_wdir,
                       outfile= met_update_outfile,
                       full_time_hour_obs = full_time_hour_obs, 
                       input_tz = "EST5EDT", 
                       output_tz = 'EST')
  # read in the hourly data that was created and summarize to daily mean
  setwd(folder)
  sw_hist <- read.csv('./ARIMA_working/update_met.csv')
  sw_hist_daily <- sw_hist %>% mutate(Date = date(time)) %>% 
    group_by(Date) %>% 
    mutate(ShortWave_mean = mean(ShortWave)) %>% 
    select(Date, ShortWave_mean)
  sw_hist_daily <- sw_hist_daily[!duplicated(sw_hist_daily$Date),]
  
  
  #######################################################################################################################################################################################
  ####### gather discharge driver data #######################################################
  ##########################################################################################################################################################################
  # two data sources for discharge data
  # for 2018-07-05 through 2019-06-03 (this is the most recent file for wvwa flow data)
  # after 2019-06-03, use diana data but convert to wvwa units
  
  
  # download the latest diana weir file
 #download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','./SCCData/FCRweir.csv')
  dianaheader<-read.csv("./SCCData/FCRweir.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
  dianadata<-read.csv("./SCCData/FCRweir.csv", skip=4, header=F) #get data minus wonky Campbell rows
  names(dianadata)<-names(dianaheader) #combine the names to deal with Campbell logger formatting
  dianadata$TIMESTAMP <- as.POSIXct(dianadata$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
  colnames(dianadata)[colnames(dianadata)=="Lvl_psi"] <- "diana_psi_corr"
  dianadata <- dianadata %>% select("TIMESTAMP", "diana_psi_corr")
  
  # the old weir equations are taken directly from MEL's Inlow Aggregation script
  dianadata_pre <- dianadata[dianadata$TIMESTAMP< as.POSIXct('2019-06-06 09:30:00'),]
  dianadata_pre <- dianadata_pre %>% mutate(diana_flow1 = (diana_psi_corr )*0.70324961490205 - 0.1603375 + 0.03048) %>% 
    mutate(diana_flow_cfs = (0.62 * (2/3) * (1.1) * 4.43 * (diana_flow1 ^ 1.5) * 35.3147)) %>% 
    mutate(flow_cms = diana_flow_cfs*0.028316847   )%>% 
    select(TIMESTAMP, diana_psi_corr, flow_cms)
  
  # q = 2.391 * H^2.5
  # where H = head in meters above the notch
  # the head was 14.8 cm on June 24 at ~13:30
  #14.8 cm is 0.148 m 
  #14.9cm on Jun 27 at 3:49PM
  dianadata_post <- dianadata[dianadata$TIMESTAMP > as.POSIXct('2019-06-07 00:00:00'),]
  dianadata_post <- dianadata_post %>%  mutate(head = (0.149*diana_psi_corr)/0.293) %>% 
    mutate(flow_cms = 2.391* (head^2.5)) %>% 
    select(TIMESTAMP, diana_psi_corr, flow_cms)
  
  dianadata <- rbind(dianadata_pre, dianadata_post)                                                   
  
  # calculate daily values
  discharge_diana_daily <- dianadata %>% 
    select(TIMESTAMP, flow_cms) %>% 
    mutate(Date = date(TIMESTAMP))%>% 
    group_by(Date) %>% 
    summarise_all('mean') %>% 
    select(-TIMESTAMP)
  
  # convert diana into wvwa units (equation taken from 'thesis/r scripts/lm_wvwa_diana_pressuredata.R' on 10-08-2019)
  discharge_diana_daily_wvwaunits <- discharge_diana_daily %>% 
    mutate(flow_cms_diana_wvwaunits = (flow_cms*0.713454 + (-0.004732)))
  
  # throw out data 2019-06-04 through 2019-06-06 because the plug was removed from the weir in prep for replacing weir face so numbers are artificially low
  discharge_diana_daily_wvwaunits <- discharge_diana_daily_wvwaunits[discharge_diana_daily_wvwaunits$Date!=as.Date('2019-06-04'),]
  discharge_diana_daily_wvwaunits <- discharge_diana_daily_wvwaunits[discharge_diana_daily_wvwaunits$Date!='2019-06-05',]
  discharge_diana_daily_wvwaunits <- discharge_diana_daily_wvwaunits[discharge_diana_daily_wvwaunits$Date!='2019-06-06',]
  
  discharge_diana_daily_wvwaunits <- discharge_diana_daily_wvwaunits %>% select(-flow_cms)
  colnames(discharge_diana_daily_wvwaunits) <- c('Date', 'mean_flow')
  
  
  update <- left_join(chl_update, discharge_diana_daily_wvwaunits)
  update <- left_join(update, sw_hist_daily)
  
  # now join with the original training data
  data_assimilate <- rbind(data, update)
  data_assimilate <- data_assimilate[order(data_assimilate$Date),]
  
  # create the lag for new datapoints
  data_assimilate <- data_assimilate %>% mutate(Chla_ARlag1_sqrt = ifelse(Chla_ARlag1_sqrt>0, Chla_ARlag1_sqrt, lag(Chla_sqrt, n = 1L)))
  
  
  #get rid of na's
  data_assimilate <- na.omit(data_assimilate)
  
  write.csv(data_assimilate,'data_arima_working.csv' , row.names = FALSE)
  print('newly updated')
  
}else{
  data_assimilate <- data[data$Date<forecast_start_day,]
  write.csv(data_assimilate, 'data_arima_working.csv', row.names = FALSE)
  print('limited to forecast day range')}
}
