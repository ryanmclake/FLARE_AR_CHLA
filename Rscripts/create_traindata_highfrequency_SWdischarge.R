# script to create training data for daily forecasts using weekly model covariates (mean SW and discharge)
library(tidyverse)
library(lubridate)

# training data should run from 2018-08-15 to 2019-12-31 (and will get approriately subsetted using with the data assimilation script)

source("./Rscripts/extract_EXOchl_chain_dailyavg.R")
temperature_location <-  "./SCCData/mia-data"

temp_obs_fname <- "Catwalk.csv"
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
observed_depths_chla_fdom <- 1
reference_tzone <- "EST"
full_time = seq(as.Date('2018-08-14'), as.Date('2019-12-31'), by = '1 day')
#full_time = seq(as.Date(forecast_start_day), as.Date(forecast_start_day)+16 , by = timestep)


source("./Rscripts/temp_oxy_chla_qaqc.R")
observed_depths_chla_fdom <- 1
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
cleaned_temp_oxy_chla_file <-  "./Catwalk_postQAQC.csv"
temp_oxy_chla_qaqc(data_file = temp_obs_fname_wdir[1], 
                   maintenance_file =  './SCCData/mia-data/CAT_MaintenanceLog.txt', 
                   output_file = cleaned_temp_oxy_chla_file)

new_temp_obs_fname_wdir <- temp_obs_fname_wdir
new_temp_obs_fname_wdir[1] <- cleaned_temp_oxy_chla_file


# gather chl data from exo sonde up to the current day
chl_hist <- extract_chla_chain_dailyavg(fname = new_temp_obs_fname_wdir,
                                        full_time = full_time,
                                        depths = 1.0,
                                        observed_depths_chla_fdom = observed_depths_chla_fdom,
                                        input_tz = "EST5EDT", 
                                        output_tz = "GMT")

date_vector <- data.frame(full_time)
chl_update <- data.frame(cbind(date_vector, chl_hist[[1]][,1])  )
colnames(chl_update) <- c('Date', 'Chla_EXO')
chl_update <- chl_update %>% mutate(Chla_sqrt = sqrt(Chla_EXO*0.55 - 0.0308)) %>%  #convert into CTD units and then take the square root
  select(-Chla_EXO) %>% 
  mutate(Chla_ARlag1_sqrt = lag(Chla_sqrt)) # put in zero here because will calculate the lag later so top most entry takes it lagged value from the earlier dataset
chl_update$Date <- as.Date(chl_update$Date)
data <- chl_update


# bring in discharge data
flow <- read.csv('./SCCData/manual-data/inflow_for_EDI_2013_06Mar2020.csv')
flow <- flow %>% select(DateTime, WVWA_Flow_cms)
# get rid of Nas
flow <- na.omit(flow)
flow_avg <- flow %>% mutate(Date = date(DateTime)) %>% 
  group_by(Date) %>% 
  mutate(WVWA_avg_cms = mean(WVWA_Flow_cms, na.rm = TRUE)) %>% 
  select(Date, WVWA_Flow_cms) %>% 
  distinct(Date,.keep_all = TRUE)

flow_avg <- as.data.frame(flow_avg)
data <- left_join(data, flow_avg)

# bring in met SW data
met_station_location <-  "./SCCData/carina-data"
met_obs_fname_wdir <-paste0(met_station_location, "/FCRmet.csv")
working_arima <-  "./ARIMA_working"
met_update_outfile <- paste0(working_arima, "/", "update_met.csv")

full_time_hour_obs <- seq(  as.POSIXct( "2018-08-15 00:00:00"), 
                            (as.POSIXct("2020-01-02")), 
                            by = "1 hour")

source("./Rscripts/create_obs_met_input_DA.R")

create_obs_met_input_DA(fname = met_obs_fname_wdir,
                        outfile= met_update_outfile,
                        full_time_hour_obs = full_time_hour_obs, 
                        input_tz = "EST5EDT", 
                        output_tz = 'EST')
# read in the hourly data that was created and summarize to daily mean
met_hist <- read.csv('./ARIMA_working/update_met.csv')
met_hist_daily <- met_hist %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(RelHum_mean = mean(RelHum, na.rm = TRUE)) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  select(Date, RelHum_mean, ShortWave_mean)%>% 
  distinct(Date,.keep_all = TRUE)


data <- left_join(data, met_hist_daily)
data <- data %>% select(-RelHum_mean)
colnames(data) <- c('Date', 'Chla_sqrt', 'Chla_ARlag1_sqrt', 'mean_flow', 'ShortWave_mean')

write.csv(data, 'data_arima_daily_SWdischarge.csv', row.names = FALSE)

