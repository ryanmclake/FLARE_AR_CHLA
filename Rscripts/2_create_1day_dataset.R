
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 02 ---- 
# script to create high-frequency catawlk data from 2018 through 2020
folder <- getwd()
data_location <- paste0(getwd(),"/","SCCData")
working_arima <- paste0(folder, '/ARIMA_working')
source("./Rscripts/extract_EXOchl_chain_dailyavg.R")

temperature_location <-  "./SCCData/mia-data"

temp_obs_fname <- "Catwalk.csv"
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
observed_depths_chla_fdom <- 1
reference_tzone <- "EST"
full_time <- seq(as.Date('2018-08-15'), as.Date(Sys.Date()), by = 'day')


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
  dplyr::select(-Chla_EXO) %>% 
  mutate(Chla_ARlag1_sqrt = lag(Chla_sqrt)) # put in zero here because will calculate the lag later so top most entry takes it lagged value from the earlier dataset
chl_update$Date <- as.Date(chl_update$Date)
data <- chl_update


# bring in discharge data
source(paste0(folder,"/","Rscripts/inflow_qaqc.R"))


cleaned_inflow_file <- paste0(working_arima, "/FCRinflow_postQAQC.csv")
inflow_file1 <- c(paste0(data_location,"/diana-data/FCRweir.csv"),
                  paste0(folder,"/sim_files/FCR_inflow_WVWA_2013_2019.csv"))

inflow_qaqc(fname = inflow_file1,
            cleaned_inflow_file ,
            local_tzone = 'EST', 
            input_file_tz = 'EST',
            working_arima)

full_time = seq(as.Date('2018-08-14'), as.Date(Sys.Date()), by = '1 day')
Discharge <- read.csv(paste0(folder,"/", "ARIMA_working/FCRinflow_postQAQC.csv"))
Discharge$Date <- as.Date(Discharge$time)  
Discharge <- Discharge %>% dplyr::select(Date, FLOW)  
colnames(Discharge) <- c('Date', 'mean_flow')

data <- left_join(data, Discharge)


# Now SW data
# first for data in 2018 (have to do this separately because met data file is separated by year)
met_station_location <- paste0(data_location, "/", "carina-data")
met_obs_fname <- 'FCRmet_legacy_2018.csv'
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
working_arima <- paste0(folder, "/", "ARIMA_working")  
met_update_outfile <- paste0(working_arima, "/", "update_met.csv")


full_time_hour_obs <- seq(  as.POSIXct(paste0((head(data$Date, n=1)), " 00:00:00")), 
                            as.POSIXct(paste0((tail(data$Date, n=1)), " 00:00:00")), 
                            by = "1 hour")

source(paste0(folder,"/","Rscripts/create_obs_met_input.R"))

create_obs_met_input(fname = met_obs_fname_wdir,
                     outfile= met_update_outfile,
                     full_time_hour_obs = full_time_hour_obs, 
                     input_tz = "EST5EDT", 
                     output_tz = 'EST')
# read in the hourly data that was created and summarize to daily mean
met_hist_2018 <- read.csv('./ARIMA_working/update_met.csv')
met_hist_daily_2018 <- met_hist_2018 %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(RelHum_mean = mean(RelHum, na.rm = TRUE)) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  dplyr::select(Date, RelHum_mean, ShortWave_mean)
met_hist_daily_2018 <- met_hist_daily_2018[!duplicated(met_hist_daily_2018$Date),]
met_hist_daily_2018 <- met_hist_daily_2018 %>% dplyr::select(Date, ShortWave_mean)
met_hist_daily_2018 <- as.data.frame(met_hist_daily_2018)

###### do the same for 2019
met_station_location <- paste0(data_location, "/", "carina-data")
met_obs_fname <- 'FCRmet_legacy_2019.csv'
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
working_arima <- paste0(folder, "/", "ARIMA_working")  
met_update_outfile <- paste0(working_arima, "/", "update_met.csv")

full_time_hour_obs <- seq(  as.POSIXct( "2019-01-01 00:00:00"), 
                            as.POSIXct("2020-01-01 00:00:00"), 
                            by = "1 hour")

create_obs_met_input(fname = met_obs_fname_wdir,
                     outfile= met_update_outfile,
                     full_time_hour_obs = full_time_hour_obs, 
                     input_tz = "EST5EDT", 
                     output_tz = 'EST')
# read in the hourly data that was created and summarize to daily mean
met_hist_2019 <- read.csv('./ARIMA_working/update_met.csv')
met_hist_daily_2019 <- met_hist_2019 %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(RelHum_mean = mean(RelHum, na.rm = TRUE)) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  dplyr::select(Date, RelHum_mean, ShortWave_mean)
met_hist_daily_2019 <- met_hist_daily_2019[!duplicated(met_hist_daily_2019$Date),]
met_hist_daily_2019 <- met_hist_daily_2019 %>% dplyr::select(Date, ShortWave_mean)
met_hist_daily_2019 <- as.data.frame(met_hist_daily_2019)

# do the same for 2020 met data, which is in a different met file
met_obs_fname <- 'FCRmet_legacy_2020.csv'
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
full_time_hour_obs <- seq(  as.POSIXct("2020-01-01 00:00:00"), 
                            as.POSIXct("2020-12-31 00:00:00"), 
                            by = "1 hour")
create_obs_met_input(fname = met_obs_fname_wdir,
                     outfile= met_update_outfile,
                     full_time_hour_obs = full_time_hour_obs, 
                     input_tz = "EST5EDT", 
                     output_tz = 'EST')
met_hist <- read.csv('./ARIMA_working/update_met.csv')
met_hist_daily_2020 <- met_hist %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(RelHum_mean = mean(RelHum, na.rm = TRUE)) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  dplyr::select(Date, RelHum_mean, ShortWave_mean)
met_hist_daily_2020 <- met_hist_daily_2020[!duplicated(met_hist_daily_2020$Date),]

met_hist_daily_2020 <- met_hist_daily_2020 %>% dplyr::select(Date, ShortWave_mean)
met_hist_daily_2020 <- as.data.frame(met_hist_daily_2020)

##### do the same for 2021 met data, which is in a different met file
met_obs_fname <- 'FCRmet.csv'
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
full_time_hour_obs <- seq(  as.POSIXct("2021-01-01 00:00:00"), 
                            as.POSIXct("2021-12-31 00:00:00"), 
                            by = "1 hour")
create_obs_met_input(fname = met_obs_fname_wdir,
                     outfile= met_update_outfile,
                     full_time_hour_obs = full_time_hour_obs, 
                     input_tz = "EST5EDT", 
                     output_tz = 'EST')
met_hist <- read.csv('./ARIMA_working/update_met.csv')
met_hist_daily_2021 <- met_hist %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(RelHum_mean = mean(RelHum, na.rm = TRUE)) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  dplyr::select(Date, RelHum_mean, ShortWave_mean)
met_hist_daily_2021 <- met_hist_daily_2020[!duplicated(met_hist_daily_2021$Date),]

met_hist_daily_2021 <- met_hist_daily_2020 %>% dplyr::select(Date, ShortWave_mean)
met_hist_daily_2021 <- as.data.frame(met_hist_daily_2021)


met_hist_daily <- rbind(met_hist_daily_2018, met_hist_daily_2019, met_hist_daily_2020, met_hist_daily_2021)


# combine data into one dataframe
data <- left_join(data, met_hist_daily)
colnames(data) <- c('Date', 'Chla_sqrt', 'Chla_ARlag1_sqrt', 'mean_flow', 'ShortWave_mean')

# add col names by timestep lag
data <- data %>% 
  mutate(Chla_ARlag_timestep_sqrt = lag(Chla_sqrt, 1L))

write.csv(data, paste0(folder, '/training_datasets/data_arima_1day.csv'), row.names = FALSE)

