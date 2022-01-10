# script to create historical training dataset for bvr chl
## historical dataset should run through the entire time period when I want to forecast
### forecasts could start 2020-06-25 when EXO data starts

library(tidyverse)
library(lubridate)

###### chl data, taken from published ctd data at BVR
destination <- "./sim_files" # file downloaded in 'RUN_FIRST_initialize_repo.R'

ctd <- read.csv(paste0(destination, '/CTD_final_2013_2020.csv'))
ctd <- ctd[ctd$Reservoir =='BVR' & ctd$Site == '50',]

# pull from 1.0m
layer <- ctd %>% 
  mutate(Date = date(Date)) %>% 
  group_by(Date) %>% 
  slice(which.min(abs(as.numeric(Depth_m) - 1.0))) %>% 
  dplyr::select(Date, Chla_ugL)

###### add chl lag
layer <- layer %>% 
  mutate(year = year(Date)) 

layer$Chla_ARlag1_ugL <- NA

# create lag, but exclude the observations where the previous observation is from the previous year
for(i in 2:nrow(layer)){
  if(layer$year[i]==layer$year[i-1]){
    layer$Chla_ARlag1_ugL[i] <- layer$Chla_ugL[i-1]  
  }
    
}

layer <- na.omit(layer)

layer <- layer %>% 
  mutate(Chla_sqrt = sqrt(Chla_ugL),
         Chla_ARlag_timestep_sqrt = sqrt(Chla_ARlag1_ugL)) %>% 
  dplyr::select(Date, Chla_sqrt, Chla_ARlag_timestep_sqrt)
plot(layer$Date, layer$Chla_sqrt)
plot(layer$Chla_ARlag_timestep_sqrt, layer$Chla_sqrt)

weeks <- seq(max(layer$Date)+7, Sys.Date(), by = '1 week')
weeks <- as.data.frame(weeks)
names(weeks) <- 'Date'
layer <- rbind(layer, weeks)

# now add in EXO data
#  compiled in 'Combine_old_BVR_files.R', data comes from github 
exo<-read.csv("./SCCData/bvre-data/bvre-waterquality_2020-06-18_2021-10-11.csv") #get data minus wonky Campbell rows
exo$TIMESTAMP <- as.POSIXct(exo$TIMESTAMP)
exo <- exo %>% 
  dplyr::select(TIMESTAMP, Chla_1) %>% 
  filter(Chla_1 != "NAN")%>%
  group_by(TIMESTAMP) %>% 
  mutate(EXOChla_daily_mean = mean(Chla_1)) %>% 
  distinct(TIMESTAMP, .keep_all = TRUE) %>% 
  mutate(Chla_sqrt = sqrt(EXOChla_daily_mean),
         Chla_sqrt_CTDunits = 0.07 + 0.67*Chla_sqrt) # coefficients come from 'calculate_EXO_CTD_regression_stats_BVR' on the sqrt-transformed regression
plot(sqrt(exo$EXOChla_daily_mean), exo$Chla_sqrt_CTDunits)
abline(0, 1)
plot(exo$TIMESTAMP, exo$EXOChla_daily_mean)
plot(exo$TIMESTAMP, exo$Chla_sqrt_CTDunits^2)
plot(exo$TIMESTAMP, ((exo$Chla_sqrt_CTDunits - 0.07)/0.67)^2)

exo <- exo %>% dplyr::select(TIMESTAMP, Chla_sqrt_CTDunits)
names(exo) <- c('Date' ,'Chla_sqrt')
exo <- as.data.frame(exo)
#exo <- exo[exo$Date > as.Date('2020-11-10'),]
plot(exo$Date, ((exo$Chla_sqrt- 0.07)/0.67)^2)

# join with CTD chl data
layer <- as.data.frame(layer)
chl <- left_join(layer, exo, by = c("Date" = "Date"))
chl <- chl %>% 
  mutate(Chla_sqrt = ifelse(is.na(Chla_sqrt.x), Chla_sqrt.y, Chla_sqrt.x)) %>% 
  dplyr::select(Date, Chla_sqrt, Chla_ARlag_timestep_sqrt) %>% 
  mutate(Chla_ARlag_timestep_sqrt = ifelse(is.na(Chla_ARlag_timestep_sqrt), lag(Chla_sqrt), Chla_ARlag_timestep_sqrt))

plot(chl$Date, chl$Chla_sqrt^2)
check <- chl %>% 
  mutate(chl_in_exo_units = ifelse(Date > as.Date('2020-11-10'), ((Chla_sqrt - 0.07)/0.67)^2, Chla_sqrt^2))
plot(as.Date(check$Date), check$chl_in_exo_units)

###### discharge data, created by HLW using watershed model, published somewhere?
flow <- read.csv(file.path('./', 'SCCData', 'bvre-data', 'BVR_flow_calcs_obs_met_2021-10-13.csv'))
flow <- flow[,-1]

flow <- flow %>% 
  mutate(mean_flow = flow$Q_BVR_m3.d/24/60/60) %>% 
  mutate(Date = as.Date(time)) %>% 
  dplyr::select(Date, mean_flow)



###### shortwave data
sw_hist <- read.csv('./historical_model_selection/Data/MET/Met_FCR_daily.csv')
sw_hist <- sw_hist %>% dplyr::select(Date, ShortWave_mean)
sw_hist$Date <- as.Date(sw_hist$Date)

# and add 2018 met data
met_obs_fname <- "FCRmet_legacy_2018.csv"
met_station_location <- "./SCCData/carina-data"
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
working_arima <- "./ARIMA_working"
AR_obs_met_outfile <- paste0(working_arima, "/", "update_met_2018.csv")
reference_tzone <- "GMT"
full_time_hour_obs <- seq(as.POSIXct('2018-04-10 00:00:00'), 
                          as.POSIXct('2018-12-31 00:00:00'),
                          by = "1 hour")

source("./Rscripts/create_obs_met_input.R")

create_obs_met_input(fname = met_obs_fname_wdir,
                     outfile= AR_obs_met_outfile,
                     full_time_hour_obs = full_time_hour_obs, 
                     input_tz = "EST5EDT", 
                     output_tz = reference_tzone)
# read in the hourly data that was created and summarize to daily mean

sw_hist_2018 <- read.csv('./ARIMA_working/update_met_2018.csv')
sw_hist_daily_2018 <- sw_hist_2018 %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  dplyr::select(Date, ShortWave_mean)
sw_hist_daily_2018 <- sw_hist_daily_2018[!duplicated(sw_hist_daily_2018$Date),]
sw_hist_daily_2018 <- as.data.frame(sw_hist_daily_2018)
sw_hist_daily_2018$Date <- as.Date(sw_hist_daily_2018$Date)

# and add 2019 met data
met_obs_fname <- "FCRmet_legacy_2019.csv"
met_station_location <- "./SCCData/carina-data"
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
working_arima <- "./ARIMA_working"
AR_obs_met_outfile <- paste0(working_arima, "/", "update_met_2019.csv")
reference_tzone <- "GMT"
full_time_hour_obs <- seq(as.POSIXct('2019-01-01 00:00:00'), 
                          as.POSIXct('2019-12-31 00:00:00'),
                          by = "1 hour")

source("./Rscripts/create_obs_met_input.R")
create_obs_met_input(fname = met_obs_fname_wdir,
                     outfile= AR_obs_met_outfile,
                     full_time_hour_obs = full_time_hour_obs, 
                     input_tz = "EST5EDT", 
                     output_tz = reference_tzone)
# read in the hourly data that was created and summarize to daily mean
sw_hist_2019 <- read.csv('./ARIMA_working/update_met_2019.csv')
sw_hist_daily_2019 <- sw_hist_2019 %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  dplyr::select(Date, ShortWave_mean)
sw_hist_daily_2019 <- sw_hist_daily_2019[!duplicated(sw_hist_daily_2019$Date),]
sw_hist_daily_2019 <- as.data.frame(sw_hist_daily_2019)
sw_hist_daily_2019$Date <- as.Date(sw_hist_daily_2019$Date)

# and add 2020 met data
#download.file("https://github.com/FLARE-forecast/FCRE-data/blob/5a3751847fcbf5b31c3799aa6fb9d6e662873b08/FCRmet.csv?raw=true", "./SCCData/carina-data/FCRmet_legacy_2020.csv")
met_obs_fname <- "FCRmet_legacy_2020.csv"
met_station_location <- "./SCCData/carina-data"
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
working_arima <- "./ARIMA_working"
AR_obs_met_outfile <- paste0(working_arima, "/", "update_met_2020.csv")
reference_tzone <- "GMT"
full_time_hour_obs <- seq(as.POSIXct('2020-01-01 00:00:00'), 
                          as.POSIXct('2020-12-31 00:00:00'),
                          by = "1 hour")

create_obs_met_input(fname = met_obs_fname_wdir,
                     outfile= AR_obs_met_outfile,
                     full_time_hour_obs = full_time_hour_obs, 
                     input_tz = "EST5EDT", 
                     output_tz = reference_tzone)
# read in the hourly data that was created and summarize to daily mean
sw_hist_2020 <- read.csv('./ARIMA_working/update_met_2020.csv')
sw_hist_daily_2020 <- sw_hist_2020 %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  dplyr::select(Date, ShortWave_mean)
sw_hist_daily_2020 <- sw_hist_daily_2020[!duplicated(sw_hist_daily_2020$Date),]
sw_hist_daily_2020 <- as.data.frame(sw_hist_daily_2020)
sw_hist_daily_2020$Date <- as.Date(sw_hist_daily_2020$Date)

# and add 2021 met data
#download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-metstation-data/FCRmet.csv?raw=true", './SCCData/carina-data/FCRmet.csv')
met_obs_fname <- "FCRmet.csv"
met_station_location <- "./SCCData/carina-data"
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
working_arima <- "./ARIMA_working"
AR_obs_met_outfile <- paste0(working_arima, "/", "update_met_2021.csv")
reference_tzone <- "GMT"
full_time_hour_obs <- seq(as.POSIXct('2021-01-01 00:00:00'), 
                          as.POSIXct(Sys.Date()),
                          by = "1 hour")

create_obs_met_input(fname = met_obs_fname_wdir,
                     outfile= AR_obs_met_outfile,
                     full_time_hour_obs = full_time_hour_obs, 
                     input_tz = "EST5EDT", 
                     output_tz = reference_tzone)
# read in the hourly data that was created and summarize to daily mean
sw_hist_2021 <- read.csv('./ARIMA_working/update_met_2021.csv')
sw_hist_daily_2021 <- sw_hist_2021 %>% mutate(Date = date(time)) %>% 
  group_by(Date) %>% 
  mutate(ShortWave_mean = mean(ShortWave, na.rm = TRUE)) %>% 
  dplyr::select(Date, ShortWave_mean)
sw_hist_daily_2021 <- sw_hist_daily_2021[!duplicated(sw_hist_daily_2021$Date),]
sw_hist_daily_2021 <- as.data.frame(sw_hist_daily_2021)
sw_hist_daily_2021$Date <- as.Date(sw_hist_daily_2021$Date)

sw_all <- rbind(sw_hist, sw_hist_daily_2018, sw_hist_daily_2019, sw_hist_daily_2020, sw_hist_daily_2021)
plot(sw_all$Date, sw_all$ShortWave_mean)


###### combine all data: chl, sw, flow

c <- left_join(chl, sw_all)
c <- left_join(c, flow)
c <- na.omit(c)
write.csv(c, './training_datasets/data_arima_7day_BVR.csv', row.names = FALSE)




