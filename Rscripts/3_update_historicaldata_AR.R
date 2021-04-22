###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 03 ---- 
# script to pull weekly driver data to update training dataset for AR model
# to be consistent with 2013-2016 dataset, use observed data from Mondays only

# assimilated data includes:
# chla response           source: from EXO sonde but converted into CTD units
# chla one week lag,      source: from EXO sonde but converted into CTD units
# shortwave radiation     source: FCR met station
# discharge               source: EDI through 2018 or diana pressure transducer


# read in the original training dataset from 2013-2016
# this is the format that the end file should have so that it can read in to the jags code
train <- read.csv('./training_datasets/data_arima_WW.csv')
train$Date <- as.Date(train$Date)

#######################################################################################################################################################################################
### gather EXO data for chl response and lag driver data 
#######################################################################################################################################################################################
chl_hist_CTD <- read.csv('./sim_files/FCR_CTD_2018_2019.csv')
chl_hist_CTD$Date <- as.Date(chl_hist_CTD$Date)
chl_hist_CTD <- chl_hist_CTD %>% mutate(Chla_sqrt = sqrt(Chla_ugL)) %>% 
  mutate(Chla_ARlag1_sqrt = sqrt(chla_lag))
chl_hist_CTD <- chl_hist_CTD %>%  select(-Chla_ugL, -chla_lag)
#2019 CTD doesn't start until June--add in exo data but convert to ctd units

source(paste0(folder,"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) 
#this is the original file modified by WW to take a daily avg rather than the midnight reading
source(paste0(folder,"/","Rscripts/temp_oxy_chla_qaqc.R")) 


observed_depths_chla_fdom <- 1
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
cleaned_temp_oxy_chla_file <- paste0(working_arima, "/Catwalk_postQAQC.csv")
temp_oxy_chla_qaqc(data_file = temp_obs_fname_wdir[1], 
                   maintenance_file = paste0(data_location, '/mia-data/CAT_MaintenanceLog.txt'), 
                   output_file = cleaned_temp_oxy_chla_file)

new_temp_obs_fname_wdir <- temp_obs_fname_wdir
new_temp_obs_fname_wdir[1] <- cleaned_temp_oxy_chla_file

# change the function to 'extract_chla_chain_dailyavg
chl_hist <- extract_chla_chain_dailyavg(fname = new_temp_obs_fname_wdir,
                                        full_time,
                                        depths = 1.0,
                                        observed_depths_chla_fdom = observed_depths_chla_fdom,
                                        input_tz = "EST5EDT", 
                                        output_tz = reference_tzone)


date_vector <- data.frame(full_time)
chl_hist_daily <- data.frame(cbind(date_vector, chl_hist[[1]][,1])  )
colnames(chl_hist_daily) <- c('Date', 'Chla_EXO')
chl_hist_daily <- chl_hist_daily %>% mutate(Chla_sqrt_EXO = sqrt(Chla_EXO*0.55 - 0.0308)) %>%  #convert into CTD units and then take the square root
  select(-Chla_EXO) %>%                                                                   # drop the EXO data
  mutate(Chla_ARlag1_sqrt = lag(Chla_sqrt_EXO, n = 7L))                                       # create the weekly lag
# select only the time period I want to add to the CTD data (ie, where CTD data doesn't exist in the past)
chl_hist_daily <- chl_hist_daily[chl_hist_daily$Date>=as.Date('2019-04-22'),]
chl_hist_daily <- chl_hist_daily %>% mutate(dow  = rep(1:7, length.out = nrow(chl_hist_daily)))
chl_hist_weekly <- chl_hist_daily[chl_hist_daily$dow==7,]
chl_hist_weekly <- chl_hist_weekly %>% select(-dow)
colnames(chl_hist_weekly) <- c('Date', 'Chla_sqrt', 'Chla_ARlag1_sqrt')
chl_hist_weekly$Date <- as.Date(chl_hist_weekly$Date)

chl_hist_join <- rbind(chl_hist_CTD, chl_hist_weekly)

#######################################################################################################################################################################################
########### gather FCR met data up to the current day  
#######################################################################################################################################################################################

met_obs_fname <- "FCRmet.csv"
met_station_location <- paste0(data_location, "/", "carina-data")
met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
working_arima <- paste0(folder, "/", "ARIMA_working")  
AR_obs_met_outfile <- paste0(working_arima, "/", "AR_historical_met.csv")
full_time_hour_obs <- seq(as.POSIXct('2018-04-20 00:00:00'), 
                          as.POSIXct(Sys.Date()),
                          by = "1 hour")
  
source(paste0(folder,"/","Rscripts/create_obs_met_input.R"))

create_obs_met_input(fname = met_obs_fname_wdir,
                                  outfile= AR_obs_met_outfile,
                                  full_time_hour_obs = full_time_hour_obs, 
                                  input_tz = "EST5EDT", 
                                  output_tz = reference_tzone)
# read in the hourly data that was created and summarize to daily mean

sw_hist <- read.csv('./ARIMA_working/AR_historical_met.csv')
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

discharge_wvwa <- read.csv('./sim_files/FCR_inflow_WVWA_2013_2019.csv')
discharge_wvwa <- na.omit(discharge_wvwa)
discharge_wvwa_daily <- discharge_wvwa %>%  
  select(DateTime, Flow_cms) %>% 
  mutate(Date = date(DateTime)) %>% 
  group_by(Date) %>% 
  summarise_all('mean') %>% 
  select(-DateTime)
# subset to 2018-07-05
discharge_wvwa_daily <- discharge_wvwa_daily[discharge_wvwa_daily$Date>as.Date('2018-04-20'),]
# remove days when the weir was unplugged just prior to replacing the weir face in 2019
discharge_wvwa_daily <- discharge_wvwa_daily[discharge_wvwa_daily$Date!=as.Date('2019-06-04'),]
discharge_wvwa_daily <- discharge_wvwa_daily[discharge_wvwa_daily$Date!=as.Date('2019-06-05'),]
discharge_wvwa_daily <- discharge_wvwa_daily[discharge_wvwa_daily$Date!=as.Date('2019-06-06'),]
colnames(discharge_wvwa_daily) <- c('Date', 'flow_cms_wvwa')

# download the latest diana weir file
download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','./SCCData/FCRweir.csv')
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
colnames(discharge_diana_daily_wvwaunits) <- c('Date', 'flow_cms_diana')
plot(discharge_diana_daily_wvwaunits$Date, discharge_diana_daily_wvwaunits$flow_cms_diana)


discharge_daily <- full_join(discharge_diana_daily_wvwaunits, discharge_wvwa_daily)

plot(discharge_daily$flow_cms_diana, discharge_daily$flow_cms_wvwa)
plot(discharge_daily$Date, discharge_daily$flow_cms_diana, type = 'l')
points(discharge_daily$Date, discharge_daily$flow_cms_wvwa, col = 'green', type ='l')
abline(h=0)

# select wvwa data when it is available and use diana converted to wvwa units when it is not
discharge_daily <- discharge_daily %>% mutate(mean_flow = if_else(is.na(flow_cms_wvwa), flow_cms_diana, flow_cms_wvwa))
discharge <- discharge_daily %>% select(Date, mean_flow)

data <- left_join(chl_hist_join, discharge)
data <- left_join(data, sw_hist_daily)


# now join with the original training data
data_assimilate <- rbind(train, data)
data_assimilate <- data_assimilate[order(data_assimilate$Date),]
plot(data_assimilate$Date, data_assimilate$Chla_sqrt)

#get rid of na's
data_assimilate <- na.omit(data_assimilate)

write.csv(data_assimilate,'data_arima_updated.csv' , row.names = FALSE)
