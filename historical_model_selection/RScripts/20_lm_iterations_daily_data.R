# gather met, inflow, and exo data to fit high-frequency autoregressive time series of exo chl data

library(tidyverse)
library(lubridate)

#download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','./Data/Catwalk/Catwalk.csv')
#download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','./Data/MET/FCRmet.csv')
#download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','./Data/Inflow/FCRweir.csv')

folder <- "."
data_location <-  "./SCCData"


source(paste0('C:/Users/wwoel/Desktop/FLARE_AR_CHLA',"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) #this is the original file modified to take a daily avg rather than the midnight reading
source(paste0('C:/Users/wwoel/Desktop/FLARE_AR_CHLA',"/","Rscripts/temp_oxy_chla_qaqc.R")) 

temperature_location <- paste0(data_location, "/", "mia-data")
temp_obs_fname <- "Catwalk.csv"
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
observed_depths_chla_fdom <- 1
reference_tzone <- "EST"
full_time = seq(as.Date('2018-08-15'), as.Date(Sys.time()) , by = '1 day')
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
cleaned_temp_oxy_chla_file <- paste0('C:/Users/wwoel/Desktop/FLARE_AR_CHLA/ARIMA_working', "/Catwalk_postQAQC.csv")
temp_oxy_chla_qaqc(data_file = temp_obs_fname_wdir[1], 
                   maintenance_file = paste0('C:/Users/wwoel/Desktop/FLARE_AR_CHLA/SCCData', '/mia-data/CAT_MaintenanceLog.txt'), 
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

met_obs_fname_wdir <-"./historical_model_selection/Data/MET/FCRmet.csv"
working_glm <- paste0(folder, "/", "GLM_working")  
met_update_outfile <-  "./historical_model_selection/Data/MET/update_met.csv"
#download.file('https://github.com/CareyLabVT/SCCData/raw/carina-data/FCRmet.csv','./SCCData/carina-data/FCRmet.csv')


full_time_hour_obs <- seq(  as.POSIXct('2018-08-15 00:00:00'), 
                            as.POSIXct(Sys.time()),
                            by = "1 hour")

source("./Rscripts/create_obs_met_input.R")

create_obs_met_input(fname = met_obs_fname_wdir,
                        outfile= met_update_outfile,
                        full_time_hour_obs = full_time_hour_obs, 
                        input_tz = "EST5EDT", 
                        output_tz = 'EST')
# read in the hourly data that was created and summarize to daily mean
setwd(folder)
sw_hist <- read.csv('./historical_model_selection/Data/MET/update_met.csv')


# and do the same for the legacy data before 2019
create_obs_met_input(fname = "./historical_model_selection/Data/MET/FCRmet_legacy01.csv",
                        outfile= "./historical_model_selection/Data/MET/update_met_legacy.csv" ,
                        full_time_hour_obs = full_time_hour_obs, 
                        input_tz = "EST5EDT", 
                        output_tz = 'EST')
sw_legacy<- read.csv('./historical_model_selection/Data/MET/update_met_legacy.csv')

met_all <- rbind(sw_legacy, sw_hist)
met_all$time <- as.POSIXct(met_all$time)

met_summary <- met_all %>%                                                 # start with the raw data
  mutate(Date = as.Date(time, format="%Y-%m-%d")) %>% # create a column of just the date
  select(-time) %>%                                # drop the datetime column
  group_by(Date) %>%                                           # group by date (for daily statistics)
  summarise_all(funs(mean, median, max, sum), na.rm = TRUE) %>% 
  select(-(ShortWave_sum:WindSpeed_sum), -Rain_median, -Rain_max) %>% # drop calcs you don't want
  select(Date, everything())                    # arrange columns alphabetically after Date



#######################################################################################################################################################################################
####### gather discharge driver data #######################################################
##########################################################################################################################################################################
# two data sources for discharge data
# for 2018-07-05 through 2019-06-03 (this is the most recent file for wvwa flow data)
# after 2019-06-03, use diana data but convert to wvwa units


# download the latest diana weir file
#download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','./SCCData/FCRweir.csv')
dianaheader<-read.csv("./historical_model_selection/Data/Inflow/FCRweir.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
dianadata<-read.csv("./historical_model_selection/Data/Inflow/FCRweir.csv", skip=4, header=F) #get data minus wonky Campbell rows
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

# convert diana into wvwa units (equation taken from 'thesis/r scripts/lm_wvwa_diana_pressuredata.R' on 10-08-2019)
dianadata <- dianadata %>%  mutate(flow_cms_diana_wvwaunits = (flow_cms*0.713454 + (-0.004732)))

# calculate daily values
discharge_diana_daily <- dianadata %>% 
  select(TIMESTAMP, flow_cms_diana_wvwaunits) %>% 
  mutate(Date = date(TIMESTAMP))%>% 
  group_by(Date) %>% 
 mutate(mean_flow = mean(flow_cms_diana_wvwaunits)) %>% 
  mutate(flow_max = max(flow_cms_diana_wvwaunits)) %>% 
  mutate(flow_min = min(flow_cms_diana_wvwaunits)) %>% 
  mutate(flow_median = median(flow_cms_diana_wvwaunits)) %>% 
  select(-TIMESTAMP, - flow_cms_diana_wvwaunits)

discharge_diana_daily <- discharge_diana_daily[!duplicated(discharge_diana_daily$Date),]
discharge_diana_daily <- discharge_diana_daily %>% select(Date, everything())


# throw out data 2019-06-04 through 2019-06-06 because the plug was removed from the weir in prep for replacing weir face so numbers are artificially low
discharge_diana_daily <- discharge_diana_daily[discharge_diana_daily$Date!=as.Date('2019-06-04'),]
discharge_diana_daily <- discharge_diana_daily[discharge_diana_daily$Date!='2019-06-05',]
discharge_diana_daily <- discharge_diana_daily[discharge_diana_daily$Date!='2019-06-06',]





# gather wvwa inflow data before april 22 2019
inf_old <- read.csv('./SCCData/manual-data/inflow_working_2019.csv')
inf_old$Date <- as.Date(inf_old$DateTime)
inf_old <- inf_old[inf_old$Date>as.Date('2018-08-14'),]

inf_old <- inf_old %>% group_by(Date) %>%
  mutate(mean_flow = mean(Flow_cms, na.rm = TRUE)) %>% 
  mutate(flow_max = max(Flow_cms, na.rm = TRUE)) %>% 
  mutate(flow_min = min(Flow_cms, na.rm = TRUE)) %>% 
  mutate(flow_median = median(Flow_cms, na.rm = TRUE)) %>% 
  select(Date, mean_flow:flow_median)
inf_old <- inf_old[!duplicated(inf_old$Date),]
inf_old <- as.data.frame(inf_old)

# subset diana data to time period where wvwa is unavailable
inf_diana <- discharge_diana_daily[discharge_diana_daily$Date>as.Date('2019-09-20'),]
inf_diana <- as.data.frame(inf_diana)

inf_all <- rbind(inf_old, inf_diana)

update <- left_join(chl_update, inf_all)
update <- left_join(update, met_summary)


# create the lag for new datapoints
update <- update %>% mutate(Chla_ARlag1_sqrt = ifelse(Chla_ARlag1_sqrt>0, Chla_ARlag1_sqrt, lag(Chla_sqrt, n = 1L)))


#get rid of na's
update <- update %>% 
  select(-Snow_max, -Snow_sum, -Snow_median, -Rain_mean)

# make some transformations of data to fit lm assumptions
update <- update %>% mutate(WindSpeed_max_log = log(WindSpeed_max)) %>% 
  mutate(WindSpeed_mean_log = log(WindSpeed_mean)) %>% 
  mutate(WindSpeed_median_log = log(WindSpeed_median + 0.6845246)) %>% 
  mutate(Rain_sum_log = log(Rain_sum + 0.006096)) %>% 
  mutate(AirTemp_max_log = log(AirTemp_max)) %>% 
  mutate(AirTemp_mean_log = log(AirTemp_mean)) %>% 
  mutate(AirTemp_median_log = log(AirTemp_median)) %>% 
  mutate(RelHum_max_log = log(RelHum_max)) %>% 
  select(-WindSpeed_max, -WindSpeed_mean, -WindSpeed_median, -Rain_sum, -AirTemp_max, -AirTemp_mean, -AirTemp_median, -RelHum_max)

update <- na.omit(update)

# save the dataset as a data file
write.csv(update, './historical_model_selection/ARIMA_data/EXO_plusdrivers_AR.csv', row.names = FALSE)



############################################################################################################################################################################
# create a correlation matrix to be exported and analyzed in xcel to remove correlated variables greater than abs(0.5)
# some code below to compare between variables to decide which to keep

install.packages('Hmisc')
library(Hmisc)

# get rid of desciptor columns (depth, week identifiers, etc.), so only possible drivers are left
data <- update %>%
  select(-Date)
data <- na.omit(data)

cor <- rcorr(as.matrix(data), type = "spearman")
spear <- cor$r
write.csv(spear, "./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/dailymodel_correlationmatrix_2013_2016.csv", row.names = FALSE)

attach(update)
compare <- function(variab){
  x <- lm(Chla_sqrt~variab)
  plot(variab, Chla_sqrt)
  abline(x)
  summary(x)
}

compare(LongWave_max)
compare(LongWave_mean)
compare(mean_flow)
compare(flow_median)
compare(RelHum_mean)
compare(RelHum_max_log)
compare(RelHum_median)
compare(Rain_sum_log)
compare(WindSpeed_median_log)
compare(WindSpeed_mean_log)
compare(WindSpeed_max_log)
compare(LongWave_median)
compare(ShortWave_max)
compare(ShortWave_mean)
compare(ShortWave_median)
# final selected variables: mean_flow, RelHum_mean, Shortwave_median, Windspeed_max
##############################################################################################################################################
# model selection using dredge function
library(MuMIn)
update <- read.csv('./historical_model_selection/ARIMA_data/EXO_plusdrivers_AR.csv')
update$Date <- as.Date(update$Date)
# subset to Dec 15, 2018 to only the training period
update <- update[update$Date<"2018-12-16",]

all_model <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow + RelHum_mean + ShortWave_median + WindSpeed_max_log, 
           data = update, family = gaussian, na.action = 'na.fail')
glm <- dredge(all_model, rank = 'AICc', fixed = 'Chla_ARlag1_sqrt')
select_model <- subset(glm, delta<2)


hum_model <- glm(Chla_sqrt~Chla_ARlag1_sqrt + RelHum_mean, data = update, family= gaussian, na.action = 'na.fail')

pred_model <- predict(hum_model, newdata = update)
source(paste0('C:/Users/wwoel/Desktop/FLARE_AR_CHLA',"/","Rscripts/model_assessment.R"))
model_metrics(pred_model, update$Chla_sqrt)



# subset the datasets to just the columns needed for the model above
data_hf <- update %>% select(Date, Chla_sqrt, Chla_ARlag1_sqrt, RelHum_mean)
write.csv(data_hf, row.names = FALSE, 'C:/Users/wwoel/Desktop/FLARE_AR_CHLA/training_datasets/data_arima_highfrequency_RelHum_mean.csv')
