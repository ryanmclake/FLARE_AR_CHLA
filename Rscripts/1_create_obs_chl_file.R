
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 01 ---- 
### script to extract obs chl-a from the entire forecasting period and save as csv

### SPECIFY ALL OF THE DIRECTORIES YOU WILL USE FROM SCRIPTS 1-13
folder <- getwd()
data_location <- paste0(getwd(),"/","SCCData")

### Source the function scripts needed to get the observed chlorophyll a
source(paste0(folder,"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) 
source(paste0(folder,"/","Rscripts/temp_oxy_chla_qaqc.R")) 


observed_depths_chla_fdom <- 1
temp_obs_fname_wdir <- paste0(folder, "/SCCData/mia-data/Catwalk.csv") 
cleaned_temp_oxy_chla_file <-  paste0(folder, '/ARIMA_working/Catwalk_postQAQC.csv')
temp_oxy_chla_qaqc(data_file = temp_obs_fname_wdir, 
                   maintenance_file = paste0(folder, '/SCCData/mia-data/CAT_MaintenanceLog.txt'), 
                   output_file = cleaned_temp_oxy_chla_file)

full_time <- seq(as.Date('2018-08-15'), as.Date('2020-08-29'), by = 'day')

chla_obs <- extract_chla_chain_dailyavg(fname = paste0(folder, '/ARIMA_working/Catwalk_postQAQC.csv'),
                                        full_time = full_time,
                                        depths = 1.0,
                                        observed_depths_chla_fdom = 1,
                                        input_tz = "EST5EDT", 
                                        output_tz = "GMT")


obs_out <- matrix(NA, nrow = length(full_time), ncol = 2)
obs_out <- as.data.frame(obs_out)
obs_out[,1] <- seq(as.Date('2018-08-15'), as.Date('2020-08-29'), by = 'day')
obs_out[,2] <- chla_obs[[1]][,1]
colnames(obs_out) <- c('Date', 'chla_ugL')

write.csv(obs_out, paste0(folder, '/obs_chl_15Aug18_29Aug20.csv'), row.names = FALSE)
