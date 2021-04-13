# compare CTD and EXO time series with both in CTD units


# EXO data
folder <- "C:/Users/wwoel/Dropbox/Thesis"
data_location <-  "C:/Users/wwoel/Dropbox/Thesis/Data"
source(paste0('C:/Users/wwoel/Desktop/FLARE_AR_CHLA',"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) #this is the original file modified to take a daily avg rather than the midnight reading
source(paste0('C:/Users/wwoel/Desktop/FLARE_AR_CHLA',"/","Rscripts/temp_oxy_chla_qaqc.R")) 

temperature_location <- paste0(data_location, "/", "Catwalk")
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
chl_update <- chl_update %>% mutate(Chla_EXO_inCTD = Chla_EXO*0.55 - 0.0308) %>%  #convert into CTD units and then take the square root
  select(-Chla_EXO) 
chl_update$Date <- as.Date(chl_update$Date)


# read in CTD data
ctd <- read.csv('./Data/CTD/CTD_final_2013_2019.csv')
ctd$Date <- as.Date(ctd$Date)
ctd <- ctd[ctd$Reservoir=='FCR',]
ctd <- ctd[ctd$Date>'2018-08-14',]
ctd_1.6 <- ctd %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
ctd_1.6 <- ctd_1.6 %>% select(Date, Chla_ugL)

both <- left_join(ctd_1.6, chl_update)

plot(both$Date, both$Chla_ugL, type = 'l', col = 'red', ylim = c(0, 30))
points(both$Date, both$Chla_EXO_inCTD, type = 'l', col = 'blue')
legend('topleft', col = c('red', 'blue'), c('CTD', 'EXO in CTD units'), lty = c(1,1), bty = 'n')
