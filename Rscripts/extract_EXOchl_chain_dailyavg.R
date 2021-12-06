
# script to extract EXO sonde chl data for the forecast time period and calculate daily averages


extract_chla_chain_dailyavg <- function(fname = catwalk_fname,full_time,depths = modeled_depths,observed_depths_chla_fdom= observed_depths_chla_fdom,input_tz = 'EST5EDT', output_tz = 'GMT'){
  d <- read.csv(fname)
  #d_names <- read.csv(fname, skip =1)
  #names(d) <- names(d_names)
  
  d$date <- lubridate::date(d$DateTime)
  Chla_obs <- array(NA,dim=c(length(full_time),2))
  
  
  for (i in 1:length(full_time)) {
    temp <- subset(d, d$date==lubridate::date(full_time)[i])
    temp1 <- temp %>% mutate(chl_avg = mean(EXOChla_ugL_1, na.rm = TRUE))
    Chla_obs[i,1] <- temp1$chl_avg[1]
    
       }
  return(list(Chla_obs = Chla_obs, depths = depths))
}



