create_inflow_outflow_file <- function(full_time_day,working_arima,input_tz = 'EST5EDT', output_tz = reference_tzone){
  
  full_time_day_2017 <- as.POSIXct(full_time_day,
                                   tz = reference_tzone) - 365*24*60*60
  full_time_day_2016 <- as.POSIXct(full_time_day,
                                   tz = reference_tzone) - 2*365*24*60*60
  full_time_day_2015 <- as.POSIXct(full_time_day, 
                                   tz = reference_tzone) - 3*365*24*60*60
  full_time_day_2014 <- as.POSIXct(full_time_day, 
                                   tz = reference_tzone) - 4*365*24*60*60
  full_time_day_2015 <- as.POSIXct(full_time_day, 
                                   tz = reference_tzone) - 5*365*24*60*60
  
  inflow = read.csv(paste0(working_arima,'/',
                           'FCR_weir_inflow_2013_2017_20180716.csv'))
  spillway = read.csv(paste0(working_arima,'/',
                             'FCR_spillway_outflow_2013_2017_20180716.csv'))
  
  inflow_time_local <- as.POSIXct(inflow$time, tz = reference_tzone)
  inflow$time <- with_tz(inflow_time_local,reference_tzone)
  
  spillway_time_local <- as.POSIXct(spillway$time, tz = reference_tzone)
  spillway$time <- with_tz(spillway_time_local,reference_tzone)
  
  inflow_2017 =  inflow[which(inflow$time %in% full_time_day_2017),]
  inflow_2016 =  inflow[which(inflow$time %in% full_time_day_2016),]
  inflow_2015 =  inflow[which(inflow$time %in% full_time_day_2015),]
  inflow_2014 =  inflow[which(inflow$time %in% full_time_day_2014),]
  inflow_2013 =  inflow[which(inflow$time %in% full_time_day_2015),]
  inflow_new = inflow_2014[,1:3]
  
  for(i in 1:length(full_time_day)){
    curr_day <- day(full_time_day[i])
    curr_month <- month(full_time_day[i])
    index1 <- which(day(inflow$time) == curr_day & month(inflow$time) == curr_month)
    index2 <- which(day(spillway$time) == curr_day & month(spillway$time) == curr_month)
    inflow_new[i,2] <- mean(inflow[index1,2])
    inflow_new[i,3] <- sd(inflow[index1,2])
  }

  inflow_new$time =  full_time_day
  names(inflow_new) = c("time", "FLOW", "SD")
  
  write.csv(inflow_new,
            file = paste0(working_arima,'/','FCR_inflow.csv'),
            row.names = FALSE,
            quote = FALSE)
  
}