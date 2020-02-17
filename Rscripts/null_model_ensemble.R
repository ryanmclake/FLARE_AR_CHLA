
# code written into run_daily_arima_relhum to create a null ensemble model with process uncertainty if null_model = TRUE

if(null_model){
  
  
  # extract process error from bayes output and write to an array
  null_error <- array(NA, dim = c(nsteps, nmembers))
  
  for (i in 1:nsteps) {
    step_index <- 1
    
    for(j in 1:nmembers){
      
      p <- sample(seq(1,length(samples[[1]][,1])), 1, replace = TRUE) #changed 0 to 1 7/16
      null_error[i, j] <- samples[[1]][p,4]
      step_index = step_index + 1
      
      if(step_index > 16){
        step_index <- 1
      } 
    }
  }
  
  # take the obs chl on the forecast_start_day, the day the forecast is being made, and propagate that out for every timestep
  chla_obs[[1]][1]
  null_error <- null_error + chla_obs[[1]][1]
  
  # write the distribution of all 420 ensembles on each forecast day to a csv
  if(day(forecast_start_day) < 10){
    file_name_forecast_start_day <- paste0("0",day(forecast_start_day))
  }else{
    file_name_forecast_start_day <- day(forecast_start_day) 
  }
  
  if(month(forecast_start_day) < 10){
    file_name_forecast_start_month <- paste0("0",month(forecast_start_day))
  }else{
    file_name_forecast_start_month <- month(forecast_start_day) 
  }
  
  null_file_name <- paste0(year(forecast_start_day), "_", 
                           file_name_forecast_start_month, "_", 
                           file_name_forecast_start_day, "_", 
                           "null_ensembles.csv")
  
  write.csv(null_error, paste0(forecast_location, '/null_ensemble/', null_file_name), row.names = FALSE)
  
  # create a csv with summary stats of the null (mean and CI)
  null_summary <- array(NA, dim = c(16,3))
  for (i in 1:16) {
    # take the mean of the 420 ensembles
    temp <- mean(null_error[i,])
    null_summary[i,1] <- temp
    # calculate conf intervals and add to array
    error_upper <-  qnorm(0.975, mean = mean(null_error[i,]), sd =sd((null_error[i,])) )
    error_lower <- qnorm(0.025, mean =  mean(null_error[i,]), sd =sd((null_error[i,])) )
    null_summary[i,2] <- error_upper
    null_summary[i,3] <- error_lower
    
  }
  null_summary_file_name <- paste0(year(forecast_start_day), "_", 
                                   file_name_forecast_start_month, "_", 
                                   file_name_forecast_start_day, "_", 
                                   "null_summary.csv")
  write.csv(null_summary, paste0(forecast_location, '/null_ensemble/', null_summary_file_name), row.names = FALSE)
}

