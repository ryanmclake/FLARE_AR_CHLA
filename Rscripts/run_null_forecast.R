
run_null <- function(
  folder,
  forecast_location,
  forecast_start_day,
  nmembers,
  timestep,
  timestep_numeric,
  timestep_interval,
  max_timestep
){
  
  
  library(tidyverse)
  library(lubridate)
  library(rjags)
  
  obs_all <- read.csv(paste0(folder, '/obs_chl_15Aug18_29Aug20.csv'))
  
  colnames(obs_all) <- c('date', 'chla_ugL')
  obs_all$date <- as.Date(obs_all$date)
  obs_all <- obs_all %>% mutate(chla_sqrt = sqrt(chla_ugL))
  obs <- obs_all[obs_all$date<forecast_start_day,]
  
  if(timestep=='1day'){  
    full_time <- seq(min(obs$date), as.Date(forecast_start_day)+ days(max_horizon), by = timestep_numeric) 
    forecast_time <- full_time[full_time>forecast_start_day]
  }else{
    
    past_time <- seq(min(obs$date), as.Date(forecast_start_day), by = timestep_numeric) 
    add_time <- seq(as.Date(forecast_start_day),  as.Date(forecast_start_day) + days(max_horizon), by = timestep_numeric) 
    full_time <- c(past_time, add_time)
    obs <- obs[obs$date %in% past_time,]
    forecast_time <- add_time[add_time>forecast_start_day]
    
}
  
  #Full time series with gaps
  y <- c(obs$chla_sqrt)
  time <- c(obs$date)
  #Indexes of full time series with gaps
  y_index <- 1:length(y)
  #Remove gaps
  y_gaps <- y[!is.na(y)]
  #keep indexes to reference the gappy time series
  y_index <- y_index[!is.na(y)]
  
  N <- length(full_time)
  
  randomwalk = '
model {
  
## Priors
  sigma ~ dunif(0.001, 100) # prior for standard deviation of sigma, process error
	tau_process <- 1 / (sigma * sigma) # bayes uses precision, 1/sd^2
	tau_obs <- 1/(sd_obs*sd_obs) # precision for obs error, 1/sd of observation error ^2
  latent.chl[1] ~ dnorm(x_ic, tau_obs) #latent.chl is latent state 
  chla[1] ~ dnorm(latent.chl[1], tau_obs) 

## observation error and process model
  for (i in 2:N) {
    latent.chl[i] ~ dnorm(latent.chl[i-1], tau_process) 
        chla[i] ~ dnorm(latent.chl[i], tau_obs) # chla vector includes process (and parameter, tau_process) error + observation error
  }
  
  
## Data Model
  for(t in 1:nobs){
    y[t] ~ dnorm(latent.chl[y_index[t]],tau_obs)
  }

}'
  
  j.model <- jags.model(file = textConnection(randomwalk),
                        data = list('y' = y_gaps,
                                    'y_index' = y_index,
                                    'nobs' = length(y_index),
                                    'sd_obs' = 0.5,
                                    'x_ic' = obs$chla_sqrt[1],
                                    'N' = N),
                        n.chains = 4,
                        n.adapt = 100)
  
  #burn in, this updates the jags$state()
  update(j.model,n.iter = 1000)
  
  #sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
  samples = coda.samples(model = j.model,
                         variable.names = c( 'chla', 'sigma'),
                         n.iter = 10000)
  
  #### diagnostics
  # gelman.diag(samples)
  # library(LaplacesDemon)
  # ESS(samples)
  
  # output dataframe
  null_out <- array(NA, dim = c(N, 7))
  null_out <- as.data.frame(null_out)
  colnames(null_out) <- c('mean', 'upper_CI', 'lower_CI', 'date', 'forecast_run_day', 'day_in_future', 'timestep')
  
  
  for (i in 1:length(full_time) ){
    p <- sample(seq(1,length(samples[[1]][,i])), 420, replace = TRUE) # pick 420 random numbers from the first chain, i'th column
    temp <- samples[[1]][p,i]
    null_out[i,1] <-  mean(temp)^2
    null_out[i,2] <-  qnorm(0.975, mean = mean(temp), sd =sd(temp) )^2 #upper CI
    null_out[i,3] <- qnorm(0.025, mean = mean(temp), sd =sd(temp) )^2 #lower CI
  }
  
  null_out$date <- full_time 
  null_out <- left_join(null_out, obs_all)
  null_out <- null_out %>% mutate(residual = chla_ugL-mean)
  
  # save only the forecasted data at end of time series
  null_save <- null_out[null_out$date %in% forecast_time,]
  
  null_save$forecast_run_day <- forecast_start_day
  if(timestep=='1day'){
    null_save$day_in_future <- seq(1,max_horizon)
    null_save$timestep <- seq(1,max_horizon)
  }else if(timestep=='7day'){
    null_save$day_in_future <- c(7,14)
    null_save$timestep <- c(1,2)
  }else if(timestep=='14day'){
    null_save$day_in_future <- 14
    null_save$timestep <- 1
    }
  
  # save the file
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
  
  
  
  null_summary_file_name <- paste0(year(forecast_start_day), "_", 
                                   file_name_forecast_start_month, "_", 
                                   file_name_forecast_start_day, "_", 
                                   "null_summary.csv")
  write.csv(null_save, paste0(forecast_location, '/',  null_summary_file_name), row.names = FALSE)
  

}

