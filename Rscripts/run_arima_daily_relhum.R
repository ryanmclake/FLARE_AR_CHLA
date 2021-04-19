# run_arima function to produce daily chl forecasts:

run_arima <- function(
  start_day= "2019-08-22 00:00:00",
  sim_name = NA, 
  hist_days = 1,
  forecast_days = 16,  
  spin_up_days = 0,
  restart_file = NA,
  folder, 
  forecast_location = NA,
  push_to_git = FALSE,
  pull_from_git = TRUE, 
  data_location = NA, 
  #nmembers = NA,
  n_ds_members = 50,
  uncert_mode = 1,
  reference_tzone,
  downscaling_coeff = NA,
  DOWNSCALE_MET = TRUE,
  FLAREversion,
  null_model = FALSE
){
  
  
  
  
  
  ################################################
  ### LOAD R FUNCTIONS
  #################################################
  
  source(paste0(folder,"/","Rscripts/create_obs_met_input.R"))
  source(paste0(folder,"/","Rscripts/process_GEFS2GLM.R"))
  source(paste0(folder,"/","Rscripts/create_inflow_outflow_file_arima.R"))
  source(paste0(folder,"/","Rscripts/archive_forecast.R"))
  source(paste0(folder,"/","Rscripts/met_downscale/process_downscale_GEFS.R")) 
  
  
  
  ###RUN OPTIONS
  pre_scc <- FALSE
  
  ### METEROLOGY DOWNSCALING OPTIONS
  downscaling_coeff = NA  
  if(is.na(downscaling_coeff)){
    FIT_PARAMETERS <- TRUE
  }else{
    FIT_PARAMETERS <- FALSE
  }
  
  if(DOWNSCALE_MET == FALSE){
    FIT_PARAMETERS <- FALSE
  }
  
  
  
  #Define modeled depths and depths with observations
  modeled_depths <- 1
  
  # obs depths temp and do are not relevant for ARIMA but am leaving here in case they are called later in the script, this should be cleaned out eventually 
  observed_depths_temp <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  observed_depths_do <- c(1, 5, 9)
  observed_depths_chla_fdom <- 1
  
  temp_obs_fname <- "Catwalk.csv"
  met_obs_fname <- "FCRmet.csv" # as of 27-Nov-19, need to change this to FCRmet_legacy01.csv if running dates before 01-01-2019
                                # file name online 43 of process_downscale_GEFS script needs to be changed as well
  
  inflow_file1 <- "FCR_weir_inflow_2013_2017_20180716.csv"
  outflow_file1 <- "FCR_spillway_outflow_2013_2017_20180716.csv" 
  forecast_start_day <- start_day
  
  
  #####################################
  ####### update driver data from git####
  #####################################
  
  # set file locations for different driver data 
  temperature_location <- paste0(data_location, "/", "mia-data")
  met_station_location <- paste0(data_location, "/", "carina-data")
  noaa_location <- paste0(data_location, "/", "noaa-data")
#  if(pull_from_git){
#    
#    if(!file.exists(temperature_location)){
#      setwd(data_location)
#      system("git clone -b mia-data --single-branch https://github.com/CareyLabVT/SCCData.git mia-data")
#    }
#    if(!file.exists(met_station_location)){
#      setwd(data_location)
#      system("git clone -b carina-data --single-branch https://github.com/CareyLabVT/SCCData.git carina-data")
#    }
#    if(!file.exists(noaa_location)){
#      setwd(data_location)
#      system("git clone -b noaa-data --single-branch https://github.com/CareyLabVT/SCCData.git noaa-data")
#    }
#  
    
    
#    setwd(temperature_location)
#    system(paste0("git pull"))
#    
#    setwd(met_station_location)
#    system(paste0("git pull"))
#    
#    setwd(noaa_location)
#    system(paste0("git pull"))
#  }
  
  #download.file('https://github.com/CareyLabVT/SCCData/raw/mia-data/Catwalk.csv','./SCCData/mia-data/Catwalk.csv')
  
   
  ###################################
  ##### uncertainty options #########
  #################################
  

  
 if( uncert_mode == 1){ # all sources of uncertainty are turned on aka normal run
    #All sources of uncertainty and data used to constrain 
    #SOURCES OF uncertainty
    process_uncertainty <- TRUE
    weather_uncertainty <- TRUE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- TRUE
    met_downscale_uncertainty <- TRUE
    driver_uncertainty_discharge <- TRUE
    }else if( uncert_mode == 2){ # isolate process uncertainty (ie, all other sources of uncertainty turned off)
    process_uncertainty <- TRUE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    driver_uncertainty_discharge <- FALSE
    }else if( uncert_mode == 3){ # isolate weather uncertainty (ie, all other sources of uncertainty turned off)
    process_uncertainty <- FALSE
    weather_uncertainty <- TRUE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    driver_uncertainty_discharge <- FALSE
    } else if(uncert_mode == 4) { # isolate initial condition uncertainty (ie, all other sources of uncertainty turned off)
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    driver_uncertainty_discharge <- FALSE
    } else if(uncert_mode == 5){ # isolate parameter uncertainty (ie, all other sources of uncertainty turned off)
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- TRUE
    met_downscale_uncertainty <- FALSE
    driver_uncertainty_discharge <- FALSE
    } else if(uncert_mode == 6){ # isolate discharge uncertainty (ie, all other sources of uncertainty turned off)
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
    driver_uncertainty_discharge <- TRUE
    }
    
    
  ####################################################
  #### STEP 3: CREATE TIME VECTORS
  ####################################################
  
  # The simulations are run from 00:00:00 GMT time 
  # so that they directly interface with the NOAA forecast
  # The output is converted back to local time before being saved
  
  begin_sim  <- as_datetime(start_day,tz = reference_tzone)
  total_days <- forecast_days #deleted hist_days 7/08/19
  end_sim <- begin_sim + total_days*24*60*60
  start_forecast_step <- hist_days
  forecast_start_time <- forecast_start_day#begin_sim + (start_forecast_step)*24*60*60
  
  
  if(day(forecast_start_time) < 10){
    forecast_day <- paste0("0", day(forecast_start_time))
  }else{
    forecast_day <- paste0(day(forecast_start_time))
  }
  if(month(forecast_start_time) < 10){
    forecast_month <- paste0("0", month(forecast_start_time))
  }else{
    forecast_month <- paste0(month(forecast_start_time))
  }
  full_time <- seq(begin_sim, end_sim, by = "1 day")
  full_time_local <- with_tz(full_time, tzone = local_tzone)
  full_time <- strftime(full_time, 
                        format="%Y-%m-%d %H:%M",
                        tz = reference_tzone)
  full_time_local <- strftime(full_time_local,
                              format="%Y-%m-%d %H:%M",
                              tz = local_tzone)
  full_time_day <- strftime(full_time,
                            format="%Y-%m-%d",
                            tz = reference_tzone)
  full_time_day_local <- strftime(full_time_local,
                                  format="%Y-%m-%d",
                                  tz = local_tzone)
  full_time_hour_obs <- seq(as.POSIXct(full_time[1],
                                       tz = reference_tzone), 
                            as.POSIXct(full_time[length(full_time)],
                                       tz = reference_tzone),
                            by = "1 hour")
  
  ############################################################
  #### Step 4: Set Array Length for final chl forecast#######
  ###########################################################
  nsteps <- 16
  
  n_met_members <- 21 # number of NOAA ensemble members
  # n_ds_members <- 50 # number of downscaling members, this is a function of downscaling the NOAA data to the FCR spatial scale
  nmembers <- n_met_members*20
  
  nstates <- 1 #only one state here, chlorophyll
  
  
  x <- array(NA, dim=c(nsteps, nmembers, nstates))
  
  
  #nsteps <- length(full_time)
  #ndepths_modeled <- length(modeled_depths)
  #num_wq_vars <- length(wq_names) 
  #glm_output_vars <- c("temp", wq_names)
  
  
  
  ####################################################
  #### STEP 5: ORGANIZE FILES
  ####################################################
  
  ###CREATE DIRECTORY PATHS AND STRUCTURE
  working_arima <- paste0(folder, "/", "ARIMA_working")  
  ####Clear out temp GLM working directory
  unlink(paste0(working_arima, "/*"), recursive = FALSE)    
  forecast_base_name <- paste0(year(forecast_start_day),
                               forecast_month,
                               forecast_day,
                               "gep_all_00z")
  temp_obs_fname_wdir <-  paste0(working_arima, "/", temp_obs_fname)
  met_obs_fname_wdir <-paste0(met_station_location, "/", met_obs_fname)
  met_forecast_base_file_name <- paste0("met_hourly_",
                                        forecast_base_name,
                                        "_ens")
  if(is.na(sim_name)){
    sim_name <- paste0(year(full_time_local[1]), "_",
                       month(full_time_local[1]), "_",
                       day(full_time_local[1]))
  }
  
  ############################################################
  ########### get NOAA ensemble data #########################
  ############################################################
  ###CREATE HISTORICAL MET FILE
  if(met_downscale_uncertainty == FALSE){
    n_ds_members <- 1
  }
  
  
  met_file_names <- rep(NA, 1+(n_met_members*n_ds_members))
  obs_met_outfile <- paste0(working_arima, "/", "GLM_met.csv")
  
  # this function takes the observed met data at FCR and calculates hourly summaries for a given date range 'full_time_hour_obs'
  create_obs_met_input(fname = met_obs_fname_wdir,
                       outfile=obs_met_outfile,
                       full_time_hour_obs, 
                       input_tz = "EST5EDT", 
                       output_tz = reference_tzone)
  met_file_names[1] <- obs_met_outfile
  
  ###CREATE FUTURE MET FILES
  if(forecast_days > 0){
    in_directory <- paste0(noaa_location)
    out_directory <- working_arima
    file_name <- forecast_base_name
    
    VarInfo <- data.frame("VarNames" = c("AirTemp",
                                         "WindSpeed",
                                         "RelHum",
                                         "ShortWave",
                                         "LongWave",
                                         "Rain"),
                          "VarType" = c("State",
                                        "State",
                                        "State",
                                        "Flux",
                                        "Flux",
                                        "Flux"),
                          "ds_res" = c("hour",
                                       "hour",
                                       "hour",
                                       "hour",
                                       "6hr",
                                       "6hr"),
                          "debias_method" = c("lm",
                                              "lm",
                                              "lm",
                                              "lm",
                                              "lm",
                                              "compare_totals"),
                          "use_covariance" = c(TRUE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               TRUE,
                                               FALSE),
                          stringsAsFactors = FALSE)
    
    replaceObsNames <- c("AirTC_Avg" = "AirTemp",
                         "WS_ms_Avg" = "WindSpeed",
                         "RH" = "RelHum",
                         "SR01Up_Avg" = "ShortWave",
                         "IR01UpCo_Avg" = "LongWave",
                         "Rain_mm_Tot" = "Rain")
    
    met_file_names[2:(1+(n_met_members*n_ds_members))] <- process_downscale_GEFS(folder,
                                                                                 noaa_location,
                                                                                 met_station_location,
                                                                                 working_arima,
                                                                                 sim_files_folder = paste0(folder, "/", "sim_files"),
                                                                                 n_ds_members,
                                                                                 n_met_members,
                                                                                 file_name,
                                                                                 output_tz = reference_tzone,
                                                                                 FIT_PARAMETERS,
                                                                                 DOWNSCALE_MET,
                                                                                 met_downscale_uncertainty,
                                                                                 compare_output_to_obs = FALSE,
                                                                                 VarInfo,
                                                                                 replaceObsNames,
                                                                                 downscaling_coeff,
                                                                                 full_time_local,
                                                                                 first_obs_date = met_ds_obs_start,
                                                                                 last_obs_date = met_ds_obs_end)
    
    if(weather_uncertainty == FALSE & met_downscale_uncertainty == TRUE){
      met_file_names <- met_file_names[1:(1+(1*n_ds_members))]
    }else if(weather_uncertainty == FALSE & met_downscale_uncertainty == FALSE){
      met_file_names <- met_file_names[1:2]
    }
  }
  
  if(weather_uncertainty == FALSE){
    n_met_members <- 1
  }
  
  
  #go through met_file_names and take daily averages of SW and a sum of rain, which is needed for the discharge forecast
  # then select the days needed for the forecast
  
  data<-matrix(data=NA,16,(length(met_file_names)-1))
  rain <- matrix(data = NA, 16, (length(met_file_names)-1))
  
  for(j in 2:length(met_file_names)){
    temp<-read.csv(met_file_names[j])
    temp$date <- date(temp$time)
    for(i in 1:length(unique(temp$date))){
      temp1<-subset(temp, temp$date==unique(temp$date)[i])
      temp2 <- temp1 %>% mutate(RelHum_mean = mean(temp1$RelHum))  %>% 
        mutate(rain_sum = sum(temp1$Rain))
      data[i,j-1]=temp2[1,10]
      rain[i, j-1] = temp2[1,11]
    }
  }
  
  # rename with all ensemble members 
  rhum_forecast <- data[,]  
  

  ####################################################
  #### STEP 2: DETECT PLATFORM  
  ####################################################
  
  switch(Sys.info() [["sysname"]],
         Linux = { machine <- "unix" },
         Darwin = { machine <- "mac" },
         Windows = { machine <- "windows"})
  
  ###INSTALL PREREQUISITES##
  
  #INSTALL libnetcdf
  if(machine == "unix") {
    system("if [ $(dpkg-query -W -f='${Status}' libnetcdf-dev 2>/dev/null | grep -c 'ok installed') -eq 0 ]; then sudo apt update && sudo apt install libnetcdf-dev; fi;")
    Sys.setenv(LD_LIBRARY_PATH=paste("../glm/unix/", Sys.getenv("LD_LIBRARY_PATH"),sep=":"))
  }
  
  # move files from the sim folder into the working arima folder
  sim_files_folder <- paste0(folder, "/", "sim_files")
  fl <- c(list.files(sim_files_folder, full.names = TRUE))
  tmp <- file.copy(from = fl, to = working_arima, overwrite = TRUE)
  tmp <- file.copy(from = fl, to = working_arima, overwrite = TRUE)
  if(!is.na(restart_file)){
    tmp <- file.copy(from = restart_file, to = working_arima, overwrite = TRUE)
  }

  
  ############################################
  ##### chla data extraction and download  ###
  ############################################
  
  source(paste0(folder,"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) 
  #this is the original file modified to take a daily avg rather than the midnight reading
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
  chla_obs <- extract_chla_chain_dailyavg(fname = new_temp_obs_fname_wdir,
                                          full_time,
                                          depths = 1.0,
                                          observed_depths_chla_fdom = observed_depths_chla_fdom,
                                          input_tz = "EST5EDT", 
                                          output_tz = reference_tzone)
  


#########################################################################################################################
###### data assimilation  ###############################################################################################
#########################################################################################################################

# a script to check for new data to add to the historical dataset; data gets updated weekly
train_data <- 'data_arima_highfrequency_RelHum_mean.csv'
outfile <- 'data_arima_working_highfrequency.csv'

    source(paste0(folder,"/","Rscripts/data_assimilation_AR.R"))
  data_assimilation(folder = folder, 
                    data_location =  data_location,
                    hist_file = train_data,
                    forecast_start_day = forecast_start_day,
                    timestep = 1, #a numeric value for this script (so either '1' or '7')
                    outfile = outfile,
                    met_obs_fname = met_obs_fname)
  
  
  # read in the jags file to pull from parameter values 
  # load("C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3/MCMC_output_ARIMA_Whitney.Rdata")
  # find a way to update the jags file without entering the whole code right here?
  library(rjags)
  library(PerformanceAnalytics)
  
  setwd(folder)
  data = read.csv(outfile)
  
  N <- nrow(data)
  
  sink("jags_model.bug")
  cat('model {
  for (i in 1:N) {
    chla[i] ~ dnorm(chla.hat[i], tau)
    chla.hat[i] <- beta[1] + beta[2]*chla_lag[i] + beta[3]*rh[i] 
  }
  
  #Vague priors on the beta
  for(j in 1:3){
    beta[j] ~ dnorm(0,1/100000)
  }

  # Prior for the inverse variance
  sigma ~ dunif(0, 100) # standard deviation
	tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
}'
  )
  sink()
  
  jags <- jags.model('jags_model.bug',  # does this file get created in this function or need to exist already? should have diff one for weekly and daily?
                     data = list('chla' = data$Chla_sqrt,
                                 'chla_lag' = data$Chla_ARlag1_sqrt,
                                 'rh' = data$RelHum_mean,
                                 'N' = N),
                     n.chains = 4, # why 4? what does this number mean?
                     n.adapt = 100) # what does this number mean?
  
  #burn in, this updates the jags$state()
  update(jags,n.iter = 1000)
  
  #sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
  samples = coda.samples(model = jags,
                         variable.names = c('beta','sigma'),
                         n.iter = 10000)
  
  par_matrix <- as.matrix(samples[1])
  
  save(samples, file = "MCMC_output_ARIMA_highfrequency_RelHum.Rdata")
  
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
  
  
  
  # the first column is observed chl that is sqrt transformed (because the model is based on sqrt units) and corrected into CTD units (because it is observed in EXO units)
  if(initial_condition_uncertainty == TRUE& !is.na(chla_obs[[1]][1,1]) ){
    for(i in 1:nmembers){
      x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5) # sample from a normal distribution around the mean obs chl value
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again # doing this a bunch of times in case a negative comes up a second time, not sure how to quantifiably figure out what the right number of times is here
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      if(x[1,i,] < 0 ){x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5)} # if a negative value is chosen, sample again
      
    }
    
    x[1,,] <-   sqrt(x[1,,]) # take the sqrt to return to model units
  }else{
    x[1,1:nmembers,] <- sqrt(chla_obs[[1]][1,1]*0.55 - 0.0308) 
  }
  
  
  
  npars <- 4
  ensemble_pars <- array(NA, dim = c(nmembers, npars)) 
  
  # for loop to sample from distribution of each parameter value (propagating uncertainty)
  for(j in 1:nmembers){
    if(parameter_uncertainty == TRUE){
      p <- sample(seq(1,length(samples[[1]][,1])), 1, replace = TRUE) #changed 0 to 1 7/16
      ensemble_pars[j, 1] <- samples[[1]][p,1]
      ensemble_pars[j, 2] <- samples[[1]][p,2]
      ensemble_pars[j, 3] <- samples[[1]][p,3]
      ensemble_pars[j, 4] <- samples[[1]][p,4]
    }else{
      ensemble_pars[j, 1] <- mean(samples[[1]][,1])
      ensemble_pars[j, 2] <- mean(samples[[1]][,2])
      ensemble_pars[j, 3] <- mean(samples[[1]][,3])
      ensemble_pars[j, 4] <- mean(samples[[1]][,4])
    }
  }  
  
  # the model!
  for(i in 2:16){ # for each time step
    met_index <- 1
    for(j in 1:nmembers){   
      if(process_uncertainty == TRUE){
        added_process_uncertainty =  rnorm(1, 0,ensemble_pars[j, 4])
      }else{
        added_process_uncertainty = 0.0
      }
     # if(driver_uncertainty_discharge == TRUE){
    #    curr_discharge = rnorm(1, discharge_forecast[i,2], discharge_forecast[i,3])
    #  }else{
    #    curr_discharge = discharge_forecast[i,2]
    #  }
      if(weather_uncertainty == TRUE){
        curr_relhum = rhum_forecast[i,met_index]
      }else{
        curr_relhum = rhum_forecast[i]
      }
      x[i,j,] <- ensemble_pars[j, 1] + ensemble_pars[j, 2]*x[i-1,j,] +  ensemble_pars[j, 3]*curr_relhum + added_process_uncertainty
      met_index = met_index + 1
      if(met_index > n_met_members){
        met_index <- 1
      }
    }
  }
  
  # write 'x' to csv, the forecast array where nrow = 16 for each day of the forecast and ncol = 420 for each forecast ensemble
  # some rearranging to create the name of the file
 if(uncert_mode==1){
  
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
  
  forecast_ensemble_file_name <- paste0(year(forecast_start_day), "_", 
                               file_name_forecast_start_month, "_", 
                               file_name_forecast_start_day, "_", 
                               "chla_daily_ensembles.csv")
  write.csv(x, paste0(forecast_location, '/', forecast_ensemble_file_name), row.names = FALSE)
 }
  
#################################################################################################################################################################
############ calculate summary statistics and save output ###################################################################################################
#################################################################################################################################################################
  
  # create the output dataframe for archiving the forecast statistics (mean, sd, 95% CI, and obs chl)
  out <- data.frame("forecast_date" = as.Date(full_time[2:17]), 
                    "forecast_mean_chl" =rep(NA) , 
                    "forecast_median_chl" = rep(NA),
                    "forecast_sd_chl" = rep(NA), 
                    "forecast_CI95_upper" = rep(NA),
                    "forecast_CI95_lower" = rep(NA), 
                    "forecast_max" = rep(NA), 
                    "forecast_min" = rep(NA), 
                    "forecast_variance" = rep(NA),
                    "obs_chl_EXO" = rep(NA), 
                    "forecast_run_day" = start_day, 
                    "day_in_future" = rep(NA),
                    'par1' = rep(NA),
                    'par2' = rep(NA),
                    'par3'= rep(NA),
                    'par4'= rep(NA),
                    'par5' = rep(NA)
                    )
  

 
  

  for (i in 1:16) {
    error_upper <-  qnorm(0.975, mean = mean(x[i,,]), sd =sd((x[i,,])) )
    error_upper <- (error_upper^2)/0.55 + 0.0308
    error_lower <- qnorm(0.025, mean = mean(x[i,,]), sd =sd((x[i,,])) )
    error_lower <- (error_lower^2)/0.55 + 0.0308
    
    out[i, 2] <- mean(    ((x[i,,]^2)/0.55) + 0.0308)
    out[i, 3] <- median(    ((x[i,,]^2)/0.55) + 0.0308)
    out[i, 4] <- sd(    ((x[i,,]^2)/0.55) + 0.0308)
    out[i, 5] <- error_upper 
    out[i, 6] <- error_lower
    out[i, 7] <- max(  ((x[i,,]^2)/0.55) + 0.0308)
    out[i, 8] <- min(  ((x[i,,]^2)/0.55) + 0.0308)
    out[i, 9] <- var(x[i,,])
    out[i, 10] <- chla_obs[[1]][i+1,1] # +1 bc the first day is the observed chl
  }
  

  
  out[,12] <- seq(1,16, by = 1)
  out[,13] <- mean(ensemble_pars[,1])
  out[,14]<- mean(ensemble_pars[,2])
  out[,15]<- mean(ensemble_pars[,3])
  out[,16]<- mean(ensemble_pars[,4])
  
  
  
  
  setwd(folder)
 
  # create a single csv for each forecast that is named with day the forecast is run
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
  
  forecast_file_name <- paste0(year(forecast_start_day), "_", 
                               file_name_forecast_start_month, "_", 
                               file_name_forecast_start_day, "_", 
                               "chla_daily.csv")
  #some conditional statements to archive the forecast in a different location if doing an uncertainty analysis
  
  if(uncert_mode==1){
  forecast_output_location <- paste0(forecast_location,  "/",
                                     
                                     forecast_file_name)
  }else if(uncert_mode==2){
    forecast_output_location <- paste0(forecast_location,  "/",
                                       "uncertainty_2_process/",
                                       forecast_file_name)
  }else if(uncert_mode==3){
    forecast_output_location <- paste0(forecast_location,  "/",
                                       "uncertainty_3_weather/",
                                       forecast_file_name)
  }else if(uncert_mode==4){ forecast_output_location <- paste0(forecast_location,  "/",
                                                               "uncertainty_4_initial_condition/",
                                                               forecast_file_name)
  }else if(uncert_mode==5){ forecast_output_location <- paste0(forecast_location,  "/",
                                                               "uncertainty_5_parameter/",
                                                               forecast_file_name)
  }else if(uncert_mode==6){ forecast_output_location <- paste0(forecast_location,  "/",
                                                               "uncertainty_6_discharge/",
                                                               forecast_file_name)
  }
  
  
  write.csv(out, forecast_output_location, row.names = FALSE)
  
  # now archive the parameter values for the forecasts
  parms_file_name <- paste0(year(forecast_start_day), "_", 
                            file_name_forecast_start_month, "_", 
                            file_name_forecast_start_day, "_",
                            'ensemble_parameters.csv')
  if(uncert_mode==1){
    parms_output_location <- paste0(forecast_location,  "/",
                                       parms_file_name)
  }else if(uncert_mode==2){
    parms_output_location <- paste0(forecast_location,  "/",
                                       "uncertainty_2_process/",
                                       parms_file_name)
  }else if(uncert_mode==3){
    parms_output_location <- paste0(forecast_location,  "/",
                                       "uncertainty_3_weather/",
                                       parms_file_name)
  }else if(uncert_mode==4){ parms_output_location <- paste0(forecast_location,  "/",
                                                               "uncertainty_4_initial_condition/",
                                                               parms_file_name)
  }else if(uncert_mode==5){ parms_output_location <- paste0(forecast_location,  "/",
                                                               "uncertainty_5_parameter/",
                                                               parms_file_name)
  }else if(uncert_mode==6){ parms_output_location <- paste0(forecast_location,  "/",
                                                               "uncertainty_6_discharge/",
                                                               parms_file_name)
  }
  
  
    
    write.csv(ensemble_pars, parms_output_location, row.names = FALSE)
  
  # and make a simple plot to archive the ensemble spread
  if(uncert_mode==1){
  forecast_plot_name <-  paste0(year(forecast_start_day), "_", 
                                file_name_forecast_start_month, "_", 
                                file_name_forecast_start_day, "_", 
                                "chla_daily.pdf")
  forecast_plot_output_location <- paste0(forecast_location,  "/",
                                          "ensemble_plots/",
                                          forecast_plot_name) 
  
  
  
  if(!is.na(x[1,1,])){
    pdf(file = forecast_plot_output_location )
    x_axis <-   as.Date(full_time[2:17])
    plot(x_axis, ((x[,1,]^2)/0.55 +0.0308), type = 'o',ylim = range(c((min(out[,8], out[,8], na.rm = TRUE)), max(out[,7], out[,10], na.rm = TRUE))) # once catwalk data cleaning script is running, can change this to include: out[1,10], out[2,10]
         , xlab = "Date", ylab = "Chla (ug/L)")
    for(m in 2:length(x[1,,1])){
      points(x_axis, ((x[,m,]^2)/0.55 + 0.0308), type = 'o')  
    }
    for(j in 1:nrow(out)){
      points(out[j,1], (out[j,2]), col = 'orange', pch = 16, cex = 2)  
    }
    for(j in 1:nrow(out)){
      points(out[j,1], (out[j,10]), col = 'red', pch = 16, cex = 2)  
    }
    
    
    legend('topleft', c('observed chla', 'forecast ensembles', 'forecast mean'), col = c('red', 'black', 'orange'), pch = c(16, 1,16), lty = c(0,1,0), bty = 'n')
    dev.off()}
    
  }
   }

  