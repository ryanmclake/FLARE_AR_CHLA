# run_arima function to produce chl forecasts using the following arima model:
# Chla_CTD_t  = 1.65(±0.26) + 0.45(±0.08)Chla_CTD_t_1  – 3.05(±1.39)Discharge_t – 0.0025(±)Shortwave_t  + Ɛ 

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
  window_length,
  null_model = FALSE,
  data_assimilation = TRUE
){
  
  
  
  
  
  ################################################
  ### LOAD R FUNCTIONS
  #################################################
  
  source(paste0(folder,"/","Rscripts/create_obs_met_input.R"))
  source(paste0(folder,"/","Rscripts/process_GEFS2GLM.R"))
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
  met_obs_fname <- "FCRmet_legacy01.csv" # as of 27-Nov-19, need to change this to FCRmet_lecagy01.csv if running dates before 01-01-2019
                                # file name in process_downscale_GEFS script needs to be changed as well
  forecast_start_day <- start_day
  
  
  #####################################
  ####### update driver data from git####
  #####################################
  
  # set file locations for different driver data 
  temperature_location <- paste0(data_location, "/", "mia-data")
  met_station_location <- paste0(data_location, "/", "carina-data")
  noaa_location <- paste0(data_location, "/", "noaa-data")
  diana_data_location <- paste0(data_location, "/", "diana-data")
  manual_data_location <- paste0(data_location, "/", "manual-data") 
  
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
#     if(!file.exists(diana_data_location)){
#     setwd(data_location)
#     system("git clone -b diana-data --single-branch https://github.com/CareyLabVT/SCCData.git diana-data")
#   }
#    if(!file.exists(manual_data_location)){
#    setwd(data_location)
#    system("git clone -b manual-data --single-branch https://github.com/CareyLabVT/SCCData.git manual-data")
#  }
#    
#    setwd(temperature_location)
#    system(paste0("git pull"))
#    
#    setwd(met_station_location)
#    system(paste0("git pull"))
#    
#    setwd(noaa_location)
#    system(paste0("git pull"))
#  
#     setwd(diana_data_location)
#     system(paste0("git pull"))

#    setwd(manual_data_location)
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
  nsteps <- 2 # number of timesteps: initial, day 14
  
  n_met_members <- 21 # number of NOAA ensemble members
  # n_ds_members <- 50 # number of downscaling members, this is a function of downscaling the NOAA data to the FCR spatial scale
  nmembers <- n_met_members*20
  
  nstates <- 1 #only one state here, chlorophyll
  
  
  x <- array(NA, dim=c(nsteps, nmembers, nstates))
  
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
  
  
  # build a loop to go through met_file_names and take daily averages of SW and a sum of rain, which is needed for the discharge forecast
  # then select day 7 and 14
  
  data<-matrix(data=NA,16,(length(met_file_names)-1))
  rain <- matrix(data = NA, 16, (length(met_file_names)-1))
  
  for(j in 2:length(met_file_names)){
    temp<-read.csv(met_file_names[j])
    temp$date <- date(temp$time)
   # for(i in 2:(length(unique(temp$date))-1)){  # use this line for the Dec 2018 and Nov 2019 forecasts which are in GMT
      
   for(i in 1:(length(unique(temp$date)))){
      temp1<-subset(temp, temp$date==unique(temp$date)[i])
      temp2 <- temp1 %>% mutate(SW = mean(temp1$ShortWave))  %>% 
        mutate(rain_sum = sum(temp1$Rain))
      data[i,j-1]=temp2[1,10] 
      rain[i, j-1] = temp2[1,11]
      
    }
  }
  
  # extract all ensemble members for forecasts on day 14, which is the 15th entry because we are pulling from the forecast made at midnight on the present day
  sw_forecast <- data[c(1,15),]  
  

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

  
  ###############################################
  ####CREATE INFLOW AND OUTFILE FILES############
  ###############################################
  
  # right now using the average inflow on given day from historical data
  # soon will build in diana discharge data and use autoregressive model
  # source create inflwo outflow file that takes the average of the past 5 years of data
  
  source(paste0(folder,"/","Rscripts/create_inflow_outflow_file_forecastdischarge.R"))
  source(paste0(folder,"/","Rscripts/inflow_qaqc.R"))
  
  
  cleaned_inflow_file <- paste0(working_arima, "/FCRinflow_postQAQC.csv")
  inflow_file1 <- c(paste0(data_location,"/diana-data/FCRweir.csv"),
                     paste0(folder,"/sim_files/FCR_inflow_WVWA_2013_2019.csv"),
                     paste0(data_location,"/manual-data/inflow_working_2019.csv"))
  
  inflow_qaqc(fname = inflow_file1,
              cleaned_inflow_file ,
              local_tzone, 
              input_file_tz = 'EST',
              working_arima)
  
#  create_inflow_outflow_file(folder = folder,
#                             full_time_day = full_time_day,
#                             working_arima = working_arima, 
#                             input_tz = "EST5EDT",
#                             output_tz = reference_tzone)
  
  
  start_forecast_step <- hist_days + 1
  Discharge <- create_inflow_outflow_file(full_time_local = full_time_day_local,
                             working_directory = noaa_location, #?
                             input_file_tz = 'EST5EDT', 
                             start_forecast_step = start_forecast_step,
                             inflow_file1 = cleaned_inflow_file,
                             local_tzone = local_tzone,
                             met_file_names = met_file_names,
                             forecast_days = forecast_days,
                             inflow_process_uncertainty = driver_uncertainty_discharge)
  
  # identify the 7th and 14th days of the forecast
  Discharge <- as.data.frame(Discharge)
  colnames(Discharge) <- c('time', 'FLOW')
 
  week2 <- as.POSIXct(start_day,
                      tz = reference_tzone) + 14*24*60*60
  
  discharge_forecast <- Discharge[date(Discharge$time)==date(week2) | date(Discharge$time)==date(forecast_start_day),]
  
  
  ############################################
  ##### chla data extraction and download  ###
  ############################################
  
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
  chla_obs <- extract_chla_chain_dailyavg(fname = new_temp_obs_fname_wdir,
                                          full_time,
                                          depths = 1.0,
                                          observed_depths_chla_fdom = observed_depths_chla_fdom,
                                          input_tz = "EST5EDT", 
                                          output_tz = reference_tzone)
  
  
  
  # chla_obs is the matrix to pull from for day 1, which will be used as the AR term for day 7 and day 14
  # also pull the observed chla on day 7 and 14 if available to use for validation
  # this is done below

#########################################################################################################################
###### data assimilation  ###############################################################################################
#########################################################################################################################

  if(data_assimilation){
  # read in file with all data and subset to the forecast day
  data <- read.csv('data_arima_fortnightly_through_2019.csv')  
  data$Date <- as.Date(data$Date)
  data <- data[data$Date<forecast_start_day,]
  print('data subsetted to forecast start day')
  
  }else{
    
    data <- read.csv('data_arima_fortnightly_2013_2016.csv')
    print('training data only')
  }
  library(rjags)
  library(PerformanceAnalytics)
  
  setwd(folder)
  
  N <- nrow(data)
  
  sink("jags_model.bug")
  cat('model {
  for (i in 1:N) {
    chla[i] ~ dnorm(chla.hat[i], tau)
    chla.hat[i] <- beta[1] + beta[2]*chla_lag[i] + beta[3]*discharge[i] + beta[4]*sw[i]
  }
  
  #Vague priors on the beta
  for(j in 1:4){
    beta[j] ~ dnorm(0,1/100000)
  }

  # Prior for the inverse variance
  sigma ~ dunif(0, 100) # standard deviation
	tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
}'
  )
  sink()
  
  jags <- jags.model('jags_model.bug',
                     data = list('chla' = data$Chla_sqrt,
                                 'chla_lag' = data$Chla_ARlag1_sqrt,
                                 'discharge' = data$mean_flow,
                                 'sw' = data$ShortWave_mean,
                                 'N' = N),
                     n.chains = 4,
                     n.adapt = 100)
  
  #burn in, this updates the jags$state()
  update(jags,n.iter = 1000)
  
  #sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
  samples = coda.samples(model = jags,
                         variable.names = c('beta','sigma'),
                         n.iter = 10000)
  
  par_matrix <- as.matrix(samples[1])
  
  #chart.Correlation(par_matrix)
  
  save(samples, file = "MCMC_output_ARIMA_Whitney.Rdata")
  
  if(null_model){
    
    
    # extract process error from bayes output and write to an array
    null_error <- array(NA, dim = c(nsteps, nmembers))
    
    for (i in 1:nsteps) {
      step_index <- 1
      
      for(j in 1:nmembers){
        
        p <- sample(seq(1,length(samples[[1]][,1])), 1, replace = TRUE) 
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
    
    # create a csv with summary stats of the null (mean and CI)
    null_summary <- array(NA, dim = c(nsteps,4)) # 4 = mean, upper and lower conf interval, and day of forecast
    for (i in 1:nsteps) {
      # take the mean of the 420 ensembles
      temp <- mean(null_error[i,], na.rm = TRUE)
      null_summary[i,1] <- temp
      # calculate conf intervals and add to array
      error_upper <-  qnorm(0.975, mean = mean(null_error[i,]), sd =sd((null_error[i,])) )
      error_lower <- qnorm(0.025, mean =  mean(null_error[i,]), sd =sd((null_error[i,])) )
      null_summary[i,2] <- error_upper
      null_summary[i,3] <- error_lower
      
    }
    
    
    null_error <- as.data.frame(null_error)
    null_error[,421] <- c(forecast_start_day, week2)
    

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
    
    null_summary <- as.data.frame(null_summary)
    null_summary[,4] <- c(forecast_start_day, week2)
    colnames(null_summary) <- c('mean', 'upper_CI', 'lower_CI', 'date')
    
    null_summary_file_name <- paste0(year(forecast_start_day), "_", 
                                     file_name_forecast_start_month, "_", 
                                     file_name_forecast_start_day, "_", 
                                     "null_summary.csv")
    write.csv(null_summary, paste0(forecast_location, '/null_ensemble/', null_summary_file_name), row.names = FALSE)
  }
  
  
  
  
  # the first column is observed chl that is sqrt transformed (because the model is based on sqrt units) and corrected into CTD units (because it is observed in EXO units)
  if(initial_condition_uncertainty == TRUE& !is.na(chla_obs[[1]][1,1]) ){
    for(i in 1:nmembers){
      x[1,i,] <- rnorm(1, chla_obs[[1]][1,1]*0.55 - 0.0308, 0.5) # sample from a normal distribution around the mean obs chl value in CTD units, 0.5 is the mean residual btw comparison of CTD and EXO
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
  
  
  
  npars <- 5
  ensemble_pars <- array(NA, dim = c(nmembers, npars)) 
  
  # for loop to sample from distribution of each parameter value
  for(j in 1:nmembers){
    if(parameter_uncertainty == TRUE){
      p <- sample(seq(1,length(samples[[1]][,1])), 1, replace = TRUE) #changed 0 to 1 7/16
      ensemble_pars[j, 1] <- samples[[1]][p,1]
      ensemble_pars[j, 2] <- samples[[1]][p,2]
      ensemble_pars[j, 3] <- samples[[1]][p,3]
      ensemble_pars[j, 4] <- samples[[1]][p,4]
      ensemble_pars[j, 5] <- samples[[1]][p,5]
    }else{
      ensemble_pars[j, 1] <- mean(samples[[1]][,1])
      ensemble_pars[j, 2] <- mean(samples[[1]][,2])
      ensemble_pars[j, 3] <- mean(samples[[1]][,3])
      ensemble_pars[j, 4] <- mean(samples[[1]][,4])
      ensemble_pars[j, 5] <- mean(samples[[1]][,5])     
    }
  }  
  
  # the model!
    i <- 2
    met_index <- 1
    for(j in 1:nmembers){  #want this to be length(nmembers) = 
      if(process_uncertainty == TRUE){
        added_process_uncertainty =  rnorm(1, 0,ensemble_pars[j, 5])
      }else{
        added_process_uncertainty = 0.0
      }
      if(driver_uncertainty_discharge == TRUE){
        curr_discharge = rnorm(1, discharge_forecast[i,2], 0.00965) #sd from QT's discharge forecasts
      }else{
        curr_discharge = discharge_forecast[i,2]
      }
      if(weather_uncertainty == TRUE){
        curr_shortwave = sw_forecast[i,met_index]
      }else{
        curr_shortwave = sw_forecast[i]
      }
      x[i,j,] <- ensemble_pars[j, 1] + ensemble_pars[j, 2]*x[i-1,j,] + ensemble_pars[j, 3]*curr_discharge + ensemble_pars[j, 4]*curr_shortwave + added_process_uncertainty
      met_index = met_index + 1
      if(met_index > n_met_members){
        met_index <- 1
      }
    }
  
  
  
  forecast_ensemble_file_name <- paste0(year(forecast_start_day), "_", 
                                        file_name_forecast_start_month, "_", 
                                        file_name_forecast_start_day, "_", 
                                        "chla_fortnightly_ensembles.csv")
  
# conf intervals for 14 day forecast
  error1_upper <- qnorm(0.975, mean = mean(x[2,,]), sd =sd((x[2,,])) )
  error1_upper <- (error1_upper^2)/0.55 + 0.0308
  error1_lower <- qnorm(0.025, mean = mean(x[2,,]), sd =sd((x[2,,])) )
  error1_lower <- (error1_lower^2)/0.55 + 0.0308
  
  
  # create the output dataframe for archiving the forecast statistics (mean, sd, 95% CI, and obs chl)
  out <- data.frame("forecast_date" = c(week2), 
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
                    "week" = rep(NA),
                    'par1' = rep(NA),
                    'par2' = rep(NA),
                    'par3'= rep(NA),
                    'par4'= rep(NA),
                    'par5' = rep(NA)
                    )
  out[1,2] <- mean(    ((x[2,,]^2)/0.55) + 0.0308) # mean of the forecasts on day 7 un-sqrt-transformed and in EXO units
  #out[2,2] <- mean(    ((x[3,,]^2)/0.55) + 0.0308) # mean of the forecasts on day 14 untransformed and in EXO units
  out[1,3] <- median(  ((x[2,,]^2)/0.55) + 0.0308)
  #out[2,3] <- median( ((x[3,,]^2)/0.55) + 0.0308)
  out[1,4] <- sd(  ((x[2,,]^2)/0.55) + 0.0308) # standard deviation of the forecasts on day 7 untransformed and in EXO units
  #out[2,4] <- sd( ((x[3,,])^2)/0.55 + 0.0308)
  out[1,5] <- error1_upper
  #out[2,5] <- error2_upper
  out[1,6] <- error1_lower
  #out[2,6] <- error2_lower
  out[1,7] <- max(  ((x[2,,]^2)/0.55) + 0.0308)
  #out[2,7] <- max( ((x[3,,]^2)/0.55) + 0.0308)
  out[1,8] <- min(  ((x[2,,]^2)/0.55) + 0.0308)
  #out[2,8] <- min( ((x[3,,]^2)/0.55) + 0.0308)
  out[1,9] <- var(x[2,,])
  #out[2,9] <- var(x[3,,])
  #out[1,10] <- chla_obs[[1]][8,1] # obs chla on day 7
  out[1,10] <- chla_obs[[1]][15,1]
  out[1,12] <- "2"
  #out[2,12] <- "2"
  out[,13] <- mean(ensemble_pars[,1])
  out[,14]<- mean(ensemble_pars[,2])
  out[,15]<- mean(ensemble_pars[,3])
  out[,16]<- mean(ensemble_pars[,4])
  out[,17]<- mean(ensemble_pars[,5])
  
  
  
  
  setwd(folder)
  
  if(uncert_mode==1){
    write.csv(x, paste0(forecast_location, '/', forecast_ensemble_file_name), row.names = FALSE )
  }
  # and create a single csv for each forecast that is named with day the forecast is run
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
                               "chla_fortnightly.csv")
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
                                "chla_weekly.pdf")
  forecast_plot_output_location <- paste0(forecast_location,  "/",
                                          "ensemble_plots/",
                                          forecast_plot_name) 
  week1 <- as.POSIXct(start_day, format = "%Y-%m-%d %M:%H:%S", tz = reference_tzone) + 7*24*60*60
  
  
  
  if(!is.na(x[1,1,])){
    pdf(file = forecast_plot_output_location )
    x_axis <-   seq.POSIXt(forecast_start_day, week2, by = '2 week')  #c(0,7,14)
    plot(x_axis, ((x[,1,]^2)/0.55 +0.0308), type = 'o',ylim = range(c((min(out[1,8], chla_obs[[1]][8,1], na.rm = TRUE)), max(out[1,7], chla_obs[[1]][8,1], out[1,10], na.rm = TRUE))) # once catwalk data cleaning script is running, can change this to include: out[1,10], out[2,10]
         , xlab = "Date", ylab = "Chla (ug/L)")
    for(m in 2:length(x[1,,1])){
      points(x_axis, ((x[,m,]^2)/0.55 + 0.0308), type = 'o')  
    }
    points(week1, chla_obs[[1]][8,1], col = 'red', pch = 16, cex = 2)
    points(week2, chla_obs[[1]][15,1], col = 'red', pch = 16, cex = 2)
    points(forecast_start_day, chla_obs[[1]][1,1], col = 'red', pch = 16, cex = 2)
    points(week2, out[1,2], col = 'orange', pch = 16, cex = 2)
    legend('topleft', c('observed chla', 'forecast ensembles', 'forecast mean'), col = c('red', 'black', 'orange'), pch = c(16, 1,16), lty = c(0,1,0), bty = 'n')
    dev.off()}
    
  }
   }

  