# run_arima function to produce chl forecasts using the following arima model:
# Chla_CTD_t  = 1.65(±0.26) + 0.45(±0.08)Chla_CTD_t_1  – 3.05(±1.39)Discharge_t – 0.0025(±)Shortwave_t  + Ɛ 

run_arima <- function(
  start_day= "2018-07-06 00:00:00",
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
  FLAREversion
  ){
  
  
  
  
  
  ################################################
  ### LOAD R FUNCTIONS
  #################################################
  # won't need all of these, they are currently called as needed further down in the script but I want to eventually have them organized all right here 
  
  # source(paste0(folder,"/","Rscripts/edit_nml_functions.R"))
  source(paste0(folder,"/","Rscripts/create_obs_met_input.R"))
  #  source(paste0(folder,"/","Rscripts/extract_temp_chain.R"))
  source(paste0(folder,"/","Rscripts/process_GEFS2GLM.R"))
  #  source(paste0(folder,"/","Rscripts/extract_temp_CTD.R"))
  source(paste0(folder,"/","Rscripts/create_inflow_outflow_file_old.R"))
  source(paste0(folder,"/","Rscripts/archive_forecast.R"))
  source(paste0(folder,"/","Rscripts/write_forecast_netcdf.R")) 
  #  source(paste0(folder,"/","Rscripts/GLM_EnKF.R")) 
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
  
  
  # I'm not clear on whether I need this and if yes what it should be for ARIMA, should check how this propagates throughout the function 
  obs_error <- 0.0001 #NEED TO DOUBLE CHECK
  
  #Define modeled depths and depths with observations
  modeled_depths <- 1
  
  # obs depths temp and do are not relevant for ARIMA but am leaving here in case they are called later in the script, this should be cleaned out eventually 
  observed_depths_temp <- c(0.1, 1, 2, 3, 4, 5, 6, 7, 8, 9)
  observed_depths_do <- c(1, 5, 9)
  observed_depths_chla_fdom <- 1
  
  temp_obs_fname <- "Catwalk.csv"
  met_obs_fname <- "FCRmet.csv"
  
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
  if(pull_from_git){
    
    if(!file.exists(temperature_location)){
      setwd(data_location)
      system("git clone -b mia-data --single-branch https://github.com/CareyLabVT/SCCData.git mia-data")
    }
    if(!file.exists(met_station_location)){
      setwd(data_location)
      system("git clone -b carina-data --single-branch https://github.com/CareyLabVT/SCCData.git carina-data")
    }
    if(!file.exists(noaa_location)){
      setwd(data_location)
      system("git clone -b noaa-data --single-branch https://github.com/CareyLabVT/SCCData.git noaa-data")
    }
    
    
    
    setwd(temperature_location)
    system(paste0("git pull"))
    
    setwd(met_station_location)
    system(paste0("git pull"))
    
    setwd(noaa_location)
    system(paste0("git pull"))
  }
  
  
  ###################################
  ##### uncertainty options #########
  #################################
  
  uncert_mode = 1 # this might need to be initialized in the beginning but putting here for now
  if(uncert_mode == 1){
    #All sources of uncertainty and data used to constrain 
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- TRUE
    weather_uncertainty <- TRUE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- TRUE
    met_downscale_uncertainty <- TRUE
  }else if(uncert_mode == 2){
    #No sources of uncertainty  data used to constrain 
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
  }else if(uncert_mode == 3){
    #Only process uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- TRUE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
  }else if(uncert_mode == 4){
    #only noaa weather uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- TRUE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
  }else if(uncert_mode == 5){
    #only initial condition uncertainty with data constraint
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
  }else if(uncert_mode == 6){
    #only initial condition uncertainty without data constraint
    use_obs_constraint <- FALSE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- TRUE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
  }else if(uncert_mode == 7){
    #only parameter uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- TRUE
    met_downscale_uncertainty <- FALSE
  }else if(uncert_mode == 8){
    #only met downscale uncertainty
    use_obs_constraint <- TRUE
    #SOURCES OF uncertainty
    observation_uncertainty <- TRUE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- TRUE
  }else if(uncert_mode == 9){
    #No sources of uncertainty and no data used to constrain 
    use_obs_constraint <- FALSE
    #SOURCES OF uncertainty
    observation_uncertainty <- FALSE
    process_uncertainty <- FALSE
    weather_uncertainty <- FALSE
    initial_condition_uncertainty <- FALSE
    parameter_uncertainty <- FALSE
    met_downscale_uncertainty <- FALSE
  }
  
  
  if(observation_uncertainty == FALSE){
    obs_error <- 0.000001
  }
  
  ####################################################
  #### STEP 2: DETECT PLATFORM  
  ####################################################
  # I think this is only relevant for the GLM part but am leaving it anyway
  
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
  nsteps <- 3 # number of timesteps: initial, day 7, day 14
  
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
  working_glm <- paste0(folder, "/", "GLM_working")  
  ####Clear out temp GLM working directory
  unlink(paste0(working_glm, "/*"), recursive = FALSE)    
  forecast_base_name <- paste0(year(forecast_start_day),
                               forecast_month,
                               forecast_day,
                               "gep_all_00z")
  temp_obs_fname_wdir <-  paste0(working_glm, "/", temp_obs_fname)
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
  obs_met_outfile <- paste0(working_glm, "/", "GLM_met.csv")
  create_obs_met_input(fname = met_obs_fname_wdir,
                       outfile=obs_met_outfile,
                       full_time_hour_obs, 
                       input_tz = "EST5EDT", 
                       output_tz = reference_tzone)
  met_file_names[1] <- obs_met_outfile
  
  ###CREATE FUTURE MET FILES
  if(forecast_days > 0){
    in_directory <- paste0(noaa_location)
    out_directory <- working_glm
    file_name <- forecast_base_name
    #NEED TO DOUBLE CHECK THE INPUT_TZ AND WHY IT IS EST
    #met_file_names[2:(1+(n_met_members*n_ds_members))] <- process_GEFS2GLM(in_directory,
    #                                                        out_directory,
    #                                                        file_name, 
    ##                                                        #NEED TO CHANGE TO GMT IF FORECASTING AFTER DEC 8 00:00:00 GMT
    #                                                        input_tz = "EST5EDT", 
    #                                                        output_tz = reference_tzone)
    
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
                                                                                 working_glm,
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
    #plot_downscaled_met(met_file_names, VarInfo$VarNames, working_glm)
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
    for(i in 1:length(unique(temp$date))){
      temp1<-subset(temp, temp$date==unique(temp$date)[i])
      temp2 <- temp1 %>% mutate(SW = mean(temp1$ShortWave))  %>% 
        mutate(rain_sum = sum(temp1$Rain))
      data[i,j-1]=temp2[1,10]
      rain[i, j-1] = temp2[1,11]
    }
  }
  
  # extract all ensemble members for forecasts on day 7 and day 14, which are the 8th and 15th entries because we are pulling from the forecast made at midnight on the present day
  sw_forecast <- data[c(1,8,15),]  
  
  ## I don't think ARIMA needs this chunk of code
  ###MOVE DATA FILES AROUND
  sim_files_folder <- paste0(folder, "/", "sim_files")
  GLM_folder <- paste0(folder, "/", "glm", "/", machine) 
  fl <- c(list.files(sim_files_folder, full.names = TRUE))
  tmp <- file.copy(from = fl, to = working_glm, overwrite = TRUE)
  fl <- c(list.files(GLM_folder, full.names = TRUE))
  tmp <- file.copy(from = fl, to = working_glm, overwrite = TRUE)
  if(!is.na(restart_file)){
    tmp <- file.copy(from = restart_file, to = working_glm, overwrite = TRUE)
  }
  if(pre_scc){
    fl <- c(list.files("/Users/quinn/Dropbox (VTFRS)/Research/SSC_forecasting/SCC_data/preSCC/",
                       full.names = TRUE))
    tmp <- file.copy(from = fl, to = working_glm, overwrite = TRUE)
  }
  
  ###############################################
  ####CREATE INFLOW AND OUTFILE FILES############
  ###############################################
  
  # right now using the average inflow on given day from historical data
  # soon will build in diana discharge data and use autoregressive model
  # source create inflwo outflow file
  
  # 16 MAY: using old create_inflow_outflow_file script because new script includes wetland inflow data that is not needed for arima  
  
  source(paste0(folder,"/","Rscripts/create_inflow_outflow_file_old.R"))
  # download the inflow data    
  download.file('https://github.com/CareyLabVT/SCCData/raw/diana-data/FCRweir.csv','./SCCData/FCRweir.csv')
  dianaheader<-read.csv("./SCCData/FCRweir.csv", skip=1, as.is=T) #get header minus wonky Campbell rows
  dianadata<-read.csv("./SCCData/FCRweir.csv", skip=4, header=F) #get data minus wonky Campbell rows
  names(dianadata)<-names(dianaheader) #combine the names to deal with Campbell logger formatting
  dianadata$TIMESTAMP <- as.POSIXct(dianadata$TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")
  colnames(dianadata)[colnames(dianadata)=="Lvl_psi"] <- "diana_psi_corr"
  dianadata <- dianadata %>% select("TIMESTAMP", "diana_psi_corr")
  dianadata <- dianadata %>% 
    mutate(head = (0.149*diana_psi_corr)/0.293) %>% 
    mutate(flow_cms = 2.391* (head^2.5)) 
  # calculate average flow for each day
  diana_avg <- dianadata %>% mutate(date = date(TIMESTAMP)) %>% 
    group_by(date) %>% 
    mutate(flow_daily = mean(flow_cms))
  diana_avg2 <- diana_avg[!duplicated(diana_avg$date), ]
  diana_avg2$flow_daily_lag <- lag(diana_avg2$flow_daily, n = 1L)
  
  discharge_pred <- matrix(data = NA, nrow(rain), (length(met_file_names) - 1))
  
  for(i in 1:(nrow(discharge_pred))){
        temp <- diana_avg2[diana_avg2$date>=forecast_start_day & diana_avg2$date<=(forecast_start_day + 16*24*60*60),] #subset to just the period within the 16-day forecast horizon
        temp2 <- temp %>% pred[i] = 0.0002172 + 0.9535769*temp$flow_daily_lag[i] + 0.0253582*rain[i]
        discharge_pred[i, 1] = temp2[1,8]   
  
  
  
        for (j in 2:ncol(rain)) {
        temp3 <- temp %>% mutate(pred = 0.0002172 + 0.9535769*temp$flow_daily_lag[i] + 0.0253582*rain[j] )
          discharge_pred[i, j] <- temp3[i,j]
          
       }                
      
    }
    
  
  
  # lm equation to the AR model of mean flow found in thesis/data/inflows_modeling/hydromodeling/lm_inflow_final.R
  # mean flow(t) = 0.0002172 + 0.9535769*meanflow(t-1) + 0.0253582*rain_sum(t)
  # meanflow(t-1) is mean flow from the day before
  # should develop this so that it get parameterized through bayesian framework, jags??
  
  # pull from matrix 'rain' created earlier in script to get forecasts of rain for this lm
  
  
 # ifelse(forecast_start_day > "2019-06-06", #use the lm
         
  #       , # else, use the average on the given day from the past five years, calculated in create_inflow_outflow_file()
         
         create_inflow_outflow_file(full_time ,
                                    working_glm = working_glm, 
                                    input_tz = "EST5EDT",
                                    output_tz = reference_tzone)
         
         Discharge <- read.csv(paste0(folder,"/", "GLM_working/FCR_inflow.csv"))
  
  
  # need to pull the 7th and 14th days of the forecast
  week1 <- as.POSIXct(start_day, format = "%Y-%m-%d %M:%H:%S", tz = reference_tzone) + 7*24*60*60
  week2 <- as.POSIXct(start_day,
                      tz = reference_tzone) + 14*24*60*60
  
  discharge_forecast <- Discharge[date(Discharge$time)==date(week1) | date(Discharge$time)==date(week2) | date(Discharge$time)==date(forecast_start_day),]
  discharge_forecast <- discharge_forecast[,1:2]
  
  
  ############################################
  ##### chla data extraction and download  ###
  ############################################
  
  source(paste0(folder,"/","Rscripts/extract_temp_chain.R"))
  
  observed_depths_chla_fdom <- 1
  temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
  
  
  chla_obs <- extract_chla_chain(fname = temp_obs_fname_wdir,
                                 full_time,
                                 depths = 1.0,
                                 observed_depths_chla_fdom = observed_depths_chla_fdom,
                                 input_tz = "EST5EDT", 
                                 output_tz = reference_tzone)
  
  
  # chla_obs is the matrix to pull from for day 1, then chla_obs-1 will be used for day 7 and day14
  # can specify full_time as the days that I want
  
  #################################
  ### populate the forecast array##
  #################################
  
  # the first column is simply the exo sonde data on the day the forecast is being run
  # we correct here to convert the observed EXO chl in CTD units since that is what the model is developed on
  # read in the jags file to pull from parameter values 
  load("C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3/MCMC_output_ARIMA_Whitney.Rdata")
  
  # the first column is observed chl that is sqrt transformed (because the model is based on sqrt units) and corrected into CTD units (because it is observed in EXO units)
  x[1,1:nmembers,] <- sqrt(chla_obs[[1]][1,1]*0.55 - 0.0308)
  
  npars <- 5
  ensemble_pars <- array(NA, dim = c(nmembers, npars)) 
  
  # for loop to propagate parameter uncertainty (sort of)
  for(j in 1:nmembers){
    p <- sample(seq(0,length(samples[[1]][,1])), 1, replace = TRUE)
    ensemble_pars[j, 1] <- samples[[1]][p,1]
    ensemble_pars[j, 2] <- samples[[1]][p,2]
    ensemble_pars[j, 3] <- samples[[1]][p,3]
    ensemble_pars[j, 4] <- samples[[1]][p,4]
    ensemble_pars[j, 5] <- samples[[1]][p,5]
  }  
  
  # the model!
  for(i in 2:3){
    met_index <- 1
    for(j in 1:nmembers){  #want this to be length(nmembers) = 21
      x[i,j,] <- ensemble_pars[j, 1] + ensemble_pars[j, 2]*x[i-1,j,] + ensemble_pars[j, 3]*discharge_forecast[i,2] + ensemble_pars[j, 4]*sw_forecast[i,met_index] + rnorm(1, 0,ensemble_pars[j, 5])
      if(met_index > 21){
        met_index <- 21
      }
    }
  }
  
  
  
  error1 <- qnorm(0.975, mean = mean(x[2,,]), sd =sd((x[2,,])) )
  error1 <- (error1^2)/0.55 + 0.0308
  error2 <- qnorm(0.975, mean = mean(x[3,,]), sd = sd(x[3,,]))
  error2 <- (error2^2)/0.55 + 0.0308
  
  # create the output dataframe for archiving the forecast statistics (mean, sd, 95% CI, and obs chl)
  out <- data.frame("date" = c(week1, week2), "forecast_mean_chl" =rep(NA) , "forecast_sd_chl" = rep(NA), "forecast_CI_95" = rep(NA), "obs_chl_EXO" = rep(NA), "forecast_start_day" = start_day, "week" = rep(NA))
  out[1,2] <- mean(    ((x[2,,]^2)/0.55) + 0.0308) # mean of the forecasts on day 7 un-sqrt-transformed and in EXO units
  out[2,2] <- mean(    ((x[3,,]^2)/0.55) + 0.0308) # mean of the forecasts on day 14 untransformed and in EXO units
  out[1,3] <- sd(  ((x[2,,]^2)/0.55) + 0.0308) # standard deviation of the forecasts on day 7 untransformed and in EXO units
  out[2,3] <- sd( ((x[3,,])^2)/0.55 + 0.0308)
  out[1,4] <- error1 #95% CI of the forecasts on day 7
  out[2,4] <- error2
  out[1,5] <- chla_obs[[1]][7,1] # obs chla on day 7
  out[2,5] <- chla_obs[[1]][14,1]
  #out[1:2,6] <- as.POSIXct(start_day, format = "%Y-%m-%d")
  out[1,7] <- "1"
  out[2,7] <- "2"
  
  
  setwd(folder)
  
  write.table( out,  
               file="./FCR_forecasts/ARIMA_forecast.csv", 
               append = T, 
               sep=',', 
               row.names=F, 
               col.names=F)
  
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
                               "chla_weekly.csv")
  forecast_output_location <- paste0(forecast_location,  "/",
                                     forecast_file_name)
  write.csv(out, forecast_output_location, row.names = FALSE)
  
 
  # and make a simple plot to archive the ensemble spread
  forecast_plot_name <-  paste0(year(forecast_start_day), "_", 
                                file_name_forecast_start_month, "_", 
                                file_name_forecast_start_day, "_", 
                                "chla_weekly.pdf")
  forecast_plot_output_location <- paste0(forecast_location,  "/",
                                          "ensemble_plots/",
                                          forecast_plot_name)
  
  pdf(file = forecast_plot_output_location )
  x_axis <-   seq.POSIXt(forecast_start_day, week2, by = 'week')  #c(0,7,14)
  plot(x_axis, ((x[,1,]^2)/0.55 +0.0308), type = 'o',ylim = range(c((x^2)/0.55 +0.0308)), xlab = "Date", ylab = "Chla (ug/L)")
  for(m in 2:length(x[1,,1])){
    points(x_axis, ((x[,m,]^2)/0.55 + 0.0308), type = 'o')  
  }
  points(week1, chla_obs[[1]][7,1], col = 'red', pch = 16, cex = 2)
  points(week2, chla_obs[[1]][14,1], col = 'red', pch = 16, cex = 2)
  points(forecast_start_day, chla_obs[[1]][1,1], col = 'red', pch = 16, cex = 2)
  points(week1, out[1,2], col = 'orange', pch = 16, cex = 2)
  points(week2, out[2,2], col = 'orange', pch = 16, cex = 2)
  legend('topleft', c('observed chla', 'forecast ensembles', 'forecast mean'), col = c('red', 'black', 'orange'), pch = c(16, 1,16), lty = c(0,1,0), bty = 'n')
  dev.off()
  

  
}
