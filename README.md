# ReadMe file for FLARE_AR_CHLA
Repository for running and saving forecasts of chlorophyll-a at Falling Creek Reservoir and Beaverdam Reservoir using an autoregressive linear model
Forecasts can be run every day and code is configured to be run with daily (1 day), weekly (7 day), or fortnightly (14 day) timesteps.


## File are organized into the following folders: ##
* `training_datasets`--for storing data files which are run as inputs to bayesian model fitting\
* `sim_files`--for storing historical observational data published in the Environemtnal Data Initiative (EDI) at Falling Creek and Beaverdam Reservoir\
* `SCCData`--for storing observed data hosted on Github (https://github.com/FLARE-forecast/FCRE-data) at Falling Creek Reservoir. At the end of each year, these data are published on EDI, but workflows are designed to work with data directly from GitHub. `mia-data` stores observed water quality data from FCR, `diana-data` stores observed discharge data from a weir at FCR, `carina-data` stores observed meteorological data from FCR, and `bvre-data` stores observed water quality data from Beaverdam Reservoir \
* `Rscripts`--for storing R scripts needed for data management/QAQC, forecast creation, and analysis\
* `FCR_forecasts`--for storing forecast output. organized into folders by timestep (e.g., '1day' for forecasts run on a daily timestep), and then by simulation name\
* `ARIMA_working`--working folder for files created within a forecast run\
* `historical_model_selection`--for storing files relevant for the selection of the autoregressive model which was chosen for this analysis. This analysis occured before the forecasts were developed in order to select a model using linear regression and AIC

# Info about installing JAGS 
JAGS (Just Another Gibbs Sampler) is an independent program for analysis of Bayesian models which is used in this forecast analysis. The program needs to be installed on your computer before you can run code using the `rjags` package. You can find information on downloading JAGS here: https://mcmc-jags.sourceforge.io/

# Setting up SCCData data repositories
After cloning this repository, you will open the `RUN_FIRST_initialize_repo.R` script to download files from GitHub and EDI
'carina-data' this corresponds to data from the FCR meteorological station\
'diana-data' this corresponds to data from the incoming stream weir\
'manual-data' this corresponds to manually collected profiles at FCR\
'mia-data' this corresponds to data from the FCR catwalk sensors\


# How to initialize a forecast run
1. Create a directory for running forecasts. I recommend creating a new project in R and cloning the FLARE_AR_CHLA repo 
2. Make sure you've set up the SCCData repository (run the `RUN_FIRST_initialize_repo.R` script to ensure you have downloaded all necessary files)
3. Open `automated_forecast_ARIMA.R` and modify for your simulation. (e.g., set the `data_location`, `folder`, `sim_name`, etc.)
4. Choose a `forecast_start_day`
5. Run the entire `automated_forecast_ARIMA.R` script. This will continue running forecasts unless a NOAA forecast is unavailable to drive the forecast\

# A bit more about the Rscripts and running forecasts
* Scripts are numbered to indicate the order in which they should be run. Scripts which are not numbered are functions which are called within the numbered scripts.
* Scripts 1-5 perform actions to set up dataframes in order to run the forecasts
* Scripts 6a and 6b will automatically run forecasts starting the date that you specify. `6a1_automated_forecast_ARIMA.R` will run the daily forecasts over the forecast time period, `6a2_automated_forecast_ARIMA.R` for weekly, and `6a3_automated_forecast_ARIMA.R` for fortnightly. The `6b*.R` scripts similarly run daily, weekly, and fortnightly null forecasts. 
  * __NOTE:__ Running forecasts over the entire time period will take multiple days and there may be specific dates when the function will crash, necessitating you to input a new date and continue the simulation. Forecasts for the manuscript for re-submission to Ecological Applications in Feb 2022 have already been created in `FCR_forecasts/*day/Mar2021_UC`. Code to analysis these forecasts and produce figures can be founds in scripts numbered 7-12.
* `supplementary_analysis_files` includes code used to run analysis in the supplemental material of the manuscript (re-submitted to Ecological Application Feb 2022)
* `scalability_analysis` includes code needed to set up and analyze forecasts run at FCR and BVR from August 2020-2021. This analysis was conducted to examine the performance of the forecast model at an additional study site.  
  * __NOTE:__ Running forecasts (in script 5a and 5b) over the entire time period will take multiple days and there may be specific dates when the function will crash, requiring you to input a new date and continue the simulation. Forecasts for the manuscript for re-submission to Ecological Applications in Feb 2022 have already been created in `FCR_forecasts/7day/BVR_scalability_Aug2020-2021` and `FCR_forecasts/7day/FCR_scalability_Aug2020-2021`. The output from these forecasts is then analyzed in scripts 6-7 within this folder. 



