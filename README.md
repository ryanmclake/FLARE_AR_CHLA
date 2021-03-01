# ReadMe file for FLARE_AR_CHLA
Repository for running and saving forecasts of chlorophyll-a at Falling Creek Reservoir using an autoregressive linear model
Forecasts can be run on a daily basis and code is configured to be run with daily (1 day), weekly (7 day), or fortnightly (14 day) timesteps.


File are organized into the following folders:
`training_datasets`--for storing data files which are run as inputs to bayesian model fitting
`sim_files`--for storing historical observational data at Falling Creek Reservoir
`SCCData_temp`--for storing NOAA weather forecasts
`SCCData`--for storing recent observed data at Falling Creek Reservoir (see notes below for how to download these datafiles) 
`Rscripts`--for storing R scripts needed for data management/QAQC, forecast creation, and analysis
`FCR_forecasts`--for storing forecast output. organized into folders by timestep (e.g., '1day' for forecasts run on a daily timestep), and then by simulation name
`ARIMA_working`--working folder for files created within a forecast run

# Info about installing JAGS on a Mac

# Setting up SCCData data repositories
These repositories need to be updated to new location
Rename the `SCCData_temp` folder to just `SCCData` 
Define your 'data_location' directory to be the `SCCData` folder and run the following commands at the command line:
`setwd(data_location)`

`system("git clone -b carina-data --depth 1 https://github.com/CareyLabVT/SCCData.git carina-data")`

`system("git clone -b mia-data --depth 1 https://github.com/CareyLabVT/SCCData.git mia-data")`

`system("git clone -b diana-data --depth 1 https://github.com/CareyLabVT/SCCData.git diana-data")`

`system("git clone -b manual-data --depth 1 https://github.com/CareyLabVT/SCCData.git manual-data")`

Download the meterology file (Met_final_2015_2019.csv)from the Environmental Data Initiative and place it in the `manual-data` directory: https://portal.edirepository.org/nis/mapbrowse?packageid=edi.389.4


# How to initialize a forecast run
1. Create a directory for running forecasts. I recommend creating a new project in R and cloning the FLARE_AR_CHLA repo 
2. Make sure you've set up the SCCData repository (directions above)
3. Copy the 'noaa-data' folder located at 'SCCData_temp/noaa-data' into the `SCCData` folder
4. Open `automated_forecast_ARIMA.R` and modify for your simulation. (e.g., set the `data_location`, `folder`, `sim_name`, etc.)
5. Choose a `forecast_start_day`
6. Run the entire `automated_forecast_ARIMA.R` script. This will continue running forecasts unless a NOAA forecast is unavailable to drive the forecast
