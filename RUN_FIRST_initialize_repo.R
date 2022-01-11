###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse,lubridate,mvtnorm,
               RCurl,testit,imputeTS,modelr,
               RcppRoll,PerformanceAnalytics,
               rjags,LaplacesDemon,scales,
               Metrics,rsq)


### STEP 00 ---- 

# Initialize the repository for use (set up folders, download published data and data from other repositories)
# after cloning the github repo: https://github.com/wwoelmer/FLARE_AR_CHLA/tree/EcoAppsReleaseApr2021
folder <- getwd()

### Create a directory with the NOAA GEFS forecasts in FCR
dir.create('./SCCData')
dir.create('./SCCData/bvre-data') # location for Beaverdam data

#### Download data from GitHub Repo where data are automatically uploaded from the field: https://github.com/FLARE-forecast/FCRE-data
### Set the directory to the data repo
setwd('./SCCData/')

### Meteorological data from the dam
system("git clone -b fcre-metstation-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git carina-data")

### Physical, Chemical, and Biological data from sensors off of the Catwalk in FCR (See fig. XX in MS)
system("git clone -b fcre-catwalk-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git mia-data")

### Discharge data from the primary inflow into FCR (See fig. 1 in MS)
system("git clone -b fcre-weir-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git diana-data")

setwd('..')

##### Download published data from the Environmental Data Initiative (EDI)
### Download the published inflow data from EDI
data  <-  "https://pasta.lternet.edu/package/data/eml/edi/202/7/f5fa5de4b49bae8373f6e7c1773b026e" 
destination <- "./sim_files"
download.file(data, destfile = paste0(destination, '/inflow_for_EDI_2013_10Jan2021.csv'), method='curl')

### Download the published meterological data from EDI. EDI == Environmental Data Initiative
# download met data
data  <-  "https://pasta.lternet.edu/package/data/eml/edi/389/5/3d1866fecfb8e17dc902c76436239431" 
destination <- "./sim_files"
download.file(data, destfile = paste0(destination, '/Met_final_2015_2020.csv'), method='curl')

##### Download the published CTD data from EDI. EDI == Environmental Data Initiative
data  <- "https://pasta.lternet.edu/package/data/eml/edi/200/11/d771f5e9956304424c3bc0a39298a5ce"
destination <- paste0(folder, "/sim_files")
download.file(data, destfile = paste0(destination, '/CTD_final_2013_2020.csv'), method='curl')

##### Download the published FCR EXO data from EDI. EDI == Environmental Data Initiative
data  <-  "https://pasta.lternet.edu/package/data/eml/edi/271/5/c1b1f16b8e3edbbff15444824b65fe8f" 
destination <- paste0(folder, "/sim_files")
download.file(data, destfile = paste0(destination, '/Catwalk_EDI_2020.csv'), method='curl')

########################################################################################################################################
# data from Beaverdam Reservoir (bvre) 

### Download the BVR Catwalk data
data  <-  'https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/bvre-waterquality.csv'
destination <- paste0(folder, "/SCCData/bvre-data")
download.file(data, destfile = paste0(destination, '/bvre-waterquality.csv'))

### Download BVR Catwalk Maintenance Log
data  <-  'https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/BVR_maintenance_log.txt'
destination <- paste0(folder, "/SCCData/bvre-data")
download.file(data, destfile = paste0(destination, '/BVR_maintenance_log.txt'))

#soil data
url="https://websoilsurvey.sc.egov.usda.gov/DSD/Download/AOI/u3w5wfn2eaouxxeoweiwlw12/wss_aoi_2021-10-13_12-59-06.zip"
location <- "./SCCData/bvre-data/usda_soil_data/"
download.file(url, paste0(location, "mysoil.zip")) #Note: will probably have to update wss_aoi date if it's been a while - go to wss homepage and click on start wss link on right of page
unzip(paste0(location, "mysoil.zip"))            #zoom in to site, use define aoi tool to select desired area, go to download soils data tab, scroll to bottom of page and click "create download link", right click and copy link address, paste on line 10

# Grab the necessary soil and elevation spatial layers and parameters (usgs)
url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_03010101_HU8_Shape.zip"
curl_download(url,paste0(location, "NHD_H_03010101_HU8_Shape.zip"))
unzip(paste0(location, "NHD_H_03010101_HU8_Shape.zip",exdir="03010101"))