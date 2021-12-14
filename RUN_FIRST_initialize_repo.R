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
dir.create('./SCCData/noaa-data')
files <- list.files('./SCCData_temp/noaa-data/', full.names = TRUE)
file.copy(files, './SCCData/noaa-data')

### Set the directory to the data repo
setwd('./SCCData/')

### Meteorological data from the dam
system("git clone -b fcre-metstation-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git carina-data")

### Physical, Chemical, and Biological data from sensors off of the Catwalk in FCR (See fig. XX in MS)
system("git clone -b fcre-catwalk-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git mia-data")

### Discharge data from the primary inflow into FCR (See fig. XX in MS)
system("git clone -b fcre-weir-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git diana-data")

### Manually collected data from FCR's z-max (See fig. XX in MS)
system("git clone -b fcre-manual-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git manual-data")

### Download the published meterological data from EDI. EDI == Environmental Data Initiative
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/389/5/3d1866fecfb8e17dc902c76436239431" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

### Download the published inflow data from EDI. EDI == Environmental Data Initiative
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/202/7/f5fa5de4b49bae8373f6e7c1773b026e"
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##### Download the published CTD data from EDI. EDI == Environmental Data Initiative
data  <- "https://pasta.lternet.edu/package/data/eml/edi/200/11/d771f5e9956304424c3bc0a39298a5ce"
destination <- paste0(folder, "/sim_files")
download.file(data, destfile = paste0(destination, '/CTD_final_2013_2020.csv'), method='curl')

##### Download the published EXO data from EDI. EDI == Environmental Data Initiative
data  <-  "https://pasta.lternet.edu/package/data/eml/edi/271/5/c1b1f16b8e3edbbff15444824b65fe8f" 
destination <- paste0(folder, "/sim_files")
download.file(data, destfile = paste0(destination, '/Catwalk_EDI_2020.csv'), method='curl')

########################################################################################################################################
# data from Beaverdam Reservoir (bvre)

#soil data
url="https://websoilsurvey.sc.egov.usda.gov/DSD/Download/AOI/u3w5wfn2eaouxxeoweiwlw12/wss_aoi_2021-10-13_12-59-06.zip"
location <- "./SCCData/bvre-data/usda_soil_data/"
download.file(url, paste0(location, "mysoil.zip")) #Note: will probably have to update wss_aoi date if it's been a while - go to wss homepage and click on start wss link on right of page
unzip(paste0(location, "mysoil.zip"))            #zoom in to site, use define aoi tool to select desired area, go to download soils data tab, scroll to bottom of page and click "create download link", right click and copy link address, paste on line 10

# Grab the necessary soil and elevation spatial layers and parameters (usgs)
url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_03010101_HU8_Shape.zip"
curl_download(url,paste0(location, "NHD_H_03010101_HU8_Shape.zip"))
unzip(paste0(location, "NHD_H_03010101_HU8_Shape.zip",exdir="03010101"))