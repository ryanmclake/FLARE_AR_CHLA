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
