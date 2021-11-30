# script to download data needed to run scripts in this repository, historical_model_selection in prep for running forecasts of chl-a at Falling Creek Reservoir


# download CTD data
data  <- "https://pasta.lternet.edu/package/data/eml/edi/200/11/d771f5e9956304424c3bc0a39298a5ce"
destination <- "./historical_model_selection/Data/CTD"
download.file(data, destfile = paste0(destination, '/CTD_final_2013_2020.csv'), method='curl')

# download fluoroprobe data
data <- "https://pasta.lternet.edu/package/data/eml/edi/272/5/a24f4dbe9f0d856688f257547069d0a3"
destination <- "./historical_model_selection/Data/Fluoroprobe"
download.file(data, destfile = paste0(destination, '/FluoroProbe.csv'), method='curl')

# download inflow data
data  <-  "https://pasta.lternet.edu/package/data/eml/edi/202/7/f5fa5de4b49bae8373f6e7c1773b026e" 
destination <- "./historical_model_selection/Data/Inflow"
download.file(data, destfile = paste0(destination, '/inflow_for_EDI_2013_10Jan2021.csv'), method='curl')

# download met data
data  <-  "https://pasta.lternet.edu/package/data/eml/edi/389/5/3d1866fecfb8e17dc902c76436239431" 
destination <- "./historical_model_selection/Data/MET"
download.file(data, destfile = paste0(destination, '/Met_final_2015_2020.csv'), method='curl')

# download water chemistry data
data  <- "https://pasta.lternet.edu/package/data/eml/edi/199/8/da174082a3d924e989d3151924f9ef98"
destination <- "./historical_model_selection/Data/water_chemistry"
download.file(data, destfile = paste0(destination, '/chemistry.csv'), method='curl')

# download PAR secchi data
data  <- "https://pasta.lternet.edu/package/data/eml/edi/198/8/07ba1430528e01041435afc4c65fbeb6"
destination <- "./historical_model_selection/Data/YSI_PAR_SECCHI"
download.file(data, destfile = paste0(destination, '/YSI_PAR_profiles.csv'), method='curl')

# download catwalk data
### Physical, Chemical, and Biological data from sensors off of the Catwalk in FCR (See fig. XX in MS)
dir.create('./SCCData')
setwd('./SCCData/')
system("git clone -b fcre-catwalk-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git mia-data")


data  <- "https://pasta.lternet.edu/package/data/eml/edi/198/8/07ba1430528e01041435afc4c65fbeb6"
destination <- "./historical_model_selection/Data/Catwalk"
download.file(data, destfile = paste0(destination, '/Catwalk.csv'), method='curl')


