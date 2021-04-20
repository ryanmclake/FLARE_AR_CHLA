# script initialize repository for use (set up folders, download published data and data from other repositories)

# after cloning the github repo: https://github.com/wwoelmer/FLARE_AR_CHLA/tree/EcoAppsReleaseApr2021


dir.create('./SCCData/noaa-data')
files <- list.files('./SCCData_temp/noaa-data/', full.names = TRUE)
file.copy(files, './SCCData/noaa-data')

setwd('./SCCData/')

system("git clone -b fcre-metstation-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git carina-data")

system("git clone -b fcre-catwalk-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git mia-data")

system("git clone -b fcre-weir-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git diana-data")

system("git clone -b fcre-manual-data --depth 1 https://github.com/FLARE-forecast/FCRE-data.git manual-data")

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/389/5/3d1866fecfb8e17dc902c76436239431" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


dir.create('./SCCData/carina-data')
dir.create('./SCCData/diana-data')
dir.create('./SCCData/manual-data')
dir.create('./SCCData/mia-data')
