#calculating flow for BVR using the Thornthwaite-mather water balance model
#modified to daily timestep - added in recharge to help with baseflow underestimation 11Jun2020
#Updated 4Sep2020 - change from GSOD temp/precip data to NLDAS for consistency 
#Updated 23Sep21 - change to obs met data because forecasts look weird when going from glm to FLARE

##### using HLW's script to calculate inflow for BVR and update existing file which goes through 2019 to include 2020 and 2021 estimates

#packages
if (!require("pacman"))install.packages("pacman")
pacman::p_load(httr,EcoHydRology,GSODR,curl,elevatr,raster,soilDB,rgdal,lattice,lubridate, tidyverse)
library(magrittr)

#soil data
url="https://websoilsurvey.sc.egov.usda.gov/DSD/Download/AOI/u3w5wfn2eaouxxeoweiwlw12/wss_aoi_2021-10-13_12-59-06.zip"
location <- "./SCCData/bvre-data/usda_soil_data/"
download.file(url, paste0(location, "mysoil.zip")) #Note: will probably have to update wss_aoi date if it's been a while - go to wss homepage and click on start wss link on right of page
unzip(paste0(location, "mysoil.zip"))            #zoom in to site, use define aoi tool to select desired area, go to download soils data tab, scroll to bottom of page and click "create download link", right click and copy link address, paste on line 10

list.files(paste0(location, "mysoil/wss_aoi_2021-10-13_12-59-06/spatial/"),pattern = "shp")
list.files(paste0(location, "mysoil/wss_aoi_2021-10-13_12-59-06/tabular/"))

objects()
rm(list=objects())

#Using ROANOKE RIVER AT NIAGARA, VA  usgs gage to use as a template (will write over with BVR-specific data) 
myflowgage_id="02056000"
myflowgage=get_usgs_gage(myflowgage_id,begin_date = "2015-01-01",end_date = Sys.Date())

#change coordinates and area for entire BVR watershed
myflowgage$area<- 2.27 #km
myflowgage$declat<- 37.31321
myflowgage$declon<- -79.81535

#read in obs met data from FCR met station
met <- read.csv("./sim_files/Met_final_2015_2020.csv")

#drop first 3 rows because end of 1600 hour
met <- met[-c(1:3),]

#only select first entry for each hour
met_hourly <- met %>% dplyr::select(c(DateTime,ShortwaveRadiationUp_Average_W_m2,InfaredRadiationUp_Average_W_m2,
                               AirTemp_Average_C,RH_percent,WindSpeed_Average_m_s,Rain_Total_mm)) %>% 
                      mutate(DateTime = ymd_hms(DateTime), 
                             dt = as_date(DateTime), 
                             hr = hour(DateTime)) %>% 
                      group_by(dt, hr) %>% filter(DateTime == min(DateTime)) #%>% filter(DateTime <=as.Date("2019-12-31")) 
met_hourly_final <- met_hourly[,-c(8,9)]
names(met_hourly_final) <- c("time","ShortWave","LongWave","AirTemp","RelHum","WindSpeed","Rain")
#write.csv(met_hourly_final,"inputs/FCR_hourly_met_2015_2020.csv",row.names=FALSE)

#convert to as.date format
met$DateTime  <-as.Date(met$DateTime)

#then summarize by date
met_daily <- met %>% dplyr::select(DateTime, AirTemp_Average_C, Rain_Total_mm) %>% group_by(DateTime) %>%
  rename(mdate=DateTime) %>% filter(mdate<=as.Date("2020-12-31")) %>%
  summarise(MaxTemp_C = max(AirTemp_Average_C, na.rm=T),
            MinTemp_C = min(AirTemp_Average_C, na.rm=T),
            MeanTemp_C = mean(AirTemp_Average_C, na.rm=T),
            Precip_mmpd = sum(Rain_Total_mm, na.rm=T)) 

# add in 2021 data from github dataset
#download.file("https://github.com/FLARE-forecast/FCRE-data/blob/fcre-metstation-data/FCRmet.csv?raw=true", './SCCData/carina-data/FCRmet.csv')

d <- read.csv( './SCCData/carina-data/FCRmet.csv', skip = 3)
d_names <- read.csv('./SCCData/carina-data/FCRmet.csv', skip = 1)
names(d) <- names(d_names)

#then summarize by date
d$TIMESTAMP <- as.Date(d$TIMESTAMP)
daily_2021 <- d %>% 
  dplyr::select(TIMESTAMP, AirTC_Avg, Rain_mm_Tot) %>% 
  group_by(TIMESTAMP) %>%
  rename(mdate=TIMESTAMP) %>% 
  filter(mdate>=as.Date("2020-12-31")) %>%
  summarise(MaxTemp_C = max(AirTC_Avg, na.rm=T),
            MinTemp_C = min(AirTC_Avg, na.rm=T),
            MeanTemp_C = mean(AirTC_Avg, na.rm=T),
            Precip_mmpd = sum(Rain_mm_Tot, na.rm=T)) 


met_final <- rbind(met_daily, daily_2021)
plot(met_final$mdate, met_final$Precip_mmpd)

#replace flow with NAs because this is specific to Roanoke River (not BVR)
myflowgage$flowdata[["flow"]] <- NA

# Merge met_final weather data with flow gage to use as our base HRU data structure
myflowgage$TMWB=merge(myflowgage$flowdata,met_final)

# Grab the necessary soil and elevation spatial layers and parameters (usgs)
#url="https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/NHD/HU8/HighResolution/Shape/NHD_H_03010101_HU8_Shape.zip"
#curl_download(url,paste0(location, "NHD_H_03010101_HU8_Shape.zip"))
#unzip(paste0(location, "NHD_H_03010101_HU8_Shape.zip",exdir="03010101"))

#set coordinates to plot DEM raster
degdist=sqrt(myflowgage$area*4)/80
mybbox = matrix(c(
  myflowgage$declon - degdist, myflowgage$declon + degdist, 
  myflowgage$declat - degdist, myflowgage$declat + degdist), 
  ncol = 2, byrow = TRUE)

streams=readOGR(paste0(location, "NHD_H_03010101_HU8_Shape/Shape/NHDFlowline.dbf")) 

mysoil <- readOGR(file.path(location, "mysoil/wss_aoi_2021-10-13_12-59-06/spatial/soilmu_a_aoi.dbf"))

# Associate mukey with cokey from component
mukey_statement = format_SQL_in_statement(unique(mysoil$mukey))
q_mu2co = paste("SELECT mukey,cokey FROM component WHERE mukey IN ", mukey_statement, sep="")
mu2co = SDA_query(q_mu2co)

# Second associate cokey with ksat_r,awc_r,hzdepb_r from chorizon
cokey_statement = format_SQL_in_statement(unique(mu2co$cokey))
q_co2ch = paste("SELECT cokey,ksat_r,awc_r,hzdepb_r  FROM chorizon WHERE cokey IN ", cokey_statement, sep="")
co2ch = SDA_query(q_co2ch)

# Aggregate max values of ksat_r,awc_r, and hzdepb_r
mu2ch=merge(mu2co,co2ch)
mu2chmax=aggregate(mu2ch,list(mu2ch$mukey),max)

#set projection
proj4string(streams)
proj4string(mysoil)<- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Use the spatial extents from our stream to download elevation raster.
mydem=get_elev_raster(mysoil, z = 11, src ="aws",clip="bbox")

#view watershed
plot(mydem)
lines(mysoil,col="black")
points(myflowgage$declon,myflowgage$declat,pch = 24, cex=2, col="blue", bg="red", lwd=2)

# For initializing slopes, we store the summary stats for terrain slope
slope_sum=summary(terrain(mydem, opt='slope',unit = "radians"))

# 3 Functions to calculate SWE and excess when soil is drying, wetting, and wetting above capacity
soildrying<-function(AWprev,dP,AWC){
  AW<-AWprev*exp(dP/AWC)
  excess<-0.0
  c(AW,excess)
}

soil_wetting_above_capacity<-function(AWprev,dP,AWC){
  AW<-AWC
  excess<-AWprev+dP-AWC
  c(AW,excess)
}

soilwetting<-function(AWprev,dP,AWC){
  AW<-AWprev+dP
  excess<-0.0
  c(AW,excess)
}

#initialize parameters
myflowgage$TMWB$AWC=0.13*400 #AWC=.13; 0.12 and 0.16 were the values obtained from USDA web soil survey
  # z=2000mm --> this one is hard because it really changes Qpred a LOT - calibrate this parameter? trees generally have <3500 mm roots...
myflowgage$TMWB$dP = 0 # Net precip
myflowgage$TMWB$ET = 0 # Evapotranspiration
myflowgage$TMWB$Albedo=.23
myflowgage$TMWB$PET = 0 # Potential evapotranspiration
myflowgage$TMWB$AW =  100 # Available water
myflowgage$TMWB$SnowMelt_mm = 0 
myflowgage$TMWB$SnowfallWatEq_mm = 0 # New snow
myflowgage$TMWB$SnowWaterEq_mm = 0  # Snow depth
myflowgage$TMWB$ExcessOut = 0 # Excess going out (runoff)
myflowgage$TMWB$Drainage = 0
myflowgage$TMWB$Qpred=NA
myflowgage$TMWB$Qpred[1]=0
myflowgage$TMWB$S=NA
myflowgage$TMWB$S[1]=0
myflowgage$fcres=0.3  #typically ranges from 0.2-0.5
myflowgage$SlopeRad=0.0 

TMWBModel<-function(hru_list){  
  # hru_list is the same object we have been using till now to store all our
  # variables and parameters.
  myflowgage=hru_list
  attach(myflowgage)
  attach(TMWB)
  
  # Snow accumulation and melt, as well as PET only depend on the surface attributes, and as such, can run  at the beginning, independent of the daily calculated ET, TMWB, and the linear reservoir Storage Discharge (Qmm). 
  SNO_Energy=SnowMelt(mdate, Precip_mmpd, MaxTemp_C-3, MinTemp_C-3, myflowgage$declat, 
                      slope = 0, aspect = 0, tempHt = 1, windHt = 2, groundAlbedo = 0.25,
                      SurfEmissiv = 0.95, windSp = 2, forest = 0, startingSnowDepth_m = 0,
                      startingSnowDensity_kg_m3=450)
  
  SnowMelt_mm=SNO_Energy$SnowMelt_mm     
  SnowWaterEq_mm=SNO_Energy$SnowWaterEq_mm 
  SnowfallWatEq_mm=SNO_Energy$SnowfallWatEq_mm
  myflowgage$TMWB$SnowMelt_mm=SnowMelt_mm
  myflowgage$TMWB$SnowWaterEq_mm=SnowWaterEq_mm
  myflowgage$TMWB$SnowfallWatEq_mm=SnowfallWatEq_mm
  myflowgage$TMWB$Albedo[myflowgage$TMWB$SnowfallWatEq_mm>0]=.95
  PET=PET_fromTemp(Jday=(1+as.POSIXlt(mdate)$yday),Tmax_C=MaxTemp_C,Tmin_C = MinTemp_C, lat_radians = myflowgage$declat*pi/180) * 1000
  myflowgage$TMWB$PET=PET
  
  # Those processes that are dependant on prior days conditions, we run as a loop through each of the days.
  for (t in 2:length(AW)){
    ET[t] = min (AW[t-1],PET[t]*AW[t-1]/AWC[t-1]) 
    # Calculating Net Precipitation 
    dP[t] = Precip_mmpd[t] - SnowfallWatEq_mm[t] - ET[t] + SnowMelt_mm[t]
    # TMWB Solution
    if (dP[t]<=0) {
      values<-soildrying(AW[t-1],dP[t],AWC[t])
    } else if((dP[t]>0) & (AW[t-1]+dP[t])<=AWC[t]) {
      values<-soilwetting(AW[t-1],dP[t],AWC[t])
    } else{
      values <- soil_wetting_above_capacity(AW[t-1],dP[t],AWC[t])
    }
    AW[t]<-values[1] 
    ExcessOut[t]<-values[2] #this is essentially just runoff 
    if(Precip_mmpd[t]>0) {Drainage[t]<- Precip_mmpd[t] - ExcessOut[t] - ET[t]} #recharge equation from Shuler and Mariner 2020
    if(Drainage[t]<0){ Drainage[t]<- 0}
    S[t]=S[t-1]+ExcessOut[t] + Drainage[t]
    Qpred[t]=fcres*S[t]  #Q as calculated from TMWB model (seems to underestimate baseflow without adding in recharge component)
    S[t]<-S[t]-Qpred[t] 
    print(t)
  }
  
  # UPDATE all the calculated vectors for list to be returned from function
  # BEFORE DETACHING
  myflowgage$TMWB$SnowMelt_mm=SnowMelt_mm
  myflowgage$TMWB$SnowWaterEq_mm=SnowWaterEq_mm
  myflowgage$TMWB$SnowfallWatEq_mm=SnowfallWatEq_mm
  myflowgage$TMWB$Albedo[myflowgage$TMWB$SNO>0]=.95
  myflowgage$TMWB$dP=dP
  myflowgage$TMWB$AW=AW
  myflowgage$TMWB$ExcessOut=ExcessOut
  myflowgage$TMWB$Drainage=Drainage
  myflowgage$TMWB$S=S
  myflowgage$TMWB$PET=PET
  myflowgage$TMWB$ET=ET
  myflowgage$TMWB$Qpred=Qpred 
  detach(TMWB)
  detach(myflowgage)
  # Return the updated list.
  return(myflowgage)
}

# Call the new TMWBModel() function 
TMWBsol=TMWBModel(myflowgage)
# Convert area from km to m (10^6) and Qpred from mm to m (10^-3) 
TMWBsol$TMWB$Qpred_m3pd=TMWBsol$TMWB$Qpred*TMWBsol$area*10^3
# Convert Qpred_m3pd to Qpred_m3ps (1m3/s = 86400 m3/d)
TMWBsol$TMWB$Qpred_m3ps=TMWBsol$TMWB$Qpred_m3pd/86400

#plots to visualize data
plot(TMWBsol$TMWB$mdate,TMWBsol$TMWB$Qpred_m3pd,col="red", type='l')
plot(TMWBsol$TMWB$mdate,TMWBsol$TMWB$Qpred_m3ps,col="orange", type='l')
plot(TMWBsol$TMWB$mdate,TMWBsol$TMWB$ExcessOut,col="blue", type='l')
plot(TMWBsol$TMWB$mdate,TMWBsol$TMWB$S,col="green", type='l')
plot(TMWBsol$TMWB$mdate,TMWBsol$TMWB$Drainage,col="purple", type='l')

#create csv for q export
QExport<- data.frame("time"=TMWBsol$TMWB$mdate, "Q_BVR_m3.d"=TMWBsol$TMWB$Qpred_m3pd)

# because some of the NLDAS data filled in earlier years, merge this dataframe with the other file which HLW created
download.file('https://github.com/hlwander/bvr_glm/raw/master/inputs/BVR_flow_calcs_new.csv',
              './SCCData/bvre-data/BVR_flow_calcs_new.txt')
old <- read.csv('./SCCData/bvre-data/BVR_flow_calcs_new.txt')
old <- old[,-1]
old$time <- as.Date(old$time)
QExport <- QExport[QExport$time > max(old$time),]
QExport$time <- as.Date(QExport$time)
all <- rbind(old, QExport)
all$time <- as.Date(all$time)
plot(all$time, all$Q_BVR_m3.d)
write.csv(all, "./SCCData/bvre-data/BVR_flow_calcs_obs_met_2021-10-13.csv")

