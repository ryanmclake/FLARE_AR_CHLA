

library(rsq)
library(tidyverse)
library(lubridate)
library(Metrics)
reference_tzone <- "GMT"

#set the location of the forecasts
folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
timestep <- 'daily'
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep)
setwd(forecast_folder)

# code to read in the individual forecast files named for the day on which the forecast is made
myfiles <- list.files(path = forecast_folder, pattern = "*ensemble_parameters.csv")
dataset <- read.csv(paste0(forecast_folder,'/', myfiles[1]))

# read in files
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0(forecast_folder, "/", myfiles[i]))
  dataset <- rbind(dataset, temp)
}

# must change these based on the model you are using
colnames(dataset) <- c('intercept', 'chl_parm',  'sw_parm', 'error_term')

# these plots are of all the ensemble members and aren't really that useful
plot(dataset$intercept)
plot(dataset$chl_parm)
plot(dataset$sw_parm)
plot(dataset$discharge_parm)
plot(dataset$error_term)


myfiles_forecast <- list.files(path = forecast_folder, pattern = paste0('*', timestep, ".csv"))
dataset_forecast <- read.csv(paste0(forecast_folder, "/", myfiles_forecast[1]))

# read in files
for (i in 2:length(myfiles_forecast)) {
  temp_2 <- read.csv(paste0(forecast_folder,"/", myfiles_forecast[i]))
  dataset_forecast <- rbind(dataset_forecast, temp_2)
}

dataset_forecast$forecast_date <- as.Date(dataset_forecast$forecast_date)
# some data arranging
stuff <- dataset_forecast 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$week)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")

parms <- stuff[!duplicated(stuff$forecast_run_day),]
plot(parms$forecast_date, parms$par1, type = 'l', ylab = 'Intercept')
plot(parms$forecast_date, parms$par2, type = 'l', ylab = 'Chl parm')
plot(parms$forecast_date, parms$par3, type = 'l', ylab = 'Shortwave parm')
plot(parms$forecast_date, parms$par4, type = 'l', ylab = 'error term')

# separate into forecast horizon for individual analysis
for (i in 1:16) { # change the 16 to whatever number of timesteps you have
  temp <- stuff[stuff$day_in_future==i,]
  temp <- na.omit(temp)
  write.csv(temp, paste0(forecast_folder, '/day_', i, '.csv'), row.names = FALSE)
  }



xmin<-min(parms$forecast_date,na.rm=T)
xmax<-max(parms$forecast_date,na.rm=T) 
xseq<-seq.Date(xmin,xmax,by='1 month')

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/daily_parameter_timeseries.png', width = 1100, height = 800)
par(mfrow = c(2,2), mar=c(9,7,4,1)+.1)
plot(parms$forecast_date, parms$par1, xlab = '', ylab = 'intercept parm', type = 'l', axes = F, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
abline(v = as.Date("2019-07-15", "%Y-%m-%d"), col = 'blue')
abline(v = as.Date("2019-08-15", "%Y-%m-%d"), col = 'blue')
plot(parms$forecast_date, parms$par2, xlab = '', type = 'l', ylab = 'chlorophyll parm', axes = F,cex.lab = 2, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
abline(v = as.Date("2019-07-15", "%Y-%m-%d"), col = 'blue')
abline(v = as.Date("2019-08-15", "%Y-%m-%d"), col = 'blue')
plot(parms$forecast_date, parms$par3, xlab = '', ylab = 'sw parm', type = 'l', cex.lab = 2, axes = F,cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
abline(v = as.Date("2019-07-15", "%Y-%m-%d"), col = 'blue')
abline(v = as.Date("2019-08-15", "%Y-%m-%d"), col = 'blue')
plot(parms$forecast_date, parms$par4,  xlab = '', type = 'l', ylab = 'error term',  cex.lab = 2, axes = F, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
abline(v = as.Date("2019-07-15", "%Y-%m-%d"), col = 'blue')
abline(v = as.Date("2019-08-15", "%Y-%m-%d"), col = 'blue')

dev.off()

(max(dataset_forecast$par1) - min(dataset_forecast$par1))/mean(dataset_forecast$par1)*100
(max(dataset_forecast$par2) - min(dataset_forecast$par2))/mean(dataset_forecast$par2)*100
(max(dataset_forecast$par3) - min(dataset_forecast$par3))/mean(dataset_forecast$par3)*100
(max(dataset_forecast$par4) - min(dataset_forecast$par4))/mean(dataset_forecast$par4)*100
(max(dataset_forecast$par5) - min(dataset_forecast$par5))/mean(dataset_forecast$par5)*100
