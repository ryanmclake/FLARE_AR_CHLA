

library(rsq)
library(tidyverse)
library(lubridate)
library(Metrics)
reference_tzone <- "GMT"

#set the location of the forecasts
folder <- "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3"
forecast_folder <- "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3"
setwd(forecast_folder)
# code to read in the individual forecast files named for the day on which the forecast is made
myfiles <- list.files(path = "./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019", pattern = "*ensemble_parameters.csv")
dataset <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/", myfiles[1]))

# read in files
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/", myfiles[i]))
  dataset <- rbind(dataset, temp)
}

colnames(dataset) <- c('intercept', 'chl_parm', 'discharge_parm', 'sw_parm', 'error_term')

plot(dataset$intercept)
plot(dataset$chl_parm)
plot(dataset$sw_parm)
plot(dataset$discharge_parm)
plot(dataset$error_term)


myfiles_forecast <- list.files(path = "./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019", pattern = "*weekly.csv")
dataset_forecast <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/", myfiles_forecast[1]))

# read in files
for (i in 2:length(myfiles_forecast)) {
  temp_2 <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/", myfiles_forecast[i]))
  dataset_forecast <- rbind(dataset_forecast, temp_2)
}

dataset_forecast$forecast_date <- as.Date(dataset_forecast$forecast_date)
# some data arranging
stuff <- dataset_forecast 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$week)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")

# separate into week1 and week2 dataframes for separate analysis
week1 <- stuff[stuff$week==1,]
week2 <- stuff[stuff$week==2,]
week1 <- na.omit(week1)
week2 <- na.omit(week2)

xmin<-min(week1$forecast_date,na.rm=T)
xmax<-max(week1$forecast_date,na.rm=T) 
xseq<-seq.Date(xmin,xmax,by='1 month')

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/paramer_timeseries.png', width = 1100, height = 800)
par(mfrow = c(2,3), mar=c(9,7,4,1)+.1)
plot(week1$forecast_date, week1$par1, xlab = '', ylab = 'intercept parm', type = 'l', axes = F, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
abline(v = as.Date("2019-07-15", "%Y-%m-%d"), col = 'blue')
abline(v = as.Date("2019-08-15", "%Y-%m-%d"), col = 'blue')
plot(week1$forecast_date, week1$par2, xlab = '', type = 'l', ylab = 'chlorophyll parm', axes = F,cex.lab = 2, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
abline(v = as.Date("2019-07-15", "%Y-%m-%d"), col = 'blue')
abline(v = as.Date("2019-08-15", "%Y-%m-%d"), col = 'blue')
plot(week1$forecast_date, week1$par3, xlab = '', ylab = 'discharge parm', type = 'l', cex.lab = 2, axes = F,cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
abline(v = as.Date("2019-07-15", "%Y-%m-%d"), col = 'blue')
abline(v = as.Date("2019-08-15", "%Y-%m-%d"), col = 'blue')
plot(week1$forecast_date, week1$par4, xlab = '', ylab = 'sw parm', type = 'l', cex.lab = 2, axes = F,cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
abline(v = as.Date("2019-07-15", "%Y-%m-%d"), col = 'blue')
abline(v = as.Date("2019-08-15", "%Y-%m-%d"), col = 'blue')
plot(week1$forecast_date, week1$par5, xlab = '', type = 'l', ylab = 'error term',  cex.lab = 2, axes = F, cex.lab = 3)
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
