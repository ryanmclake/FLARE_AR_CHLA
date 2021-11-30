###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 11 ----
reference_tzone <- "GMT"

timestep <- '1day'
sim_name <- 'Mar2021_UC'
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/', sim_name)

# code to read in the individual forecast files named for the day on which the forecast is made
myfiles <- list.files(path = forecast_folder, pattern = "*ensemble_parameters.csv")
dataset <- read.csv(paste0(forecast_folder,'/', myfiles[1]))

# read in files
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0(forecast_folder, "/", myfiles[i]))
  dataset <- rbind(dataset, temp)
}

colnames(dataset) <- c('latent_chl', 'intercept', 'chl_parm', 'discharge_parm', 'sw_parm', 'error_term')

## these plots are of all the ensemble members and aren't really that useful
#plot(dataset$intercept)
#plot(dataset$chl_parm)
#plot(dataset$rh_parm)
#plot(dataset$discharge_parm)
#plot(dataset$error_term)


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
stuff$day_in_future <- as.factor(stuff$day_in_future)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")

parms <- stuff[!duplicated(stuff$forecast_run_day),]

plot(parms$forecast_date, parms$par1, type = 'l', ylab = 'Intercept')
plot(parms$forecast_date, parms$par2, type = 'l', ylab = 'Chl parm')
plot(parms$forecast_date, parms$par3, type = 'l', ylab = 'Discharge parm')
plot(parms$forecast_date, parms$par4, type = 'l', ylab = 'Shortwave parm')
plot(parms$forecast_date, parms$par5, type = 'l', ylab = 'Error term')

parms <- parms[parms$forecast_run_day>'2018-12-31',]

xmin<-min(parms$forecast_date,na.rm=T)
xmax<-max(parms$forecast_date,na.rm=T) 
xseq<-seq.Date(xmin,xmax,by='1 month')


png(paste0(forecast_folder, '/daily_parameter_timeseries_Feb21.png'), width = 1100, height = 800)
par(mfrow = c(2,3), mar=c(9,7,4,1)+.1)
plot(parms$forecast_run_day, parms$par1, xlab = '', ylab = 'Intercept par', type = 'l', axes = F, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
rect(as.Date("2019-02-28", "%Y-%m-%d"), min(parms$par1), as.Date("2019-03-24", "%Y-%m-%d"), max(parms$par1),col = 'palegreen2' , border = NA)
rect(as.Date("2019-07-15", "%Y-%m-%d"), min(parms$par1), as.Date("2019-08-05", "%Y-%m-%d"), max(parms$par1),col = 'skyblue1' , border = NA)
rect(as.Date("2020-03-16", "%Y-%m-%d"), min(parms$par1), as.Date("2020-04-23", "%Y-%m-%d"), max(parms$par1),col = 'mediumpurple2', border = NA)
points(parms$forecast_run_day, parms$par1, type = 'l')

plot(parms$forecast_run_day, parms$par2, xlab = '', type = 'l', ylab = 'Chlorophyll par', axes = F,cex.lab = 2, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
rect(as.Date("2019-02-28", "%Y-%m-%d"), min(parms$par2), as.Date("2019-03-24", "%Y-%m-%d"), max(parms$par2),col = 'palegreen2' , border = NA)
rect(as.Date("2019-07-15", "%Y-%m-%d"), min(parms$par2), as.Date("2019-08-05", "%Y-%m-%d"), max(parms$par2),col = 'skyblue1' , border = NA)
rect(as.Date("2020-03-16", "%Y-%m-%d"), min(parms$par2), as.Date("2020-04-23", "%Y-%m-%d"), max(parms$par2),col = 'mediumpurple2', border = NA)
points(parms$forecast_run_day, parms$par2, type = 'l')

plot(parms$forecast_run_day, parms$par3, xlab = '', ylab = 'Discharge par', type = 'l', cex.lab = 2, axes = F,cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
rect(as.Date("2019-02-28", "%Y-%m-%d"), min(parms$par3), as.Date("2019-03-24", "%Y-%m-%d"), max(parms$par3),col = 'palegreen2' , border = NA)
rect(as.Date("2019-07-15", "%Y-%m-%d"), min(parms$par3), as.Date("2019-08-05", "%Y-%m-%d"), max(parms$par3),col = 'skyblue1' , border = NA)
rect(as.Date("2020-03-16", "%Y-%m-%d"), min(parms$par3), as.Date("2020-04-23", "%Y-%m-%d"), max(parms$par3),col = 'mediumpurple2', border = NA)
points(parms$forecast_run_day, parms$par3, type = 'l')

plot(parms$forecast_run_day, parms$par4,  xlab = '', type = 'l', ylab = 'Shortwave par',  cex.lab = 2, axes = F, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
rect(as.Date("2019-02-28", "%Y-%m-%d"), min(parms$par4), as.Date("2019-03-24", "%Y-%m-%d"), max(parms$par4),col = 'palegreen2' , border = NA)
rect(as.Date("2019-07-15", "%Y-%m-%d"), min(parms$par4), as.Date("2019-08-05", "%Y-%m-%d"), max(parms$par4),col = 'skyblue1' , border = NA)
rect(as.Date("2020-03-16", "%Y-%m-%d"), min(parms$par4), as.Date("2020-04-23", "%Y-%m-%d"), max(parms$par4),col = 'mediumpurple2', border = NA)
points(parms$forecast_run_day, parms$par4, type = 'l')


plot(parms$forecast_run_day, parms$par5,  xlab = '', type = 'l', ylab = 'Error term',  cex.lab = 2, axes = F, cex.lab = 3)
axis(2, cex.axis = 3, cex.lab = 5)
axis.Date(side=1,at=xseq,format='%y-%b',labels=T,las=3, cex.axis = 3)
rect(as.Date("2019-02-28", "%Y-%m-%d"), min(parms$par5), as.Date("2019-03-24", "%Y-%m-%d"), max(parms$par5),col = 'palegreen2' , border = NA)
rect(as.Date("2019-07-15", "%Y-%m-%d"), min(parms$par5), as.Date("2019-08-05", "%Y-%m-%d"), max(parms$par5),col = 'skyblue1' , border = NA)
rect(as.Date("2020-03-16", "%Y-%m-%d"), min(parms$par5), as.Date("2020-04-23", "%Y-%m-%d"), max(parms$par5),col = 'mediumpurple2', border = NA)
points(parms$forecast_run_day, parms$par5, type = 'l')


dev.off()


(max(dataset_forecast$par1) - min(dataset_forecast$par1))/mean(dataset_forecast$par1)*100
(max(dataset_forecast$par2) - min(dataset_forecast$par2))/mean(dataset_forecast$par2)*100
(max(dataset_forecast$par3) - min(dataset_forecast$par3))/mean(dataset_forecast$par3)*100
(max(dataset_forecast$par4) - min(dataset_forecast$par4))/mean(dataset_forecast$par4)*100
(max(dataset_forecast$par5) - min(dataset_forecast$par5))/mean(dataset_forecast$par5)*100
