
library(ggplot2)
library(patchwork)
library(scales)

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
timestep <- '4days'
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/4day_timestep_4day_lag')

setwd(forecast_folder)

# read in the individual forecast files named for the day on which the forecast is made
myfiles_forecast <- list.files(path = forecast_folder, pattern = paste0('*', timestep, ".csv"))
dataset_forecast <- read.csv(paste0(forecast_folder, "/", myfiles_forecast[1]))
dataset_forecast$timestep <- c(1,2,3)


# read in files
for (i in 2:length(myfiles_forecast)) {
  temp_2 <- read.csv(paste0(forecast_folder,"/", myfiles_forecast[i]))
  temp_2$timestep <- c(1,2,3)
  dataset_forecast <- rbind(dataset_forecast, temp_2)
}

# some data arranging
stuff <- dataset_forecast 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$day_in_future)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")

dates <- unique(stuff$forecast_run_day)

# bring in the null model data
myfiles_null <- list.files(path = paste0(forecast_folder, '/null_ensemble'), pattern = paste0('*', 'null_summary', ".csv"))
dataset_null <- read.csv(paste0(paste0(forecast_folder, '/null_ensemble'), "/", myfiles_null[1]))
# shit I forgot to archive the forecast run day within the null model file
dataset_null$forecast_run_day <- NA
dataset_null$day_in_future <- NA
dataset_null$timestep <- NA

# read in files
# V1 = mean of the ensembles
# V2 = upper 95% CI
# V3 = lower 95% CI
for (i in 2:length(myfiles_null)) {
  temp <- read.csv(paste0(forecast_folder, '/null_ensemble',"/", myfiles_null[i]))
  temp$day_in_future <- c(0,4,8,12)
  temp$timestep <- c(0,1,2,3)
  temp$forecast_run_day <- dates[i]
  dataset_null <- rbind(dataset_null, temp)
  }

# fill in the first file's new columns
dataset_null$forecast_run_day[1:4] <- as.Date('2018-08-15')
dataset_null$forecast_run_day <- as.Date(dataset_null$forecast_run_day, '1970-01-01')
dataset_null$day_in_future[1:4] <- c(0,4,8,12)
dataset_null$timestep[1:4] <- c(0,1,2,3)

#remove spin up dates, so anything before Dec 31, 2018
stuff <- stuff[stuff$forecast_run_day>'2018-12-31',]
dataset_null <- dataset_null[dataset_null$forecast_run_day>'2018-12-31',]


# separate into forecast horizon for individual analysis
for (i in 1:3) { # change the 16 to whatever number of timesteps you have
  temp <- stuff[stuff$timestep==i,]
  write.csv(temp, paste0(forecast_folder, '/day_', i, '.csv'), row.names = FALSE)
}

for (i in 0:3) { # change the 16 to whatever number of timesteps you have
  temp <- dataset_null[dataset_null$timestep==i,]
  write.csv(temp, paste0(forecast_folder, '/day_', i, '_null.csv'), row.names = FALSE)
}


# loop to make figures at each time step
for(i in 1:3){
  
  # read in csv of each forecast horizon
  temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
  temp$forecast_date <- as.Date(temp$forecast_date)
  temp$forecast_run_day <- as.Date(temp$forecast_run_day)

  #create and save figure with forecast mean, confidence intervals, and obs chl
  #png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Fig3_forecast_timeseries/Daily_Forecast_Day', i, 'moving_y_axis.png'), width = 1100, height = 800)
  png(paste0(forecast_folder, '/Daily_Forecast_Day', i, 'moving_y_axis.png'), width = 1100, height = 800)
  print(ggplot(temp, aes(forecast_date, forecast_mean_chl)) +
    geom_line(size = 2) +
    geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'dodgerblue4') +
    geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
    xlab('Date') +
    ylab('Chlorophyll a (μg/L)') +
    ggtitle(paste0('Daily Forecast, Day ', i)) +
    scale_x_date(labels = date_format('%b')) +
    theme_bw() +
    theme(axis.text.x = element_text(size = 45),
          axis.text.y = element_text(size = 50),
          axis.title.x = element_text(size =45),
          axis.title.y = element_text(size = 45),
          legend.title = element_text(size = 35),
          legend.text = element_text(size = 30),
          panel.grid.major = element_blank(),
          legend.position = 'right',
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 40)))
 dev.off()
}

# loop to make figures at each time step
# FIGURES WITH Y AXIS SET
for(i in 1:3){
  
  # read in csv of each forecast horizon
  temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
  temp$forecast_date <- as.Date(temp$forecast_date)
  
  #create and save figure with forecast mean, confidence intervals, and obs chl
  #png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Fig3_forecast_timeseries/Daily_Forecast_Day', i, '.png'), width = 1100, height = 800)
  png(paste0(forecast_folder, '/Daily_Forecast_Day', i, '.png'), width = 1100, height = 800)
  print(ggplot(temp, aes(forecast_date, forecast_mean_chl)) +
          ylim(0,150) +
          geom_line(size = 2) +
          geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'dodgerblue4') +
          geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
          xlab('Date') +
          ylab('Chlorophyll a (μg/L)') +
          ggtitle(paste0('Daily Forecast, Day ', i)) +
          scale_x_date(labels = date_format('%b')) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 45),
                axis.text.y = element_text(size = 50),
                axis.title.x = element_text(size =45),
                axis.title.y = element_text(size = 45),
                legend.title = element_text(size = 35),
                legend.text = element_text(size = 30),
                panel.grid.major = element_blank(),
                legend.position = 'right',
                panel.grid.minor = element_blank(),
                plot.title = element_text(size = 50)))
  dev.off()
}



########################################################################################################################################################################
# create table that null and forecast metrics will be written into
metrics <- array(NA, dim = c(8, 4))
row.names(metrics) <- c('RMSE', "NSE", 'KGE', 'bias', 'bias_sd', 'R2_1_1', 'coefficient of determination', 'corr_p')
colnames(metrics) <- c('forecast', 'null', 'forecast_nonbloom', 'null_nonbloom')

metrics_overtime <- array(NA, dim = c(3,13))
colnames(metrics_overtime) <- c('day_in_future','RMSE_null', 'RMSE_forecast',"RMSE_null_nonbloom", 'RMSE_forecast_nonbloom', 
                                'R2_null', 'R2_forecast',"R2_null_nonbloom", 'R2_forecast_nonbloom', 
                                'RMSE_null_bloom', 'RMSE_forecast_bloom', 'R2_null_bloom', 'R2_forecasT_bloom')
for (i in 1:3) {
  # read in csv of each forecast horizon
  temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
  temp$forecast_date <- as.Date(temp$forecast_date)
  temp$forecast_run_day <- as.Date(temp$forecast_run_day)

  temp_null <- read.csv(paste0(forecast_folder, '/day_', i, '_null.csv'))
  temp_null$forecast_run_day <- as.Date(temp_null$forecast_run_day)
  
  temp <- temp[temp$forecast_run_day<=max(temp_null$forecast_run_day),]

  # calculate forecast metrics
  source(paste0(folder,"/","Rscripts/model_assessment.R")) # sim, obs
  
  forecast <- model_metrics(temp$forecast_mean_chl, temp$obs_chl_EXO)
  metrics[1,1] <- forecast$RMSE
  metrics[2,1] <- forecast$NSE
  metrics[3,1] <- forecast$KGE
  metrics[4,1] <- forecast$bias
  metrics[5,1] <- forecast$bias_sd
  metrics[6,1] <- forecast$R2_1_1
  metrics[7,1] <- forecast$coeff_determination
  metrics[8,1] <- forecast$cor_p
  metrics_overtime[i,3] <- forecast$RMSE
  metrics_overtime[i,7] <- forecast$coeff_determination
  
  null <- model_metrics(temp_null$mean, temp$obs_chl_EXO)
  metrics[1,2] <- null$RMSE
  metrics[2,2] <- null$NSE
  metrics[3,2] <- null$KGE
  metrics[4,2] <- null$bias
  metrics[5,2] <- null$bias_sd
  metrics[6,2] <- null$R2_1_1
  metrics[7,2] <- null$coeff_determination
  metrics[8,2] <- null$cor_p
  metrics_overtime[i,2] <- null$RMSE
  metrics_overtime[i,6] <- null$coeff_determination
  metrics_overtime[i,1] <- i
  
  non_bloom <- temp[temp$forecast_date< as.Date("2019-07-17") | temp$forecast_date > as.Date("2019-08-05"), ]
  non_bloom_forecast <- model_metrics(non_bloom$forecast_mean_chl, non_bloom$obs_chl_EXO)
  metrics[1,3] <- non_bloom_forecast$RMSE
  metrics[2,3] <- non_bloom_forecast$NSE
  metrics[3,3] <- non_bloom_forecast$KGE
  metrics[4,3] <- non_bloom_forecast$bias
  metrics[5,3] <- non_bloom_forecast$bias_sd
  metrics[6,3] <- non_bloom_forecast$R2_1_1
  metrics[7,3] <- non_bloom_forecast$coeff_determination
  metrics[8,3] <- non_bloom_forecast$cor_p
  metrics_overtime[i,5] <- non_bloom_forecast$RMSE
  metrics_overtime[i,9] <- non_bloom_forecast$coeff_determination

  bloom <- temp[temp$forecast_date> as.Date("2019-07-17") & temp$forecast_date < as.Date("2019-08-05"), ]
  bloom_forecast <- model_metrics(bloom$forecast_mean_chl, bloom$obs_chl_EXO)
  metrics_overtime[i,11] <- bloom_forecast$RMSE
  
  
  datamerge_nonbloom <- temp_null[temp_null$forecast_run_day< as.Date("2019-07-17") | temp_null$forecast_run_day > as.Date("2019-08-05"), ]
  non_bloom_null <- model_metrics(datamerge_nonbloom$mean, non_bloom$obs_chl_EXO)
  metrics[1,4] <- non_bloom_null$RMSE
  metrics[2,4] <- non_bloom_null$NSE
  metrics[3,4] <- non_bloom_null$KGE
  metrics[4,4] <- non_bloom_null$bias
  metrics[5,4] <- non_bloom_null$bias_sd
  metrics[6,4] <- non_bloom_null$R2_1_1
  metrics[7,4] <- non_bloom_null$coeff_determination
  metrics[8,4] <- non_bloom_null$cor_p
  metrics_overtime[i,4] <- non_bloom_null$RMSE
  metrics_overtime[i,8] <- non_bloom_null$coeff_determination
  
  datamerge_bloom <- temp_null[temp_null$forecast_run_day> as.Date("2019-07-17") &  temp_null$forecast_run_day < as.Date("2019-08-05"), ]
  bloom_null <- model_metrics(datamerge_bloom$mean, bloom$obs_chl_EXO)
  metrics_overtime[i,10] <- bloom_null$RMSE
  
  
  print(paste0('metrics on day ', i))
  print(metrics)
  #write.csv(metrics, paste0(forecast_folder, '/ForecastMetrics_day_', i, '.csv'))
  
}


metrics_overtime <- as.data.frame(metrics_overtime)
write.csv(metrics_overtime, paste0(forecast_folder, '/ForecastMetrics_4day.csv'), row.names = FALSE)

# a messy plot with different conditions on one plot
plot(metrics_overtime$day_in_future, metrics_overtime$RMSE_null, col = 'red', ylim = c(0,11), xlab = 'forecast horizon', ylab = 'RMSE')
points(metrics_overtime$day_in_future, metrics_overtime$RMSE_forecast, col = 'blue')
points(metrics_overtime$day_in_future, metrics_overtime$RMSE_forecast_nonbloom, col = 'orange')
points(metrics_overtime$day_in_future, metrics_overtime$RMSE_null_nonbloom, col = 'purple')
legend('topleft', c('null', 'forecast', 'null nonbloom', 'forecast nonbloom'), col = c('red', 'blue', 'purple', 'orange'), lty = c(1,1), bty = 'n')


ggplot(data = metrics_overtime, aes(x = day_in_future,y = RMSE_null)) + 
  geom_point(aes(day_in_future, RMSE_null), col = 'red', size = 15) +
  geom_point(aes(day_in_future, RMSE_forecast), col = 'blue', size = 15)+
  xlab('Weeks into the future') + 
  ylab('RMSE (ug/L)') +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right') +
  scale_x_discrete(limits = c('1','2')) 

ggplot(data = metrics_overtime, aes(x = day_in_future,y = RMSE_null)) + 
  geom_point(aes(day_in_future, RMSE_forecast_nonbloom), col = 'orange', size = 15) +
  geom_point(aes(day_in_future, RMSE_forecast_bloom), col = 'red', size = 15) +
  geom_point(aes(day_in_future, RMSE_forecast), col = 'blue', size = 15)+
  xlab('Weeks into the future') + 
  ylab('RMSE (ug/L)') +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'right') +
  scale_x_discrete(limits = c('1','2')) +
  scale_color_manual(labels = c('Total', 'bloom', 'nonbloom'), 
                     breaks = c('blue', 'red', 'orange'),
                     values = c('blue', 'red', 'orange'))


metrics_RMSE <- metrics_overtime[,c(1,3,2, 5, 4, 20,21,10,14,12,16,18,19)] #select the values wanted for manuscript figure in the right order
metrics_RMSE <- metrics_RMSE[1:14,]
write.csv(metrics_RMSE, paste0(forecast_folder, '/FCR_forecasts/metrics_RMSE.csv'), row.names = FALSE)
################################################################################################################################################################
#### calculate ensemble metrics ########################################################################################################################################
################################################################################################################################################################

# smaller score corresponds to a better forecast

library(scoringRules)

# read in forecast distribution and observed data
ensemble_forecast <- list.files(path = forecast_folder, pattern = paste0('*', '_chla_daily_ensembles', ".csv"))
ensemble_forecast <- ensemble_forecast[-c(1:9)] # start the list on 01-01-2019
obs_file <- list.files(path = forecast_folder, pattern = paste0('day_', '*', '.csv'))
nsteps <- 16
crps_forecast <- array(NA, dim = c(length(ensemble_forecast), 2, nsteps))


for (i in 1:length(ensemble_forecast)) {
  temp <- read.csv(paste0(forecast_folder,'/', ensemble_forecast[i]))
  obs <- read.csv(paste0(forecast_folder, '/day_', 1, '.csv'))
  obs_chl <- obs$obs_chl_EXO[i]
  
  for (j in 1:16) {
    dat <- as.numeric(temp[j,])
    crps_forecast[i,1,j] <- crps_sample(y = obs_chl, dat = dat)  
  }
  
}



for (i in 1:16) {
  x <- mean(crps_forecast[,1,i], na.rm = TRUE)
  print(paste0('forecast crps on day ', i,' ',  x))
}

for (i in 1:16) {
  x <- mean(crps_forecast[,1,1] - crps_forecast[,1,i])
  print(paste0('diff btw forecast crps on day 1 and ', i,' ',  x))
}


for (i in 1:16) {
  hist(as.numeric(temp[i,]), main = paste0('hist of  ensembles on forecast day ', i))
  
}

for(i in 1:16){
  plot(crps_forecast[,1,i], type ='l', main = paste0('CRPS on forecast day ', i))
  
}


########################################################################################
# and the null ensemble metrics 
########################################################################################

ensemble_null <- list.files(path = paste0(forecast_folder, '/null_ensemble'), pattern = paste0('*', '_null_ensembles', '.csv'))
ensemble_null <- ensemble_null[-c(1:9)]
crps_null <- array(NA, dim = c(length(ensemble_null), 2, nsteps))

for (i in 1:length(ensemble_null)) {
  temp <- read.csv(paste0(forecast_folder,'/null_ensemble/', ensemble_null[i]))
  
  for (j in 1:16) {
    obs <- read.csv(paste0(forecast_folder, '/day_', j, '.csv'))
    obs_chl <- obs$obs_chl_EXO[i]
    dat <- as.numeric(temp[j,])
    crps_null[i,1,j] <- crps_sample(y = obs_chl, dat = dat)  
  }
  
}

plot(crps_null[,1,1], type = 'l')

for (i in 1:16) {
  x <- mean(crps_null[,1,i], na.rm = TRUE)
  print(paste0('null crps on day ', i,' ',  x))
}



