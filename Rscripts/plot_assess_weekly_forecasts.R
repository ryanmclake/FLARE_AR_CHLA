
library(ggplot2)
library(dplyr)

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"

# define specs for the timestep and simulation
timestep <- '7day' # character definition of the timestep
timestep_numeric <- 7 # maybe timestep_numeric and timestep_interval are actually the same thing and not both needed -_-
timestep_interval <- 7 # the interval in between timesteps, e.g. 4day would be 4; daily would be 1; weekly would be 7
max_timestep <- 2 #maximum number of timesteps that can be propagated to the max time horizon
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 16)
sim_name <- 'weekly_dischargeforecast_Apr2020'
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/', sim_name)
bloom_threshold <- 17.1
setwd(forecast_folder)

# read in the individual forecast files named for the day on which the forecast is made
myfiles_forecast <- list.files(path = forecast_folder, pattern = paste0('*', timestep, ".csv"))
dataset_forecast <- read.csv(paste0(forecast_folder, "/", myfiles_forecast[1]))
dataset_forecast$timestep <- seq(1, max_timestep, by =1 )


# read in files
for (i in 2:length(myfiles_forecast)) {
  temp_2 <- read.csv(paste0(forecast_folder,"/", myfiles_forecast[i]))
  temp_2$timestep <-  seq(1, max_timestep, by =1 ) # this is the number of timesteps continued into the future, maybe should call this horizon?
  dataset_forecast <- rbind(dataset_forecast, temp_2)
}

# some data arranging
stuff <- dataset_forecast 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")

# repeat but for files which end in 'weekly' instead of '7day'
# read in the individual forecast files named for the day on which the forecast is made
myfiles_forecast2 <- list.files(path = forecast_folder, pattern = paste0('*', 'weekly', ".csv"))
dataset_forecast2 <- read.csv(paste0(forecast_folder, "/", myfiles_forecast2[1]))
dataset_forecast2$timestep <- seq(1, max_timestep, by =1 )

# read in files
for (i in 2:length(myfiles_forecast2)) {
  temp_2 <- read.csv(paste0(forecast_folder,"/", myfiles_forecast2[i]))
  temp_2$timestep <-  seq(1, max_timestep, by =1 ) # this is the number of timesteps continued into the future, maybe should call this horizon?
  dataset_forecast2 <- rbind(dataset_forecast2, temp_2)
}

# some data arranging
stuff2 <- dataset_forecast2 
stuff2$forecast_date <- as.Date(stuff2$forecast_date, "%Y-%m-%d")
stuff2 <- stuff2[order(stuff2$forecast_date),]
stuff2$day_in_future <- seq(timestep_numeric, max_horizon, by = timestep_interval)
stuff2$forecast_run_day <- as.Date(stuff2$forecast_run_day, "%Y-%m-%d")

# put stuff and stuff2 together
stuff2 <- stuff2 %>% select(-week)
stuff <- stuff %>% select(-day_in_future, day_in_future)
stuff <- rbind(stuff2, stuff)


# bring in the null model data
myfiles_null <- list.files(path = paste0(folder, '/FCR_forecasts/', timestep, '/null_weekly/'), pattern = paste0('*', 'null_summary', ".csv"))
dataset_null <- read.csv(paste0(paste0(folder, '/FCR_forecasts/', timestep, '/null_weekly/'), myfiles_null[1]))

for (i in 2:length(myfiles_null)) {
  temp <- read.csv(paste0(folder, '/FCR_forecasts/', timestep, '/null_weekly/', myfiles_null[i]))
  dataset_null <- rbind(dataset_null, temp)
  }
dataset_null$forecast_run_day <- as.Date(dataset_null$forecast_run_day)

#remove spin up dates, so anything before Dec 31, 2018
stuff <- stuff[stuff$forecast_run_day>'2018-12-31',]

# remove anything after 15Aug20
stuff <- stuff[stuff$forecast_run_day<'2020-08-16',]
dataset_null <- dataset_null[dataset_null$forecast_run_day<'2020-08-16',]


# separate into forecast horizon for individual analysis
for (i in 1:max_timestep) { 
  temp <- stuff[stuff$timestep==i,]
  write.csv(temp, paste0(forecast_folder, '/day_', i, '.csv'), row.names = FALSE)
}


  temp <- dataset_null[dataset_null$timestep==7,]
  write.csv(temp, paste0(folder, '/FCR_forecasts/', timestep, '/null_weekly' ,'/day_', 1, '_null.csv'), row.names = FALSE)
  temp <- dataset_null[dataset_null$timestep==14,]
  write.csv(temp, paste0(folder, '/FCR_forecasts/', timestep, '/null_weekly' ,'/day_', 2, '_null.csv'), row.names = FALSE)
  

########################################################################################################################################################################
# create table that null and forecast metrics will be written into
# each timestep has a different metrics table
metrics <- array(NA, dim = c(8, 4))
row.names(metrics) <- c('RMSE', "NSE", 'KGE', 'bias', 'bias_sd', 'R2_1_1', 'coefficient of determination', 'corr_p')
colnames(metrics) <- c('forecast', 'null', 'forecast_nonbloom', 'null_nonbloom')

metrics_overtime <- array(NA, dim = c(max_timestep,13))
colnames(metrics_overtime) <- c('day_in_future','RMSE_null', 'RMSE_forecast',"RMSE_null_nonbloom", 'RMSE_forecast_nonbloom', 
                                'R2_null', 'R2_forecast',"R2_null_nonbloom", 'R2_forecast_nonbloom', 
                                'RMSE_null_bloom', 'RMSE_forecast_bloom', 'R2_null_bloom', 'R2_forecasT_bloom')
for (i in 1:max_timestep) {
  # read in csv of each forecast horizon
  temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
  temp$forecast_date <- as.Date(temp$forecast_date)
  temp$forecast_run_day <- as.Date(temp$forecast_run_day)
  temp <- na.omit(temp)
  
  temp_null <- read.csv(paste0(folder, '/FCR_forecasts/', timestep, '/null_weekly', '/day_', i, '_null.csv'))
  temp_null$forecast_run_day <- as.Date(temp_null$forecast_run_day)
  temp_null <- na.omit(temp_null)
  
  temp <- temp[temp$forecast_run_day<=max(temp_null$forecast_run_day),]
  temp <- left_join(temp, temp_null, by = 'forecast_run_day')
  
  obs <- read.csv(paste0(folder, '/obs_chla_02Jan2019_15Aug2020.csv'))
  obs$forecast_date <- as.Date(obs$forecast_date)
  obs$forecast_run_day <- as.Date(obs$forecast_run_day)
  temp <- left_join(temp, obs, by = 'forecast_date')
  temp <- temp %>% select(forecast_date, forecast_mean_chl, obs_chl_EXO, mean, obs_chl_EXO_on_forecast_date, forecast_run_day.x )
  
  # calculate forecast metrics
  source(paste0(folder,"/","Rscripts/model_assessment.R")) # sim, obs
  
  forecast <- model_metrics(temp$forecast_mean_chl, temp$obs_chl_EXO_on_forecast_date)
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
  
  null <- model_metrics(temp$mean, temp$obs_chl_EXO_on_forecast_date)
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
  
  
  non_bloom <- temp[temp$obs_chl_EXO_on_forecast_date < bloom_threshold, ]
  non_bloom_forecast <- model_metrics(non_bloom$forecast_mean_chl, non_bloom$obs_chl_EXO_on_forecast_date)
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
  
  bloom <- temp[temp$obs_chl_EXO_on_forecast_date > bloom_threshold, ]
  bloom_forecast <- model_metrics(bloom$forecast_mean_chl, bloom$obs_chl_EXO_on_forecast_date)
  metrics_overtime[i,11] <- bloom_forecast$RMSE
  
  
  non_bloom_null <- model_metrics(non_bloom$mean, non_bloom$obs_chl_EXO_on_forecast_date)
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
  
  bloom_null <- model_metrics(bloom$mean, bloom$obs_chl_EXO_on_forecast_date)
  metrics_overtime[i,10] <- bloom_null$RMSE
  
  
  print(paste0('metrics on timestep ', i))
  print(metrics)
  #write.csv(metrics, paste0(forecast_folder, '/ForecastMetrics_day_', i, '.csv'))
  
}



metrics_overtime[,1] <-   seq(timestep_numeric, max_horizon, by = timestep_interval)
metrics_overtime <- as.data.frame(metrics_overtime)
write.csv(metrics_overtime, paste0(folder, '/FCR_forecasts/', timestep, '/null_weekly/ForecastMetrics_', timestep, '.csv'), row.names = FALSE)


#################################################################################################################################################################################
#################################################################################################################################################################################
# plotting

# loop to make figures at each time step
for(i in 1:max_timestep){
  
  # read in csv of each forecast horizon
  temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
  temp$forecast_date <- as.Date(temp$forecast_date)
  temp$forecast_run_day <- as.Date(temp$forecast_run_day)

  #create and save figure with forecast mean, confidence intervals, and obs chl
  png(paste0(forecast_folder, '/', timestep, '_Forecast_Timestep', i, '_moving_y_axis.png'), width = 1100, height = 800)
  print(ggplot(temp, aes(forecast_date, forecast_mean_chl)) +
    geom_line(size = 2) +
      geom_text(x = as.Date('2020-06-10'), y = max(temp$obs_chl_EXO, na.rm = TRUE), size = 15, label = paste0('RMSE = ', round(metrics_overtime[i,3], digits = 1))) + 
      geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'green4') +
    geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
    xlab('Date') +
    ylab('Chlorophyll a (μg/L)') +
    ggtitle(paste0('Weekly Forecast, Day ', i*7)) +
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

# loop to make figures at each time step
# FIGURES WITH Y AXIS SET
for(i in 1:max_timestep){
  
  # read in csv of each forecast horizon
  temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
  temp$forecast_date <- as.Date(temp$forecast_date)
  
  #create and save figure with forecast mean, confidence intervals, and obs chl

  #########################################################################
  ##### edit this file name so the naming is correct for the timestep #####
  #########################################################################
  png(paste0(forecast_folder, '/', timestep, '_Forecast_Day', i, '.png'), width = 1100, height = 800)
  print(ggplot(temp, aes(forecast_date, forecast_mean_chl)) +
          ylim(0,150) +
          geom_line(size = 2) +
          geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'green4') +
          geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
          xlab('Date') +
          ylab('Chlorophyll a (μg/L)') +
        #####################################################################
        ##### edit this title so the naming is correct for the timestep #####
        #####################################################################
        ggtitle(paste0('Weekly Forecast, Day ', i*7)) +
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


