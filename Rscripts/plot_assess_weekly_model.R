
library(rsq)
library(tidyverse)
library(lubridate)
library(Metrics)
library(hydroGOF)
reference_tzone <- "GMT"

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
timestep <- 'weekly' #specify the timestep
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/weekly_dischargeforecast_Apr2020') #specify the folder within here
setwd(forecast_folder)

# code to read in the individual forecast files named for the day on which the forecast is made
myfiles_forecast <- list.files(path = paste0(forecast_folder), pattern = paste0('*', timestep, ".csv"))
dataset_forecast <- read.csv(paste0(forecast_folder, '/', myfiles_forecast[1]))

# read in files
for (i in 2:length(myfiles_forecast)) {
  temp_2 <- read.csv(paste0(forecast_folder,'/', myfiles_forecast[i]))
  dataset_forecast <- rbind(dataset_forecast, temp_2)
}


mean(dataset_forecast$forecast_variance, na.rm = TRUE)

# some data arranging
stuff <- dataset_forecast 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$week)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")
# remove the 'spin up' period from Aug 15, 2018 - Dec 31, 2018
stuff <- stuff[stuff$forecast_date>'2018-12-31',]


# bring in the null model data
myfiles_null <- list.files(path = paste0(forecast_folder, '/null_ensemble'), pattern = paste0('*', 'null_summary', ".csv"))
dataset_null <- read.csv(paste0(paste0(forecast_folder, '/null_ensemble'), "/", myfiles_null[1]))
# shit I forgot to archive the forecast run day within the null model file
# make up a vector of the dates
dataset_null$week_in_future <- NA

# read in files
for (i in 2:length(myfiles_null)) {
  temp <- read.csv(paste0(forecast_folder, '/null_ensemble',"/", myfiles_null[i]))
  temp$week_in_future <- seq(0,2)
  dataset_null <- rbind(dataset_null, temp)
}
dataset_null[1:3,5] <- c(0,1,2)
dataset_null$date <- as.Date(dataset_null$date)
dataset_null <- dataset_null[dataset_null$date>'2018-12-31',]

# separate into forecast horizon for individual analysis
for (i in 1:2) { # change the 16 to whatever number of timesteps you have
  temp <- stuff[stuff$week==i,]
  write.csv(temp, paste0(forecast_folder, '/day_', i, '.csv'), row.names = FALSE)
}

for (i in 1:2) { # change the 16 to whatever number of timesteps you have
  temp <- dataset_null[dataset_null$week_in_future==i,]
  write.csv(temp, paste0(forecast_folder, '/day_', i, '_null.csv'), row.names = FALSE)
}


# loop to make figures at each time step
# MOVING Y AXIS
for(i in 1:2){

# read in csv of each forecast horizon
temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
temp$forecast_date <- as.Date(temp$forecast_date)

#create and save figure with forecast mean, confidence intervals, and obs chl
png(paste0(forecast_folder, '/Forecast_day_', i, 'moving_y_axis.png'), width = 1100, height = 800)
print(ggplot(temp, aes(forecast_date, forecast_mean_chl)) +
        geom_line(size = 2) +
        geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'green4') +
        geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
        xlab('Date') +
       # ylim(0,150)+
        ylab('Chlorophyll a (μg/L)') +
        ggtitle(paste0('Week ', i, ' Forecast')) +
        #geom_vline(xintercept = as.numeric(as.Date("2019-07-10", "%Y-%m-%d")), color = 'black', size = 1.5, linetype = 'dashed') +
        #geom_vline(xintercept = as.numeric(as.Date("2019-08-15", "%Y-%m-%d")), color = 'black', size = 1.5, linetype = 'dashed') +
        theme(axis.text.x = element_text(size = 30),
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
# SET Y AXIS
for(i in 1:2){
  
  # read in csv of each forecast horizon
  temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
  temp$forecast_date <- as.Date(temp$forecast_date)
  
  #create and save figure with forecast mean, confidence intervals, and obs chl
  png(paste0(forecast_folder, '/Forecast_day_', i, '.png'), width = 1100, height = 800)
  print(ggplot(temp, aes(forecast_date, forecast_mean_chl)) +
          geom_line(size = 2) +
          geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'green4') +
          geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
          xlab('Date') +
           ylim(0,150)+
          ylab('Chlorophyll a (μg/L)') +
          ggtitle(paste0('Week ', i, ' Forecast')) +
          #geom_vline(xintercept = as.numeric(as.Date("2019-07-10", "%Y-%m-%d")), color = 'black', size = 1.5, linetype = 'dashed') +
          #geom_vline(xintercept = as.numeric(as.Date("2019-08-15", "%Y-%m-%d")), color = 'black', size = 1.5, linetype = 'dashed') +
          theme(axis.text.x = element_text(size = 30),
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

######################################################################################################################################################################
## aaand calculate metrics

metrics <- array(NA, dim = c(8, 6))
row.names(metrics) <- c('RMSE', "NSE", 'KGE', 'bias', 'bias_sd', 'R2_1_1', 'coefficient of determination', 'corr_p')
colnames(metrics) <- c('forecast', 'null', 'forecast_nonbloom', 'null_nonbloom', 'forecast_bloom', 'null_bloom')

metrics_overtime <- array(NA, dim = c(2,13))
colnames(metrics_overtime) <- c('week_in_future','RMSE_null', 'RMSE_forecast',"RMSE_null_nonbloom", 'RMSE_forecast_nonbloom', 
                                'R2_null', 'R2_forecast',"R2_null_nonbloom", 'R2_forecast_nonbloom', 
                                'RMSE_null_bloom', 'RMSE_forecast_bloom', 'R2_null_bloom', 'R2_forecasT_bloom')
for (i in 1:2) {
  # read in csv of each forecast horizon
  temp <- read.csv(paste0(forecast_folder, '/day_', i, '.csv'))
  temp$forecast_date <- as.Date(temp$forecast_date)
  
  temp_null <- read.csv(paste0(forecast_folder, '/day_', i, '_null.csv'))
  temp_null$date <- as.Date(temp_null$date)
  
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
  
  non_bloom <- temp[temp$forecast_date< as.Date("2019-07-17") | temp$forecast_date > as.Date("2019-08-05"), ] # these dates correspond to bloom threshold of 4*sd(historical chla)
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
  
  
  datamerge_nonbloom <- temp_null[temp_null$date< as.Date("2019-07-17") | temp_null$date > as.Date("2019-08-05"), ]
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
  
  bloom <- temp[temp$forecast_date> as.Date("2019-07-17") , ]
  bloom <- bloom[bloom$forecast_date  < as.Date("2019-08-05"),]
  bloom_forecast <- model_metrics(bloom$forecast_mean_chl, bloom$obs_chl_EXO)
  metrics[1,5] <- bloom_forecast$RMSE
  metrics[2,5] <- bloom_forecast$NSE
  metrics[3,5] <- bloom_forecast$KGE
  metrics[4,5] <- bloom_forecast$bias
  metrics[5,5] <- bloom_forecast$bias_sd
  metrics[6,5] <- bloom_forecast$R2_1_1
  metrics[7,5] <- bloom_forecast$coeff_determination
  metrics[8,5] <- bloom_forecast$cor_p
  metrics_overtime[i,11] <- bloom_forecast$RMSE
  metrics_overtime[i, 13] <- bloom_forecast$coeff_determination

  
  datamerge_bloom <- temp_null[temp_null$date > as.Date("2019-07-17") , ]
  datamerge_bloom <- datamerge_bloom[datamerge_bloom$date < as.Date('2019-08-05'),]
  bloom_null <- model_metrics(datamerge_bloom$mean, bloom$obs_chl_EXO)
  metrics[1,6] <- bloom_null$RMSE
  metrics[2,6] <- bloom_null$NSE
  metrics[3,6] <- bloom_null$KGE
  metrics[4,6] <- bloom_null$bias
  metrics[5,6] <- bloom_null$bias_sd
  metrics[6,6] <- bloom_null$R2_1_1
  metrics[7,6] <- bloom_null$coeff_determination
  metrics[8,6] <- bloom_null$cor_p
  metrics_overtime[i,10] <- bloom_null$RMSE
  metrics_overtime[i,12] <- bloom_null$coeff_determination
  
  print(paste0('metrics on week ', i))
  print(metrics)
  #write.csv(metrics, paste0(forecast_folder, '/ForecastMetrics_day_', i, '.csv'))
  
}

metrics_overtime_weekly <- as.data.frame(metrics_overtime) 
metrics <- as.data.frame(metrics)

write.csv(metrics_overtime_weekly, paste0(forecast_folder, '/ForecastMetrics_Weekly.csv'), row.names = FALSE)

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Weekly_RMSE_over_time.png', width = 1100, height = 800)
ggplot(data = metrics_overtime_weekly, aes(x = week_in_future,y = RMSE_null)) + 
  geom_point(aes(week_in_future, RMSE_null), col = 'red', size = 15) +
  geom_point(aes(week_in_future, RMSE_forecast), col = 'blue', size = 15)+
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
dev.off()  

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Weekly_RMSE_different_conditions.png', width = 1100, height = 800)
ggplot(data = metrics_overtime_weekly, aes(x = week_in_future,y = RMSE_null)) + 
  geom_point(aes(week_in_future, RMSE_forecast_nonbloom), col = 'orange', size = 15) +
  geom_point(aes(week_in_future, RMSE_forecast_bloom), col = 'red', size = 15) +
  geom_point(aes(week_in_future, RMSE_forecast), col = 'blue', size = 15)+
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
dev.off()

# this output now gets read in to 'plot_assess_daily_model.R' in order to compare weekly and daily forecasts

