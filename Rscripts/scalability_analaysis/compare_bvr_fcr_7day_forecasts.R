# compare BVR and FCR forecasts from Aug 2020 to Aug 2021
library(tidyverse)
library(ggpubr)

folder <- getwd()
timestep <- '7day'
max_timestep <- 2

############ read in BVR files ##########################################################
# read in the individual forecast files named for the day on which the forecast is made
bvr_folder <- file.path(folder, 'FCR_forecasts', '7day', 'BVR_scalability_Aug2020-2021')
bvr_files <- list.files(path = bvr_folder, pattern = paste0('*', timestep, ".csv"))
bvr_fcasts <- read.csv(paste0(bvr_folder, "/", bvr_files[1]))
bvr_fcasts$timestep <- seq(1, max_timestep, by =1 )


# read in files
for (i in 2:length(bvr_files)) {
  temp_2 <- read.csv(paste0(bvr_folder,"/", bvr_files[i]))
  temp_2$timestep <-  seq(1, max_timestep, by =1 ) # this is the number of timesteps continued into the future, maybe should call this horizon?
  bvr_fcasts <- rbind(bvr_fcasts, temp_2)
}

bvr_fcasts$forecast_date <- as.Date(bvr_fcasts$forecast_date, "%Y-%m-%d")
bvr_fcasts <- bvr_fcasts[order(bvr_fcasts$forecast_date),]
bvr_fcasts$forecast_run_day <- as.Date(bvr_fcasts$forecast_run_day, "%Y-%m-%d")
bvr_fcasts <- na.omit(bvr_fcasts)

############ read in FCR files ##########################################################
# read in the individual forecast files named for the day on which the forecast is made
fcr_folder <- file.path(folder, 'FCR_forecasts', '7day', 'FCR_scalability_Aug2020-2021')
fcr_files <- list.files(path = fcr_folder, pattern = paste0('*', timestep, ".csv"))
fcr_fcasts <- read.csv(paste0(fcr_folder, "/", fcr_files[1]))
fcr_fcasts$timestep <- seq(1, max_timestep, by =1 )


# read in files
for (i in 2:length(fcr_files)) {
  temp_2 <- read.csv(paste0(fcr_folder,"/", fcr_files[i]))
  temp_2$timestep <-  seq(1, max_timestep, by =1 ) # this is the number of timesteps continued into the future, maybe should call this horizon?
  fcr_fcasts <- rbind(fcr_fcasts, temp_2)
}

fcr_fcasts$forecast_date <- as.Date(fcr_fcasts$forecast_date, "%Y-%m-%d")
fcr_fcasts <- fcr_fcasts[order(fcr_fcasts$forecast_date),]
fcr_fcasts$forecast_run_day <- as.Date(fcr_fcasts$forecast_run_day, "%Y-%m-%d")
fcr_fcasts <- na.omit(fcr_fcasts)

# make sure they have the same dat range
fcr_fcasts <- fcr_fcasts[fcr_fcasts$forecast_date <= as.Date('2021-09-17'),]
bvr_fcasts <- bvr_fcasts[bvr_fcasts$forecast_date <= max(fcr_fcasts$forecast_date),]

max(fcr_fcasts$forecast_date)
max(bvr_fcasts$forecast_date)
min(bvr_fcasts$forecast_date)
min(fcr_fcasts$forecast_date)

###########################################################################################################
# calculate RMSE based on forecast horizon
source(paste0(folder,"/","Rscripts/model_assessment.R")) # sim, obs

metrics <- data.frame('Reservoir' = rep(c('BVR', 'FCR'), 2),
                      'horizon' = c(7,7,14,14),
                      'RMSE' = NA)

#######################################
## for the weekly timestep
b <- bvr_fcasts[bvr_fcasts$timestep==1,]
f <- fcr_fcasts[fcr_fcasts$timestep==1,]

b_stat <- model_metrics(b$forecast_mean_chl, b$obs_chl_EXO)
f_stat <- model_metrics(f$forecast_mean_chl, f$obs_chl_EXO)

metrics[1,3] <- round(b_stat$RMSE, 2)
metrics[2,3] <- round(f_stat$RMSE, 2)

###########################################
## for the fortnightly timestep
b <- bvr_fcasts[bvr_fcasts$timestep==2,]
f <- fcr_fcasts[fcr_fcasts$timestep==2,]

b_stat <- model_metrics(b$forecast_mean_chl, b$obs_chl_EXO)
f_stat <- model_metrics(f$forecast_mean_chl, f$obs_chl_EXO)
Metrics::rmse(b$forecast_mean_chl, b$obs_chl_EXO)

metrics[3,3] <- round(b_stat$RMSE, 2)
metrics[4,3] <- round(f_stat$RMSE, 2)

metrics

ggplot(data = metrics, aes(x = horizon, y = RMSE, col = Reservoir)) +
  geom_point(size = 4) +
  xlim(0, 16)  
  
b1 <- ggplot(bvr_fcasts[bvr_fcasts$timestep==1,], aes(x = forecast_date, y = forecast_mean_chl)) +
  geom_line() +
  geom_text(x = as.Date('2021-08-01'), y = max(bvr_fcasts$forecast_CI95_upper), size = 7, label = paste0('RMSE = ', metrics[1,3])) + 
  geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
  geom_point(aes(x = forecast_date, y = obs_chl_EXO), size = 2, color = 'green3') +
  ggtitle('BVR 1-week forecasts') +
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  scale_x_date(labels = scales::date_format('%b')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 25),
        panel.grid.major = element_blank(),
        axis.title.x=element_blank(),
        legend.position = 'right',
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25))

b2 <- ggplot(bvr_fcasts[bvr_fcasts$timestep==2,], aes(x = forecast_date, y = forecast_mean_chl)) +
  geom_line() +
  geom_text(x = as.Date('2021-08-01'), y =max(bvr_fcasts$forecast_CI95_upper)-0.3, size = 7, label = paste0('RMSE = ', metrics[3,3])) + 
  geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
  geom_point(aes(x = forecast_date, y = obs_chl_EXO), size = 2, color = 'magenta3') +
  ggtitle('BVR 2-week forecasts') +  
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  scale_x_date(labels = scales::date_format('%b')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 30),
        axis.title.y = element_text(size = 25),
        panel.grid.major = element_blank(),
        axis.title.x=element_blank(),
        legend.position = 'right',
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25))

b3 <- ggplot(fcr_fcasts[fcr_fcasts$timestep==1,], aes(x = forecast_date, y = forecast_mean_chl)) +
  geom_line() +
  geom_text(x = as.Date('2021-08-01'), y = max(fcr_fcasts$forecast_CI95_upper), size = 7, label = paste0('RMSE = ', metrics[2,3])) + 
  geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
  geom_point(aes(x = forecast_date, y = obs_chl_EXO), size = 2, color = 'green4') +
  ggtitle('FCR 1-week forecasts')+ 
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  scale_x_date(labels = scales::date_format('%b')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size =25),
        axis.title.y = element_text(size = 25),
        panel.grid.major = element_blank(),
        legend.position = 'right',
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25))

b4 <- ggplot(fcr_fcasts[fcr_fcasts$timestep==2,], aes(x = forecast_date, y = forecast_mean_chl)) +
  geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'grey1', linetype = 2, alpha = 0.2) +
  geom_text(x = as.Date('2021-08-01'), y = max(fcr_fcasts$forecast_CI95_upper), size = 7, label = paste0('RMSE = ', metrics[4,3])) + 
  geom_line() +
  geom_point(aes(x = forecast_date, y = obs_chl_EXO), size = 2, color = 'darkorchid4') +
  ggtitle('FCR 2-week forecasts') +
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  scale_x_date(labels = scales::date_format('%b')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 30),
        axis.title.x = element_text(size =25),
        axis.title.y = element_text(size = 25),
        panel.grid.major = element_blank(),
        legend.position = 'right',
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 25))

png(paste0(bvr_folder, '/bvr_fcr_forecasts.png'), width = 1100, height = 800)
ggarrange(b1, b2, b3, b4)
dev.off()
