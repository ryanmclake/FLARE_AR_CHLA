# script to take appended arima forecast output and plot them and calculate some model performance statistics

library(rsq)
library(tidyverse)
library(lubridate)
library(Metrics)
library(hydroGOF)
reference_tzone <- "GMT"

#set the location of the forecasts
folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
timestep <- 'daily'
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/relhum_model_truncated_training_data')
setwd(forecast_folder)

# code to read in the individual forecast files named for the day on which the forecast is made
myfiles_forecast <- list.files(path = forecast_folder, pattern = paste0('*', timestep, ".csv"))
dataset_forecast <- read.csv(paste0(forecast_folder, "/", myfiles_forecast[1]))

# read in files
for (i in 2:length(myfiles_forecast)) {
  temp_2 <- read.csv(paste0(forecast_folder,"/", myfiles_forecast[i]))
  dataset_forecast <- rbind(dataset_forecast, temp_2)
}

# this is the entire dataset though that includes the forecasts for each time horizon

mean(dataset_forecast$forecast_variance, na.rm = TRUE)

# some data arranging
stuff <- dataset_forecast 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$day_in_future)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")

# calculate a few things that will be needed for analysis
stuff <- stuff %>% mutate(residual = forecast_mean_chl - obs_chl_EXO) %>% 
  mutate(data_minusglobalmean = obs_chl_EXO - mean(obs_chl_EXO, na.rm = TRUE))

# some figures
# all forecasts (week1 and week2 together) plotted with the obs data
yrange <- range(stuff$forecast_mean_chl, stuff$obs_chl_EXO, na.rm = TRUE)
plot(stuff$forecast_date, stuff$forecast_mean_chl, col = 'red', type = 'l', ylim = yrange)
points(stuff$forecast_date, stuff$obs_chl_EXO, type = 'l')
abline(v = as.Date("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.Date("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate

ggplot(data = stuff, aes(x = forecast_date, y = forecast_mean_chl)) + 
  geom_line() + 
  facet_wrap(~day_in_future)+ 
  geom_point(aes(y = obs_chl_EXO, color = 'red'))

ggplot(data = stuff, aes(x = forecast_date, y = residual)) + 
  geom_line() + 
  facet_wrap(~day_in_future)

hist(stuff$residual)

ggplot(data = stuff, aes(x = forecast_date)) + geom_line(aes(y = forecast_mean_chl, color = day_in_future)) + geom_line(aes(y = obs_chl_EXO))

ggplot(stuff, aes(day_in_future, residual)) + 
  geom_point(aes(color = day_in_future)) +
  scale_color_gradient(low = "blue", high = "red")


# separate into different days for separate analysis
# separate into forecast horizon for individual analysis
for (i in 1:16) { # change the 16 to whatever number of timesteps you have
  temp <- stuff[stuff$day_in_future==i,]
  write.csv(temp, paste0(forecast_folder, '/day_', i, '.csv'), row.names = FALSE)
}


day_1 <- read.csv(paste0(forecast_folder, '/day_1.csv'))
day_1$forecast_date <- as.Date(day_1$forecast_date)

hist(day_1$residual)

plot(day_1$forecast_date, day_1$obs_chl_EXO)
points(day_1$forecast_date, day_1$forecast_mean_chl, col = 'red', type = 'l')
points(day_1$forecast_date, day_1$week_lag_chl, col = 'blue', type = 'l')
plot(day_1$forecast_date, day_1$obs_chl_EXO, xlim = c(as.Date('2019-06-22'), as.Date('2019-09-06')))
points(day_1$forecast_date, day_1$forecast_mean_chl, col = 'red')


  
  
#png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/daily_forecasts/ConfIntForecasts2019_noabline_day1.png', width = 1100, height = 800)
ggplot(day_1, aes(forecast_date, forecast_mean_chl)) +
  geom_line(size = 2) +
  geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'green4') +
  geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'forestgreen', linetype = 2, alpha = 0.2) +
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  #geom_vline(xintercept = as.numeric(as.Date("2019-02-28", "%Y-%m-%d")), color = 'blue', size = 1.5) +
  #geom_vline(xintercept = as.numeric(as.Date("2019-03-20", "%Y-%m-%d")), color = 'blue', size = 1.5) +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        legend.position = 'right',
        panel.grid.minor = element_blank())
# panel.background = element_rect(fill = '#9DC3E6', color = '#9DC3E6'),
#  plot.background = element_rect(fill = '#9DC3E6', color = '#9DC3E6'))
#dev.off()


# and calculate what percentage of obs data is between the conf intervals
day_1 <- day_1 %>% mutate(CI95_yesno = ifelse(obs_chl_EXO<forecast_CI95_upper & obs_chl_EXO>forecast_CI95_lower, 1, 0))
(nrow(day_1[day_1$CI95_yesno==0,])/nrow(day_1))*100
plot(day_1$forecast_date, day_1$CI95_yesno)
abline(v = as.Date("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.Date("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate

############################
### how far in advance was a bloom detected by the forecast?
###########################

# subset to time frame before and after the bloom
dates <- seq(as.Date('2019-06-22'), as.Date('2019-09-06'), by = 'day')
bloom <- day_1[day_1$forecast_date>'2019-06-22' & day_1$forecast_date<'2019-09-06',]

# as a temporary proxy for a 'bloom', use 3x (or 4x?) the standard deviation of the historical dataset for FCR
# this threshold is specific to FCR
# (x[2,,]^2)/0.55) + 0.0308
thresh <- read.csv(paste0(folder, '/data_arima_WW.csv'))
thresh <- thresh %>% mutate(chl_EXOunits = ((Chla_sqrt^2)/0.55) + 0.0308   )
threshold <- 3*sd(thresh$chl_EXOunits)
plot(as.Date(thresh$Date)  ,thresh$chl_EXOunits)

hist <- thresh %>% select(Date, chl_EXOunits)
hist$Date <- as.Date(hist$Date)
new <- day_1 %>% select(forecast_date, obs_chl_EXO)
colnames(new) <- c('Date', 'chl_EXOunits')

chl_all <- rbind(hist, new)
plot(chl_all$Date, chl_all$chl_EXOunits, xlab = 'Date', ylab = 'Chlorophyll-a (ug/L)')


for (i in 1:nrow(bloom)) {
plot(bloom$forecast_date, bloom$obs_chl_EXO, type = 'n')
  points(bloom$forecast_date[i], bloom$forecast_mean_chl[i], col = 'red', pch = 19)
  points(bloom$forecast_date[i], bloom$obs_chl_EXO[i], col = 'green', pch = 23)
  points(bloom$forecast_date[i], bloom$forecast_CI95_lower[i], col = 'grey69', pch = 20)
  points(bloom$forecast_date[i], bloom$forecast_CI95_upper[i], col = 'grey69', pch = 20)
  abline(v = bloom$forecast_run_day[i])
  abline(h = 8.6, col = 'green') # bloom threshold 
}


# calculate a binary 'yes'=1 or 'no'=0 for the bloom threshold
bloom <- bloom %>% mutate(forecast_bloom = ifelse(forecast_mean_chl>threshold, 1, 0)) %>% 
  mutate(obs_bloom = ifelse(obs_chl_EXO>threshold, 1,0))
plot(bloom$forecast_date, bloom$obs_bloom)
points(bloom$forecast_date, bloom$forecast_bloom, col = 'red')
#############################################################################################################################################################
####### calculate forecast assessment metrics ##########################################################################################################
#############################################################################################################################################################

# bring in the null model to calculate the same metrics
source(paste0(folder,"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) #this is the original file modified to take a daily avg rather than the midnight reading
source(paste0(folder,"/","Rscripts/temp_oxy_chla_qaqc.R")) 

data_location = "C:/Users/wwoel/Desktop/FLARE_AR_CHLA/SCCData"
temperature_location <- paste0(data_location, "/", "mia-data")

# specify full_time as the entire time series from 08-15-2018 to 10-31-2019
full_time <- seq(as.Date("2019-04-14"), as.Date("2019-10-31"), by = "1 day") # set for 16 days earlier so that the 16 day forecasts can have observed chl for the null
observed_depths_chla_fdom <- 1
reference_tzone <- "GMT"
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
cleaned_temp_oxy_chla_file <- paste0(working_arima, "/Catwalk_postQAQC.csv")
temp_oxy_chla_qaqc(temp_obs_fname_wdir[1], 
                   paste0(data_location, '/mia-data/CAT_MaintenanceLog.txt'), 
                   cleaned_temp_oxy_chla_file)

new_temp_obs_fname_wdir <- temp_obs_fname_wdir
new_temp_obs_fname_wdir[1] <- cleaned_temp_oxy_chla_file

# change the function to 'extract_chla_chain_dailyavg
chla_obs <- extract_chla_chain_dailyavg(fname = new_temp_obs_fname_wdir,
                                        full_time,
                                        depths = 1.0,
                                        observed_depths_chla_fdom = observed_depths_chla_fdom,
                                        input_tz = "EST5EDT", 
                                        output_tz = reference_tzone)




data <- as.data.frame(full_time)
data2 <- as.data.frame(chla_obs[[1]][,1])
datamerge <- cbind(data, data2)
colnames(datamerge) <- c("forecast_date", "exo_chl_ugL")
datamerge$exo_chl_ugL[is.nan(datamerge$exo_chl_ugL)] <- NA
# add process error
load("C:/Users/wwoel/Desktop/FLARE_AR_CHLA/MCMC_output_ARIMA_highfrequency_RelHum.Rdata")




added_process_uncertainty =  mean(samples[[1]][,4]) # mean process error added to all values

datamerge <- datamerge %>% mutate(exo_chl_ugL_pluserror = exo_chl_ugL + added_process_uncertainty) %>% 
  mutate(day_1_lag = lag(exo_chl_ugL_pluserror, 1L)) %>% 
  mutate(day_2_lag = lag(exo_chl_ugL_pluserror, 2L)) %>%
  mutate(day_3_lag = lag(exo_chl_ugL_pluserror, 3L)) %>%
  mutate(day_4_lag = lag(exo_chl_ugL_pluserror, 4L)) %>%
  mutate(day_5_lag = lag(exo_chl_ugL_pluserror, 5L)) %>%
  mutate(day_6_lag = lag(exo_chl_ugL_pluserror, 6L)) %>%
  mutate(day_7_lag = lag(exo_chl_ugL_pluserror, 7L)) %>%
  mutate(day_8_lag = lag(exo_chl_ugL_pluserror, 8L)) %>%
  mutate(day_9_lag = lag(exo_chl_ugL_pluserror, 9L)) %>%
  mutate(day_10_lag = lag(exo_chl_ugL_pluserror, 10L)) %>%
  mutate(day_11_lag = lag(exo_chl_ugL_pluserror, 11L)) %>%
  mutate(day_12_lag = lag(exo_chl_ugL_pluserror, 12L)) %>%
  mutate(day_13_lag = lag(exo_chl_ugL_pluserror, 13L)) %>%
  mutate(day_14_lag = lag(exo_chl_ugL_pluserror, 14L)) %>%
  mutate(day_15_lag = lag(exo_chl_ugL_pluserror, 15L)) %>%
  mutate(day_16_lag = lag(exo_chl_ugL_pluserror, 16L)) 

# remove the days on/after copper sulfate dosing because this is outside of the ability of the model to anticipate
datamerge$forecast_date <- as.Date(datamerge$forecast_date)
datamerge <- datamerge[datamerge$forecast_date > '2019-04-30',]

up1 <- datamerge[datamerge$forecast_date<"2019-02-28",]
down1 <- datamerge[datamerge$forecast_date>"2019-03-20",]
datamerge <- rbind(up1, down1)
write.csv(datamerge, paste0(folder, '/daily_null.csv'), row.names = FALSE)

# subset to the non-bloom period
day_1_nonbloom <- day_1[day_1$forecast_date < as.Date("2019-07-10") |day_1$forecast_date > as.Date("2019-08-10"),]
null_nonbloom <- datamerge[datamerge$forecast_date< as.Date("2019-07-10") | datamerge$forecast_date > as.Date("2019-08-10"), ]





####################################################################################################################################################################################
## calculate model assessment metric using function ##########################################################################################################################
####################################################################################################################################################################################

source(paste0(folder,"/","Rscripts/model_assessment.R")) # sim, obs



null_1 <- model_metrics(datamerge$day_1_lag, datamerge$exo_chl_ugL)
forecast_1 <- model_metrics(day_1$forecast_mean_chl, day_1$obs_chl_EXO)

metrics_day_1_nonbloom <- model_metrics(day_1_nonbloom$forecast_mean_chl, day_1_nonbloom$obs_chl_EXO)
metrics_day_1_nonbloom_null <- model_metrics(null_nonbloom$day_1_lag, null_nonbloom$exo_chl_ugL)



metrics_week2_nonbloom <- model_metrics(week2_nonbloom$forecast_mean_chl, week2_nonbloom$obs_chl_EXO)
metrics_week2_nonbloom_null <- model_metrics(week2_nonbloom$week2_lag_chl, week2_nonbloom$obs_chl_EXO)

#ugh can't figure out how to format function to output metrics in an accessible way so hardcoded them into an xcel doc, read in here
metrics <- read.csv('./model_metrics.csv')
metrics_nonbloom <- read.csv('./model_metrics_nonbloom.csv')


png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/RMSE_over_time.png', width = 1100, height = 800)
ggplot(data = metrics, aes(x = Forecast_horizon, y = RMSE)) + 
  geom_point(aes(col = Type), size = 15) + 
  xlab('Weeks into the future') + 
  ylab('RMSE (ug/L)') +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c('1','2'))
dev.off()

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/RMSE_over_time_nonbloom.png', width = 1100, height = 800)
ggplot(data = metrics_nonbloom, aes(x = Forecast_horizon, y = RMSE)) + 
  geom_point(aes(col = Type), size = 15) + 
  xlab('Weeks into the future') + 
  ylab('RMSE (ug/L)') +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c('1','2'))
dev.off()

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/KGE_over_time.png', width = 1100, height = 800)
ggplot(data = metrics, aes(x = Forecast_horizon, y = KGE)) + 
  geom_point(aes(col = Type), size = 15) + 
  xlab('Weeks into the future') + 
  ylab('KGE') +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c('1','2'))
dev.off() 

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/KGE_over_time_nonbloom.png', width = 1100, height = 800)
ggplot(data = metrics_nonbloom, aes(x = Forecast_horizon, y = KGE)) + 
  geom_point(aes(col = Type), size = 15) + 
  xlab('Weeks into the future') + 
  ylab('KGE') +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(limits = c('1','2'))
dev.off() 

  ggplot(data = metrics, aes(x = Forecast_horizon, y = NSE)) + 
    geom_point(aes(col = Type), size = 12) + 
    xlab('Weeks into the future') + 
    ylab('NSE')# +
  
  png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/R2_1_1_over_time.png', width = 1100, height = 800)
  ggplot(data = metrics, aes(x = Forecast_horizon, y = NSE)) + 
    geom_jitter(aes(col = Type), size = 12) + 
    xlab('Weeks into the future') + 
    ylab('Nash-Sutcliffe Efficiency') +
    theme(axis.text.x = element_text(size = 35),
          axis.text.y = element_text(size = 50),
          axis.title.x = element_text(size =45),
          axis.title.y = element_text(size = 45),
          legend.title = element_text(size = 35),
          legend.text = element_text(size = 30),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_discrete(limits = c('1','2'))
  dev.off()
  
  png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/R2_1_1_over_time_nonbloom.png', width = 1100, height = 800)
  ggplot(data = metrics_nonbloom, aes(x = Forecast_horizon, y = NSE)) + 
    geom_point(aes(col = Type), size = 12) + 
    xlab('Weeks into the future') + 
    ylab('Nash-Sutcliffe Efficiency') +
    theme(axis.text.x = element_text(size = 35),
          axis.text.y = element_text(size = 50),
          axis.title.x = element_text(size =45),
          axis.title.y = element_text(size = 45),
          legend.title = element_text(size = 35),
          legend.text = element_text(size = 30),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_discrete(limits = c('1','2'))
  dev.off()  

    ggplot(data = metrics, aes(x = Forecast_horizon, y = coeff_determination)) + 
    geom_point(aes(col = Type), size = 12) + 
    xlab('Weeks into the future') + 
    ylab('Coefficient of Determination')# +
  
  
  # old calcs below

# pearson's correlation
cor(week1$forecast_mean_chl, week1$obs_chl_EXO, method = 'pearson')
cor(week2$forecast_mean_chl, week2$obs_chl_EXO, method = 'pearson')
cor(week1$week_lag_chl, week1$obs_chl_EXO, method = 'pearson')
cor(week2$week2_lag_chl, week2$obs_chl_EXO, method = 'pearson')


# rmse for mean of ensembles
rmse(week1$forecast_mean_chl, week1$obs_chl_EXO)
rmse(week2$forecast_mean_chl, week2$obs_chl_EXO)
rmse(week1_nonbloom$forecast_mean_chl, week1_nonbloom$obs_chl_EXO)
rmse(week2$forecast_mean_chl, week2$obs_chl_EXO)
rmse(week1$week_lag_chl, week1$obs_chl_EXO)
rmse(week2$week2_lag_chl, week2$obs_chl_EXO)

# rmse for median of ensembles
rmse(week1$forecast_median_chl, week1$obs_chl_EXO)
rmse(week2$forecast_median_chl, week2$obs_chl_EXO)

# bias calculations for mean and median of both weeks (1 is non-biased, <1 means model is smoothing, >1 means model is predicting more fluctuations than are observed)
sd(week1$forecast_mean_chl)/sd(week1$obs_chl_EXO)
sd(week2$forecast_mean_chl)/sd(week2$obs_chl_EXO)
sd(week1$week_lag_chl)/sd(week1$obs_chl_EXO)
sd(week2$week2_lag_chl)/sd(week2$obs_chl_EXO)
sd(week1$forecast_median_chl)/sd(week1$obs_chl_EXO)
sd(week2$forecast_median_chl)/sd(week2$obs_chl_EXO)


# r2 calculation, variation from the 1:1 line
# calculate the data minus global mean now that the copper sulfate days are removed
week1 <- week1 %>% mutate(data_minusglobalmean = obs_chl_EXO - mean(obs_chl_EXO, na.rm = TRUE)) %>% 
  mutate(residual_null = week_lag_chl - exo_chl_ugL)

1 - (sum(week1$residual^2)/sum(week1$data_minusglobalmean^2))
1 - (sum(week1$residual_null^2)/sum(week1$data_minusglobalmean^2))

week2 <- week2 %>% mutate(data_minusglobalmean = obs_chl_EXO - mean(obs_chl_EXO, na.rm = TRUE)) %>% 
  mutate(residual_null = week2_lag_chl - exo_chl_ugL)
1 - (sum(week2$residual^2)/sum(week2$data_minusglobalmean^2))
1 - (sum(week2$residual_null^2)/sum(week2$data_minusglobalmean^2))


#r2 as a linear model
summary(lm(week1$forecast_mean_chl~week1$obs_chl_EXO))
summary(lm(week1_nonbloom$forecast_mean_chl~week1_nonbloom$obs_chl_EXO))
summary(lm(week1$week_lag_chl~week1$obs_chl_EXO))
summary(lm(week2$forecast_mean_chl~week2$obs_chl_EXO))
summary(lm(week2$week2_lag_chl~week2$obs_chl_EXO))

# bias or mean residual
mean(week1$residual)
mean(week1$residual_null)
mean(week2$residual)
mean(week2$residual_null)

#null model

plot(week1$forecast_date, week1$residual, type = 'l', col = 'blue', ylim = c(-40,40))
points(week1$forecast_date, week1$residual_null, type = 'l', col = 'red')
plot(week2$forecast_date, week2$residual, type = 'l', col = 'blue', ylim = c(-50, 50))
points(week2$forecast_date, week2$residual_null, type = 'l', col = 'red')


#nash sutcliffe efficiency, 
#1 = perfect, 0 = model predictions are as accurate as the mean, NSE < 0 meansobserved mean is better predictor than the model
# suggested threshold of NSE > 0.5 for model of 'sufficient quality'
#'The efficiency coefficient is sensitive to extreme values and might yield sub-optimal results when the dataset contains large outliers in it.'

NSE(week1$forecast_mean_chl, week1$obs_chl_EXO)
NSE(week1$week_lag_chl, week1$obs_chl_EXO)
NSE(week2$forecast_mean_chl, week2$obs_chl_EXO)
NSE(week2$week_lag_chl, week2$obs_chl_EXO)

# kling-gupta efficiency, a modification of NSE
KGE(week1$forecast_mean_chl, week1$obs_chl_EXO)
KGE(week1$week_lag_chl, week1$obs_chl_EXO)
KGE(week2$forecast_mean_chl, week2$obs_chl_EXO)
KGE(week2$week2_lag_chl, week2$obs_chl_EXO)
