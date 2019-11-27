# script to take appended arima forecast output and plot them and calculate some model performance statistics

library(rsq)
library(tidyverse)
library(Metrics)
reference_tzone <- "GMT"

#set the location of the forecasts
forecast_folder <- "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3"
setwd(forecast_folder)
# code to read in the individual forecast files named for the day on which the forecast is made
# these are the forecasts with the older exo-ctd model and which is currently (07302019) being used for the main analysis
myfiles <- list.files(path = "./FCR_forecasts/wholeyear_run_07192019", pattern = "*weekly.csv")
dataset <- read.csv(paste0("./FCR_forecasts/wholeyear_run_07192019/", myfiles[1]))

# read in files
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0("./FCR_forecasts/wholeyear_run_07192019/", myfiles[i]))
  dataset <- rbind(dataset, temp)
}

mean(dataset$forecast_variance, na.rm = TRUE)

# now read in the forecast files that were run with the updated EXO-CTD model
myfiles_new <- list.files(path = "./FCR_forecasts/newEXOCTDmodel_07262019", pattern = "*weekly.csv")
dataset_new <- read.csv(paste0("./FCR_forecasts/newEXOCTDmodel_07262019/", myfiles[1]))
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0("./FCR_forecasts/newEXOCTDmodel_07262019/", myfiles[i]))
  dataset_new <- rbind(dataset_new, temp)
  
}


# some data arranging
stuff <- dataset 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$week)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")
# limit dataset to July 15 for the purposes of ESA materials
stuff <- stuff[stuff$forecast_date<"2019-07-15",]

# and for the new exo-ctd model dataset
stuff_new <- dataset_new
stuff_new$forecast_date <- as.Date(stuff_new$forecast_date, "%Y-%m-%d")
stuff_new <- stuff_new[order(stuff_new$forecast_date),]
stuff_new$week <- as.factor(stuff_new$week)
stuff_new$forecast_run_day <- as.Date(stuff_new$forecast_run_day, "%Y-%m-%d")
# limit dataset to July 15 for the purposes of ESA materials
stuff_new <- stuff_new[stuff_new$forecast_date<"2019-07-15",]




# calculate a few things that will be needed for analysis
stuff <- stuff %>% mutate(residual = forecast_mean_chl - obs_chl_EXO) %>% 
  mutate(data_minusglobalmean = obs_chl_EXO - mean(obs_chl_EXO, na.rm = TRUE))
stuff_new <- stuff_new %>% mutate(residual = forecast_mean_chl - obs_chl_EXO) %>% 
  mutate(data_minusglobalmean = obs_chl_EXO - mean(obs_chl_EXO, na.rm = TRUE))

mean(stuff_new$residual, na.rm = TRUE)
mean(stuff$residual, na.rm = TRUE)

# some figures
# all forecasts (week1 and week2 together) plotted with the obs data
yrange <- range(stuff$forecast_mean_chl, stuff$obs_chl_EXO, na.rm = TRUE)
plot(stuff$forecast_date, stuff$forecast_mean_chl, col = 'red', type = 'l', ylim = yrange)
points(stuff$forecast_date, stuff$obs_chl_EXO, type = 'l')
abline(v = as.POSIXct("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.POSIXct("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate

# week 1 and week 2 as separate vectors
ggplot(data = stuff, aes(x = forecast_date)) + geom_line(aes(y = forecast_mean_chl, color = week)) + geom_line(aes(y = obs_chl_EXO))
ggplot(data = stuff_new, aes(x = forecast_date)) + geom_line(aes(y = forecast_mean_chl, color = week)) + geom_line(aes(y = obs_chl_EXO))


# residuals over time
ggplot(data = stuff, aes(x = forecast_date)) + 
  geom_line(aes(y = residual, color = week)) + 
  geom_vline(xintercept = as.POSIXct("2019-02-28", "%Y-%m-%d")) +
  geom_vline(xintercept = as.POSIXct("2019-03-20", "%Y-%m-%d"))
ggplot(data = stuff_new, aes(x = forecast_date)) + 
  geom_line(aes(y = residual, color = week)) + 
  geom_vline(xintercept = as.POSIXct("2019-02-28", "%Y-%m-%d")) +
  geom_vline(xintercept = as.POSIXct("2019-03-20", "%Y-%m-%d"))

hist(stuff$residual)
hist(stuff_new$residual)

plot(data = stuff[stuff$week==1,], stuff$forecast_date, stuff$forecast_mean_chl, type = 'l')
points(data = stuff_new[stuff_new$week==1,], stuff_new$forecast_date, stuff_new$forecast_mean_chl, type = 'l', col = 'blue')
points(data = stuff_new[stuff_new$week==1,], stuff_new$forecast_date, stuff_new$obs_chl_EXO, type = 'l', col = 'red')

diff <- stuff$forecast_mean_chl - stuff_new$forecast_mean_chl
plot(stuff$forecast_date, diff, type = 'l')

# separate into week1 and week2 dataframes for separate analysis
week1 <- stuff[stuff$week==1,]
week2 <- stuff[stuff$week==2,]
week1 <- na.omit(week1)
week2 <- na.omit(week2)
week1_new <- stuff_new[stuff_new$week==1,]
week2_new <- stuff_new[stuff_new$week==2,]
week1_new <- na.omit(week1_new)
week2_new <- na.omit(week2_new)



hist(week1$residual)
hist(week1_new$residual)
hist(week2$residual)
hist(week2_new$residual)

xrange <- seq(1, 14)
plot(7, mean(week1$residual), xlim = c(1,14), ylim = c(-0.7, 0.9), ylab = 'Forecast - Observed (ug/L)', xlab = 'Days in future')
points(14, mean(week2$residual))
abline(h = 0)

plot(week1$forecast_mean_chl, week1$obs_chl_EXO)
abline(0, 1)
summary(lm(week1$forecast_mean_chl~week1$obs_chl_EXO))

plot(week2$forecast_mean_chl, week2$obs_chl_EXO)
abline(0,1)
summary(lm(week2$forecast_mean_chl~week2$obs_chl_EXO))

confrange <- range(week1$forecast_CI95_upper, week1$forecast_CI95_lower, na.rm = TRUE)
plot(week1$forecast_date, week1$forecast_mean_chl, type = 'l', ylim = confrange)
points(week1$forecast_date, week1$forecast_CI95_upper, type = 'l', lty = 2)
points(week1$forecast_date, week1$forecast_CI95_lower, type = 'l', lty = 2)
points(week1$forecast_date, week1$obs_chl_EXO, col = 'red', type = 'p')
abline(v = as.POSIXct("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.POSIXct("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
legend('topright', c('forecast mean', '95% CI', 'observed'), lty = c(1, 2, 1), col = c('black', 'black', 'red'),  cex = 0.75)

plot(week1$forecast_date, week1$obs_chl_EXO)
abline(v = as.POSIXct("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.POSIXct("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
points(week1$forecast_date, week1$forecast_mean_chl, col = 'red', type = 'l')

# and calculate what percentage of obs data is between the conf intervals
week1 <- week1 %>% mutate(CI95_yesno = ifelse(obs_chl_EXO<forecast_CI95_upper & obs_chl_EXO>forecast_CI95_lower, 1, 0))
(nrow(week1[week1$CI95_yesno==0,])/nrow(week1))*100
plot(week1$forecast_date, week1$CI95_yesno)
abline(v = as.POSIXct("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.POSIXct("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate

#############################################################################################################################################################
####### calculate forecast assessment metrics ##########################################################################################################

# remove the days on/after copper sulfate dosing because this is outside of the ability of the model to anticipate
week1$forecast_date <- as.Date(week1$forecast_date)

up1 <- week1[week1$forecast_date<"2019-02-28",]
down1 <- week1[week1$forecast_date>"2019-03-20",]
week1 <- rbind(up1, down1)

up2 <- week2[week2$forecast_date<"2019-02-28",]
down2 <- week2[week2$forecast_date>"2019-03-20",]
week2 <- rbind(up2, down2)

week1_new$forecast_date <- as.Date(week1_new$forecast_date)

up1_new <- week1_new[week1_new$forecast_date<"2019-02-28",]
down1_new <- week1_new[week1_new$forecast_date>"2019-03-20",]
week1_new <- rbind(up1_new, down1_new)

up2_new <- week2_new[week2_new$forecast_date<"2019-02-28",]
down2_new <- week2_new[week2_new$forecast_date>"2019-03-20",]
week2_new <- rbind(up2_new, down2_new)


# pearson's correlation
cor(week1$forecast_mean_chl, week1$obs_chl_EXO, method = 'pearson')
cor(week1_new$forecast_mean_chl, week1_new$obs_chl_EXO, method = 'pearson')

cor(week2$forecast_mean_chl, week2$obs_chl_EXO, method = 'pearson')
cor(week2_new$forecast_mean_chl, week2_new$obs_chl_EXO, method = 'pearson')


# rmse for mean of ensembles
rmse(week1$forecast_mean_chl, week1$obs_chl_EXO)
rmse(week1_new$forecast_mean_chl, week1_new$obs_chl_EXO)
rmse(week2$forecast_mean_chl, week2$obs_chl_EXO)
rmse(week2_new$forecast_mean_chl, week2_new$obs_chl_EXO)
# rmse for median of ensembles
rmse(week1$forecast_median_chl, week1$obs_chl_EXO)
rmse(week2$forecast_median_chl, week2$obs_chl_EXO)

# bias calculations for mean and median of both weeks (1 is non-biased, <1 means model is smoothing, >1 means model is predicting more fluctuations than are observed)
sd(week1$forecast_mean_chl)/sd(week1$obs_chl_EXO)
sd(week2$forecast_mean_chl)/sd(week2$obs_chl_EXO)
sd(week1$forecast_median_chl)/sd(week1$obs_chl_EXO)
sd(week2$forecast_median_chl)/sd(week2$obs_chl_EXO)

sd(week1_new$forecast_mean_chl)/sd(week1_new$obs_chl_EXO)
sd(week2_new$forecast_mean_chl)/sd(week2_new$obs_chl_EXO)

# r2 calculation
1 - (sum(week1$residual^2)/sum(week1$data_minusglobalmean^2))
1 - (sum(week1_new$residual^2)/sum(week1_new$data_minusglobalmean^2))


plot(week1$forecast_mean_chl, week1$obs_chl_EXO)
abline(0, 1)
summary(lm(week1$forecast_mean_chl~week1$obs_chl_EXO))

plot(week2$forecast_mean_chl, week2$obs_chl_EXO)
abline(0,1)
summary(lm(week2$forecast_mean_chl~week2$obs_chl_EXO))


#null model
