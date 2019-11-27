# script to take appended arima forecast output and plot them and calculate some model performance statistics

library(rsq)
library(tidyverse)
library(Metrics)
library(lubridate)
reference_tzone <- "GMT"

#set the location of the forecasts
forecast_folder <- "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3"
setwd(forecast_folder)
# code to read in the individual forecast files named for the day on which the forecast is made
myfiles <- list.files(path = "./FCR_forecasts", pattern = "*weekly.csv")
dataset <- read.csv(paste0("./FCR_forecasts/", myfiles[1]))

# read in files
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0("./FCR_forecasts/", myfiles[i]))
  dataset <- rbind(dataset, temp)
}

mean(dataset$forecast_variance, na.rm = TRUE)

# some data arranging
stuff <- dataset 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$week)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")
# limit dataset to July 15 for the purposes of ESA materials
#stuff <- stuff[stuff$forecast_date<"2019-07-15",]

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

# week 1 and week 2 as separate vectors
ggplot(data = stuff, aes(x = forecast_date)) + geom_line(aes(y = forecast_mean_chl, color = week)) + geom_line(aes(y = obs_chl_EXO))

# residuals over time
ggplot(data = stuff, aes(x = forecast_date)) + 
  geom_line(aes(y = residual, color = week)) + 
  geom_vline(xintercept = as.POSIXct("2019-02-28", "%Y-%m-%d")) +
  geom_vline(xintercept = as.POSIXct("2019-03-20", "%Y-%m-%d"))

hist(stuff$residual)


# separate into week1 and week2 dataframes for separate analysis
week1 <- stuff[stuff$week==1,]
week2 <- stuff[stuff$week==2,]
week1 <- na.omit(week1)
week2 <- na.omit(week2)


hist(week1$residual)
hist(week2$residual)

xrange <- seq(1, 14)
plot(7, mean(week1$residual), xlim = c(1,14), ylim = c(-0.7, 0.9), ylab = 'Forecast - Observed (ug/L)', xlab = 'Days in future')
points(14, mean(week2$residual))
abline(h = 0)


png("./Figures/ConfIntForecasts2019.png", width = 1016, height = 800)
confrange <- range(week1$forecast_CI95_upper, week1$forecast_CI95_lower, na.rm = TRUE)
par(mar = c(7,6,4,1)+0.1)
plot(week1$forecast_date, week1$forecast_mean_chl, type = 'l', ylim = confrange, ylab = 'Chla (ug/L)', lwd = 2, xlab = 'Date', cex.axis = 3, cex.lab = 3)
points(week1$forecast_date, week1$forecast_CI95_upper, type = 'l', lty = 2, lwd = 2)
points(week1$forecast_date, week1$forecast_CI95_lower, type = 'l', lty = 2, lwd = 2)
points(week1$forecast_date, week1$obs_chl_EXO, col = 'red', type = 'p', lwd = 2)
abline(v = as.Date("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.Date("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
legend('topright', c('forecast mean', '95% CI', 'observed'), lty = c(1, 2, 1), col = c('black', 'black', 'red'), cex = 2)
dev.off()

png("./Figures/ConfIntForecasts2019_noabline.png", width = 1016, height = 800)
confrange <- range(week1$forecast_CI95_upper, week1$forecast_CI95_lower, na.rm = TRUE)
par(mar = c(7,6,4,1)+0.1)
plot(week1$forecast_date, week1$forecast_mean_chl, type = 'l', ylim = confrange, ylab = 'Chla (ug/L)', lwd = 2, xlab = 'Date', cex.axis = 3, cex.lab = 3)
points(week1$forecast_date, week1$forecast_CI95_upper, type = 'l', lty = 2, lwd = 2)
points(week1$forecast_date, week1$forecast_CI95_lower, type = 'l', lty = 2, lwd = 2)
points(week1$forecast_date, week1$obs_chl_EXO, col = 'red', type = 'p', lwd = 2)
legend('topright', c('forecast mean', '95% CI', 'observed'), lty = c(1, 2, 1), col = c('black', 'black', 'red'), cex = 2)
dev.off()

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/ESA_ConfIntForecasts2019_withabline_withIC.png', width = 1100, height = 800)
ggplot(week1, aes(forecast_date, forecast_mean_chl)) +
  geom_line(size = 2) +
  geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'grey45') +
  geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), linetype = 2, alpha = 0.2) +
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  geom_vline(xintercept = as.numeric(as.Date("2019-02-28", "%Y-%m-%d")), color = 'blue', size = 1.5) +
  geom_vline(xintercept = as.numeric(as.Date("2019-03-20", "%Y-%m-%d")), color = 'blue', size = 1.5) +
  theme(axis.text.x = element_text(size = 50),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        legend.position = 'right',
        panel.grid.minor = element_blank())
        #panel.background = element_rect(fill = '#9DC3E6', color = '#9DC3E6'),
        #plot.background = element_rect(fill = '#9DC3E6', color = '#9DC3E6'))
dev.off()

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/ESA_ConfIntForecasts2019_noabline_Oct.png', width = 1100, height = 800)
ggplot(week1, aes(forecast_date, forecast_mean_chl)) +
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
dev.off()

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/ESA_ConfIntForecasts2019_forecastonly.png', width = 1100, height = 800)
ggplot(week1, aes(forecast_date, forecast_mean_chl)) +
  geom_line(size = 2) +
  #geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'grey45') +
  geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), linetype = 2, alpha = 0.2) +
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  #geom_vline(xintercept = as.numeric(as.Date("2019-02-28", "%Y-%m-%d")), color = 'blue', size = 1.5) +
  #geom_vline(xintercept = as.numeric(as.Date("2019-03-20", "%Y-%m-%d")), color = 'blue', size = 1.5) +
  theme(axis.text.x = element_text(size = 50),
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
dev.off()



plot(week1$forecast_date, week1$obs_chl_EXO)
abline(v = as.Date("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.Date("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
points(week1$forecast_date, week1$forecast_mean_chl, col = 'red', type = 'l')

# and calculate what percentage of obs data is between the conf intervals
week1 <- week1 %>% mutate(CI95_yesno = ifelse(obs_chl_EXO<forecast_CI95_upper & obs_chl_EXO>forecast_CI95_lower, 1, 0))
(nrow(week1[week1$CI95_yesno==0,])/nrow(week1))*100
plot(week1$forecast_date, week1$CI95_yesno)
abline(v = as.POSIXct("2019-02-28", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate
abline(v = as.POSIXct("2019-03-20", "%Y-%m-%d"), col = 'blue') # this is when the reservoir was dosed with copper sulfate

#############################################################################################################################################################
####### calculate forecast assessment metrics ##########################################################################################################
#############################################################################################################################################################

# bring in the null model to calculate the same metrics
null <- read.csv("./FCR_forecasts/null_model/null_model_lagplusprocesserror.csv")
colnames(null)[1] <- 'forecast_date'
null$forecast_date <- as.Date(null$forecast_date)

# remove the days on/after copper sulfate dosing because this is outside of the ability of the model to anticipate
week1$forecast_date <- as.Date(week1$forecast_date)

up1 <- week1[week1$forecast_date<"2019-02-28",]
down1 <- week1[week1$forecast_date>"2019-03-20",]
week1 <- rbind(up1, down1)

up2 <- week2[week2$forecast_date<"2019-02-28",]
down2 <- week2[week2$forecast_date>"2019-03-20",]
week2 <- rbind(up2, down2)

# merge datamerge with week1 and week2 to align the null model with the forecast
week1 <- left_join(week1, null)
week2 <- left_join(week2, null)



# plot of pred vs obs now that the copper sulfate days have been taken out
plot(week1$obs_chl_EXO, week1$forecast_mean_chl, ylab = 'Forecast Mean Chla (μg/L)', xlab = 'Observed Chla (μg/L)', main = 'Week 1 Forecast')
abline(0, 1)
summary(lm(week1$obs_chl_EXO~week1$forecast_mean_chl))

plot(week2$obs_chl_EXO, week2$forecast_mean_chl, ylab = 'Forecast Mean Chla (μg/L)', xlab = 'Observed Chla (μg/L)', main = 'Week 2 Forecast')
abline(0,1)
summary(lm(week2$obs_chl_EXO~ week2$forecast_mean_chl))


# pearson's correlation
cor(week1$forecast_mean_chl, week1$obs_chl_EXO, method = 'pearson')
cor(week2$forecast_mean_chl, week2$obs_chl_EXO, method = 'pearson')
cor(week1$null_week1, week1$obs_chl_EXO, method = 'pearson')
cor(week2$null_week2, week2$obs_chl_EXO, method = 'pearson')


# rmse for mean of ensembles
rmse(week1$forecast_mean_chl, week1$obs_chl_EXO)
rmse(week2$forecast_mean_chl, week2$obs_chl_EXO)
rmse(week1$null_week1, week1$obs_chl_EXO)
rmse(week2$null_week2, week2$obs_chl_EXO)

# rmse for median of ensembles
rmse(week1$forecast_median_chl, week1$obs_chl_EXO)
rmse(week2$forecast_median_chl, week2$obs_chl_EXO)

# bias calculations for mean and median of both weeks (1 is non-biased, <1 means model is smoothing, >1 means model is predicting more fluctuations than are observed)
sd(week1$forecast_mean_chl)/sd(week1$obs_chl_EXO)
sd(week2$forecast_mean_chl)/sd(week2$obs_chl_EXO)
sd(week1$null_week1)/sd(week1$obs_chl_EXO)
sd(week2$null_week2)/sd(week2$obs_chl_EXO)
sd(week1$forecast_median_chl)/sd(week1$obs_chl_EXO)
sd(week2$forecast_median_chl)/sd(week2$obs_chl_EXO)


# r2 calculation, variation from the 1:1 line
# calculate the data minus global mean now that the copper sulfate days are removed
week1 <- week1 %>% mutate(data_minusglobalmean = obs_chl_EXO - mean(obs_chl_EXO, na.rm = TRUE)) %>% 
  mutate(residual_null = null_week1 - exo_chl_ugL)

1 - (sum(week1$residual^2)/sum(week1$data_minusglobalmean^2))
1 - (sum(week1$residual_null^2)/sum(week1$data_minusglobalmean^2))

week2 <- week2 %>% mutate(data_minusglobalmean = obs_chl_EXO - mean(obs_chl_EXO, na.rm = TRUE)) %>% 
  mutate(residual_null = null_week2 - exo_chl_ugL)
1 - (sum(week2$residual^2)/sum(week2$data_minusglobalmean^2))
1 - (sum(week2$residual_null^2)/sum(week2$data_minusglobalmean^2))


#r2 as a linear model
summary(lm(week1$forecast_mean_chl~week1$obs_chl_EXO))
summary(lm(week1$null_week1~week1$obs_chl_EXO))
summary(lm(week2$forecast_mean_chl~week2$obs_chl_EXO))
summary(lm(week2$null_week2~week2$obs_chl_EXO))

# bias or mean residual
mean(week1$residual)
mean(week1$residual_null)

mean(week2$residual)
mean(week2$residual_null)
plot(week2$forecast_date, week2$residual_null)
hist(week2$residual_null)
plot(week1$forecast_date, week1$residual_null)
hist(week1$residual_null)


#null model

plot(week1$forecast_date, week1$residual, type = 'l', col = 'blue')
points(week1$forecast_date, week1$residual_null, type = 'l', col = 'red')
plot(week2$forecast_date, week2$residual, type = 'l', col = 'blue')
points(week2$forecast_date, week2$residual_null, type = 'l', col = 'red')
