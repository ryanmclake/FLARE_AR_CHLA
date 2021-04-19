# determine summary stats (median, standard deviation, max values) for observed chl-a during training period and during forecast period

library(tidyverse)


folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
timestep <- '14day'
sim_name <- '22Jul2020'
setwd(folder)

# training dataset May-Oct 2013-2016
train <- read.csv('./data_arima_WW.csv')
train$Date <- as.Date(train$Date)
train <- train %>% mutate(Chla = Chla_sqrt^2) %>% 
  mutate(Chla_EXO = ((Chla + 0.0308)/0.55))
train <- train %>% select(Date, Chla_EXO)

median(train$Chla_EXO)
sd(train$Chla_EXO)
plot(train$Date, train$Chla_EXO, xlab = 'Date', ylab = 'Chl-a (ug/L)')

# and over the forecasting period, Jan 2019-Jun 2020
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/', sim_na)
setwd(forecast_folder)

# code to read in the individual forecast files named for the day on which the forecast is made
myfiles_forecast <- list.files(path = forecast_folder, pattern = paste0('*', timestep, ".csv"))
dataset_forecast <- read.csv(paste0(forecast_folder, "/", myfiles_forecast[1]))

# read in files
for (i in 2:length(myfiles_forecast)) {
  temp_2 <- read.csv(paste0(forecast_folder,"/", myfiles_forecast[i]))
  dataset_forecast <- rbind(dataset_forecast, temp_2)
}

# some data arranging
stuff <- dataset_forecast 
stuff$forecast_date <- as.Date(stuff$forecast_date, "%Y-%m-%d")
stuff <- stuff[order(stuff$forecast_date),]
stuff$week <- as.factor(stuff$day_in_future)
stuff$forecast_run_day <- as.Date(stuff$forecast_run_day, "%Y-%m-%d")

stuff <- stuff %>% select(forecast_run_day, obs_chl_EXO)
colnames(stuff) <- c('Date', 'Chla_EXO')

# for summary stats, remove spin up dates, so anything before Dec 31, 2018
stuff <- stuff[stuff$Date>'2018-12-31',]

median(stuff$Chla_EXO)
sd(stuff$Chla_EXO)
max(stuff$Chla_EXO)

plot(stuff$Date, stuff$Chla_EXO, xlim = c(as.Date('2020-01-01'), as.Date('2020-05-01')), type = 'l')
abline(h = 17.1)
abline(v = as.Date('2020-03-16'))
abline(v = as.Date('2020-04-23'))

all <- rbind(train, stuff)
png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/SI_Fig_chl_timeseries.png', width = 1100, height = 800)
par(mar= c(5.1, 5.1, 4.1, 2.1))
plot(all$Date, all$Chla_EXO, xlab = 'Date', ylab = 'Chl-a (μg/L)', cex.axis = 2, cex.main = 2, cex.lab = 2, cex = 1.5)
dev.off()





