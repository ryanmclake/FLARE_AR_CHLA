library(ggplot2)
library(RColorBrewer)
library(scales)

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"

daily <- read.csv(paste0(folder, '/FCR_forecasts/1day/daily_SW_discharge/ForecastMetrics_daily.csv'))
daily <- daily %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
daily$timestep <- 1
for (i in 1:length(daily$day_in_future)) {
  daily$horizon[i] <- i
}
colnames(daily) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep', 'horizon')

two_day <- read.csv(paste0(folder, '/FCR_forecasts/2day/2day_timestep_2day_lag_08Jul2020/ForecastMetrics_2days.csv'))
two_day <- two_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
two_day$timestep <- 2
colnames(two_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(two_day$day_in_future)) {
  two_day$horizon[i] <- i
}

three_day <- read.csv(paste0(folder, '/FCR_forecasts/3day/11Jul2020/ForecastMetrics_3day.csv'))
three_day <- three_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
three_day$timestep <- 3
colnames(three_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(three_day$day_in_future)) {
  three_day$horizon[i] <- i
}

four_day <- read.csv(paste0(folder, '/FCR_forecasts/4days/4day_timestep_4day_lag/ForecastMetrics_4days.csv'))
four_day <- four_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
four_day$timestep <- 4
colnames(four_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(four_day$day_in_future)) {
  four_day$horizon[i] <- i
}

five_day <- read.csv(paste0(folder, '/FCR_forecasts/5day/12Jul2020/ForecastMetrics_5day.csv'))
five_day <- five_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
five_day$timestep <- 5
colnames(five_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(five_day$day_in_future)) {
  five_day$horizon[i] <- i
}

six_day <- read.csv(paste0(folder, '/FCR_forecasts/6day/13Jul2020/ForecastMetrics_6day.csv'))
six_day <- six_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
six_day$timestep <- 6
colnames(six_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(six_day$day_in_future)) {
  six_day$horizon[i] <- i
}

seven_day <- read.csv(paste0(folder, '/FCR_forecasts/7day/14Jul20_EXOdata_only/ForecastMetrics_7day.csv'))
seven_day <- seven_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
seven_day$timestep <- 7
colnames(seven_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(seven_day$day_in_future)) {
  seven_day$horizon[i] <- i
}

eight_day <- read.csv(paste0(folder, '/FCR_forecasts/8day/15Jul2020/ForecastMetrics_8day.csv'))
eight_day <- eight_day[eight_day$day_in_future<9,]
eight_day <- eight_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
eight_day$timestep <- 8
colnames(eight_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(eight_day$day_in_future)) {
  eight_day$horizon[i] <- i
}

nine_day <- read.csv(paste0(folder, '/FCR_forecasts/9day/15Jul2020/ForecastMetrics_9day.csv'))
nine_day <- nine_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
nine_day$timestep <- 9
colnames(nine_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(nine_day$day_in_future)) {
  nine_day$horizon[i] <- i
}

ten_day <- read.csv(paste0(folder, '/FCR_forecasts/10day/10Jul2020/ForecastMetrics_10day.csv'))
ten_day <- ten_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
ten_day$timestep <- 10
colnames(ten_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(ten_day$day_in_future)) {
  ten_day$horizon[i] <- i
}

eleven_day <- read.csv(paste0(folder, '/FCR_forecasts/11day/16Jul2020/ForecastMetrics_11day.csv'))
eleven_day <- eleven_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
eleven_day$timestep <- 11
colnames(eleven_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(eleven_day$day_in_future)) {
  eleven_day$horizon[i] <- i
}

fourteen_day <- read.csv(paste0(folder, '/FCR_forecasts/14day/07Jul2020/ForecastMetrics_fortnightly.csv'))
fourteen_day <- fourteen_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
fourteen_day$timestep <- 14
colnames(fourteen_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')
for (i in 1:length(fourteen_day$day_in_future)) {
  fourteen_day$horizon[i] <- i
}


steps <- rbind(daily, two_day)
steps <- rbind(steps, three_day)
steps <- rbind(steps, four_day)
steps <- rbind(steps, five_day)
steps <- rbind(steps, six_day)
steps <- rbind(steps, seven_day)
steps <- rbind(steps, eight_day)
steps <- rbind(steps, nine_day)
steps <- rbind(steps, ten_day)
steps <- rbind(steps, eleven_day)
steps <- rbind(steps, fourteen_day)

ggplot(data = steps, aes(x = day_in_future, y = fullyear, col = timestep)) + 
  geom_point(size = 4) +
  scale_color_gradientn(colors = rainbow(20)) 

ggplot(data = steps, aes(x = day_in_future, y = fullyear, size = timestep)) + 
  geom_point()
  
ggplot(data = steps, aes(x = day_in_future, y = nonbloom, col = timestep)) + 
  geom_point(size = 4) +
  scale_color_gradientn(colors = rainbow(8)) 

ggplot(data = steps, aes(x = day_in_future, y = bloom, col = timestep)) + 
  geom_point(size = 4) +
  scale_color_gradientn(colors = rainbow(8)) 

ggplot(data = steps, aes(x = timestep, y = fullyear, col = day_in_future)) +
  geom_point(size = 4) +
  scale_color_gradientn(colors = rainbow(20)) 

ggplot(data = steps, aes(x = horizon, y = fullyear, col = timestep)) +
  geom_point(size = 4) +
  scale_color_gradientn(colors = rainbow(20)) 



