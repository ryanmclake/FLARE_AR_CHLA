library(ggplot2)
install.packages('RColorBrewer')
library(RColorBrewer)
library(scales)

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"

daily <- read.csv(paste0(folder, '/FCR_forecasts/1day/daily_SW_discharge/ForecastMetrics_daily.csv'))
daily <- daily %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
daily$timestep <- 1
colnames(daily) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')

two_day <- read.csv(paste0(folder, '/FCR_forecasts/2day/2day_timestep_2day_lag_08Jul2020/ForecastMetrics_2days.csv'))
two_day <- two_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
two_day$timestep <- 2
colnames(two_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')

three_day <- read.csv(paste0(folder, '/FCR_forecasts/3day/11Jul2020/ForecastMetrics_3day.csv'))
three_day <- three_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
three_day$timestep <- 3
colnames(three_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')

four_day <- read.csv(paste0(folder, '/FCR_forecasts/4days/4day_timestep_4day_lag/ForecastMetrics_4days.csv'))
four_day <- four_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
four_day$timestep <- 4
colnames(four_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')

five_day <- read.csv(paste0(folder, '/FCR_forecasts/5day/12Jul2020/ForecastMetrics_5day.csv'))
five_day <- five_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
five_day$timestep <- 5
colnames(five_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')

seven_day <- read.csv(paste0(folder, '/FCR_forecasts/7day/weekly_dischargeforecast_Apr2020/ForecastMetrics_Weekly.csv'))
seven_day <- seven_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
seven_day$timestep <- 7
colnames(seven_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')

ten_day <- read.csv(paste0(folder, '/FCR_forecasts/10day/10Jul2020/ForecastMetrics_10day.csv'))
ten_day <- ten_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
ten_day$timestep <- 10
colnames(ten_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')

fourteen_day <- read.csv(paste0(folder, '/FCR_forecasts/14day/07Jul2020/ForecastMetrics_fortnightly.csv'))
fourteen_day <- fourteen_day %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_bloom)
fourteen_day$timestep <- 14
colnames(fourteen_day) <-  c('day_in_future', 'fullyear', 'nonbloom', 'bloom', 'timestep')

seven_day$day_in_future <- c(7,14)
fourteen_day$day_in_future <- c(14)

steps <- rbind(daily, two_day)
steps <- rbind(steps, three_day)
steps <- rbind(steps, four_day)
steps <- rbind(steps, five_day)
steps <- rbind(steps, seven_day)
steps <- rbind(steps, ten_day)
steps <- rbind(steps, fourteen_day)

ggplot(data = steps, aes(x = day_in_future, y = fullyear, col = timestep)) + 
  geom_point(size = 4) +
  scale_color_gradientn(colors = rainbow(20)) 

ggplot(data = steps, aes(x = day_in_future, y = nonbloom, col = timestep)) + 
  geom_point(size = 4) +
  scale_color_gradientn(colors = rainbow(8)) 

ggplot(data = steps, aes(x = day_in_future, y = bloom, col = timestep)) + 
  geom_point(size = 4) +
  scale_color_gradientn(colors = rainbow(8)) 
