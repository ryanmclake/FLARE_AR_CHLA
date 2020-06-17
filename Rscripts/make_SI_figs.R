# make SI figure comparing daily forecasts with different covariates

swd <- read.csv('./FCR_forecasts/daily/daily_SW_discharge/dailyforecast_SWdischarge_metrics.csv')
rh <- read.csv('./FCR_forecasts/daily/relhum_training_spinup_Feb0420/dailyforecast_relhum_metrics.csv')

swd <- swd %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_daily_bloom)
colnames(swd) <- c('day_in_future', 'RMSE_swd', 'RMSE_swd_nonbloom', 'RMSE_swd_bloom')

rh <- rh %>% select(day_in_future, RMSE_forecast, RMSE_forecast_nonbloom, RMSE_forecast_daily_bloom)
colnames(rh) <- c('day_in_future', 'RMSE_rh', 'RMSE_rh_nonbloom', 'RMSE_rh_bloom')

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/SI_Fig_RMSE_daily_rh_swd.png', width = 1100, height = 800)
par(mar = c(5,5,4,2), mfrow = c(2,2))
plot(swd$day_in_future, swd$RMSE_swd, col = 'red', type = 'p', xlim = c(0,14), ylim = c(0,11), main = 'Full-Year',  cex.axis = 2, cex.main = 2, cex.lab = 2, cex = 3, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(rh$day_in_future, rh$RMSE_rh, col= 'blue', type = 'p', cex = 3)
legend('topleft', c('daily RH', 'daily SW + discharge'), cex = 1.6, col = c('blue', 'red'), pch = c(1,1), bty = 'n')

plot(swd$day_in_future, swd$RMSE_swd_nonbloom, col = 'red', type = 'p', ylim = c(0, 8), xlim = c(0,14), main = 'Non-Bloom', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 3, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(rh$day_in_future, rh$RMSE_rh_nonbloom, col= 'blue', type = 'p', cex = 3)

plot(swd$day_in_future, swd$RMSE_swd_bloom, col = 'red', type = 'p', ylim = c(0, 40), main = 'Bloom', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 3, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(rh$day_in_future, rh$RMSE_rh_bloom, col= 'blue', type = 'p', cex = 3)
dev.off()

#####################################################################################################################################
# make SI figure of weekly training and forecast chl time series
data <- read.csv('./data_arima_working.csv')
data$Date <- as.Date(data$Date)
par(mfrow = c(1,1))

# plot the data un-sqrt-transformed and in EXO units
plot(data$Date, ((data$Chla_sqrt + 0.0308)/0.55)^2)
