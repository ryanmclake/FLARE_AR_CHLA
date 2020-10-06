
library(ggplot2)
library(patchwork)
library(scales)

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"

daily <- read.csv(paste0(folder, '/FCR_forecasts/1day/17Jul2020/ForecastMetrics_1dayrun23Sep20.csv'))
seven_day <- read.csv(paste0(folder, '/FCR_forecasts/7day/weekly_dischargeforecast_Apr2020/ForecastMetrics_7day.csv'))
fourteen_day <- read.csv(paste0(folder, '/FCR_forecasts/14day/22Jul2020/ForecastMetrics_14day.csv'))

par(mar = c(5,5,4,2), mfrow = c(2,2))
plot(daily$day_in_future, daily$RMSE_forecast, col = 'blue', cex = 3, ylim = c(0,11), xlim = c(0,14), main = 'Full-Year',  cex.axis = 2, cex.main = 2, cex.lab = 2, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(seven_day$day_in_future, seven_day$RMSE_forecast,  col = 'blue', pch = 15, cex = 3)
points(fourteen_day$day_in_future, fourteen_day$RMSE_forecast, col = 'blue', pch = 24, cex = 3)
legend('topleft', c('1 day', '7 day', '14 day' ), cex = 1.6, col = c('blue', 'blue', 'blue'),  pch = c( 1,  15,  17), bty = 'n')

plot(daily$day_in_future, daily$RMSE_forecast_nonbloom, col = 'blue', cex = 3, ylim = c(0,7), main = 'Non-bloom',  cex.axis = 2, cex.main = 2, cex.lab = 2, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(seven_day$day_in_future, seven_day$RMSE_forecast_nonbloom,  col = 'blue', pch = 15, cex = 3)
points(fourteen_day$day_in_future, fourteen_day$RMSE_forecast_nonbloom, col = 'blue', pch = 24, cex = 3)

plot(daily$day_in_future, daily$RMSE_forecast_bloom, col = 'blue', cex = 3, ylim = c(0,38), main = 'Bloom',  cex.axis = 2, cex.main = 2, cex.lab = 2, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(seven_day$day_in_future, seven_day$RMSE_forecast_bloom,  col = 'blue', pch = 15, cex = 3)
points(fourteen_day$day_in_future, fourteen_day$RMSE_forecast_bloom, col = 'blue', pch = 24, cex = 3)

metrics_overtime <- array(NA, dim = c(14,19))
colnames(metrics_overtime) <- c('day_in_future',
                               'daily', 'weekly', 'fortnightly',
                               'daily_null', 'weekly_null', 'fortnightly_null',
                               'daily_bloom', 'weekly_bloom', 'fortnightly_bloom',
                               'daily_bloom_null', 'weekly_bloom_null', 'fortnightly_bloom_null',
                               'daily_nonbloom', 'weekly_nonbloom', 'fortnightly_nonbloom',
                               'daily_nonbloom_null', 'weekly_nonbloom_null', 'fortnightly_nonbloom_null')


metrics_overtime <- as.data.frame(metrics_overtime)

metrics_overtime[7,3] <- seven_day[1,3] #week1 forecast rmse
metrics_overtime[7,15] <- seven_day[1,5] #week1 nonbloom rmse
metrics_overtime[7,9] <- seven_day[1,11] #week1 bloom RMSE

metrics_overtime[14,3] <- seven_day[2,3] #week2 forecast rmse
metrics_overtime[14,15] <- seven_day[2,5] #week2 nonbloom rmse
metrics_overtime[14,9] <- seven_day[2,11] #week2 forecast bloom weekly RMSE

metrics_overtime[7,6] <- seven_day[1,2]#week1 null rmse
metrics_overtime[7,18] <- seven_day[1,4]#week1 nonbloom null rmse
metrics_overtime[7,12] <- seven_day[1,10] #week1 null bloom weekly RMSE

metrics_overtime[14,6] <-seven_day[2,2] #week2 null rmse
metrics_overtime[14,18] <-seven_day[2,4]  #week2 nonbloom rmse
metrics_overtime[14,12] <- seven_day[2,10] #week2 null bloom weekly RMSE

# input 14 day metrics
metrics_overtime[14,4] <- fourteen_day[1,3] #fortnightly forecast
metrics_overtime[14,16] <- fourteen_day[1,5] #fortnightly forecast nonbloom
metrics_overtime[14,10] <- fourteen_day[1,11] #fortnightly forecast bloom

metrics_overtime[14,7] <- fourteen_day[1,2] #fortnightly null
metrics_overtime[14,19] <- fourteen_day[1,4] #fortnightly null nonbloom
metrics_overtime[14,13] <- fourteen_day[1,10] #fortnightly null bloom

# input daily metrics
metrics_overtime$day_in_future <- daily[1:14,1]
metrics_overtime$daily <- daily[1:14,3]
metrics_overtime$daily_null <- daily[1:14,2 ]
metrics_overtime$daily_bloom <- daily[1:14,11]
metrics_overtime$daily_bloom_null <- daily[1:14,10]
metrics_overtime$daily_nonbloom <- daily[1:14,5]
metrics_overtime$daily_nonbloom_null <- daily[1:14,4]

# and the bloom statistics
# a messy plot with different conditions on one plot
plot(metrics_overtime$day_in_future,   metrics_overtime$daily_null, col = 'red', ylim = c(0,11), xlab = 'forecast horizon', ylab = 'RMSE')
points(metrics_overtime$day_in_future, metrics_overtime$daily, col = 'blue')
points(metrics_overtime$day_in_future, metrics_overtime$daily_nonbloom, col = 'orange')
points(metrics_overtime$day_in_future, metrics_overtime$daily_nonbloom_null, col = 'purple')
points(metrics_overtime$day_in_future, metrics_overtime$weekly,  col = 'blue', pch = 15)
points(metrics_overtime$day_in_future, metrics_overtime$weekly_nonbloom, col = 'orange', pch = 15)
points(metrics_overtime$day_in_future, metrics_overtime$weekly_null, col = 'red', pch = 15)
points(metrics_overtime$day_in_future, metrics_overtime$weekly_nonbloom_null, col = 'purple', pch = 15)
legend('topleft', c('null', 'forecast', 'null nonbloom', 'forecast nonbloom'), col = c('red', 'blue', 'purple', 'orange'), lty = c(1,1), bty = 'n')


##################
# multi-panel figure
png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/RMSE_fig_1day7day14dayforecasts/RMSE_all_conditions_run23Sep20.png'), width = 1100, height = 800)
par(mar = c(5,5,4,2), mfrow = c(2,2))
plot(metrics_overtime$day_in_future, metrics_overtime$daily_nonbloom_null, col = 'red', pch =4, ylim = c(0,11), xlim = c(0,14), main = 'Non-Bloom', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 3, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(metrics_overtime$day_in_future, metrics_overtime$daily_nonbloom, col = 'blue', cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$weekly_nonbloom,  col = 'blue', pch = 0, cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$fortnightly_nonbloom, col = 'blue', pch = 2, cex = 3)
legend('topleft', c('null', 'daily', 'weekly', 'fortnightly' ), cex = 1.6, col = c('red', 'blue', 'blue', 'blue'), pch = c(4, 1,  0,  2), bty = 'n')

par(mar = c(5,5,4,2))
plot(metrics_overtime$day_in_future, metrics_overtime$daily_bloom_null, col = 'red', pch = 4, ylim = c(0,35), xlim = c(0,14), main = 'Bloom', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 3, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(metrics_overtime$day_in_future, metrics_overtime$daily_bloom, col = 'blue', cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$weekly_bloom,  col = 'blue', pch = 0, cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$fortnightly_bloom, col = 'blue', pch = 2, cex = 3)

par(mar = c(5,5,4,2))
plot(metrics_overtime$day_in_future, metrics_overtime$daily_null, col = 'red', pch = 4, xlim = c(0,14), ylim = c(0,11), main = '1.5 Years',  cex.axis = 2, cex.main = 2, cex.lab = 2, cex = 3, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(metrics_overtime$day_in_future, metrics_overtime$daily, col = 'blue', cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$weekly,  col = 'blue', pch = 0, cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$fortnightly, col = 'blue', pch = 2, cex = 3)

dev.off()

##################
# just forecasts (no null)
png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/fortnight_forecasts/RMSE_just_forecasts.png'), width = 1100, height = 800)
par(mar = c(5,5,4,2), mfrow = c(2,2))
plot(metrics_overtime$day_in_future, metrics_overtime$daily, col = 'blue', cex = 3, xlim = c(0,14), ylim = c(0,11), main = 'Full-Year',  cex.axis = 2, cex.main = 2, cex.lab = 2, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(metrics_overtime$day_in_future, metrics_overtime$weekly,  col = 'blue', pch = 15, cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$fortnightly, col = 'blue', pch = 17, cex = 3)
legend('topleft', c('daily', 'weekly', 'fortnightly' ), cex = 1.6, col = c('blue', 'blue', 'blue'),  pch = c( 1,  15,  17), bty = 'n')

par(mar = c(5,5,4,2))
plot(metrics_overtime$day_in_future, metrics_overtime$daily_nonbloom, col = 'blue', ylim = c(0,7), xlim = c(0,14), main = 'Non-Bloom', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 3, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(metrics_overtime$day_in_future, metrics_overtime$weekly_nonbloom,  col = 'blue', pch = 15, cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$fortnightly_nonbloom, col = 'blue', pch = 17, cex = 3)

par(mar = c(5,5,4,2))
plot(metrics_overtime$day_in_future, metrics_overtime$daily_bloom, col = 'blue', ylim = c(0,35), xlim = c(0,14), main = 'Bloom', cex.main = 2, cex.axis = 2, cex.lab = 2, cex = 3, xlab = 'Forecast horizon (days)', ylab = 'RMSE (μg/L)')
points(metrics_overtime$day_in_future, metrics_overtime$weekly_bloom,  col = 'blue', pch = 15, cex = 3)
points(metrics_overtime$day_in_future, metrics_overtime$fortnightly_bloom, col = 'blue', pch = 17, cex = 3)
dev.off()

###########################
# write file to csv

metrics_RMSE <- metrics_overtime#[,c(1,3,2, 5, 4, 20,21,10,14,12,16,18,19)] #select the values wanted for manuscript figure in the right order
metrics_RMSE <- metrics_RMSE[1:14,]
write.csv(metrics_RMSE, paste0(folder, '/FCR_forecasts/metrics_RMSE.csv'), row.names = FALSE)
