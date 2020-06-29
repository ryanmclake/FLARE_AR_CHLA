# compare forecasts from no DA simulation and normal DA 


no_da <- read.csv('./FCR_forecasts/weekly/no_DA/ForecastMetrics_Weekly.csv')
da <- read.csv('./FCR_forecasts/weekly/no_DA/ForecastMetrics_Weekly_withDA.csv')

plot(no_da$week_in_future, no_da$RMSE_forecast)

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/no_DA_figs/compare_DA_RMSE.png', width = 1100, height = 800)
par(mar = c(5,5,4,2), mfrow = c(2,2))
plot(no_da$week_in_future, no_da$RMSE_forecast, col = 'red', xlim = c(0,3), ylim = c(0,18), main = 'Jun-Oct 2019',  pch = 15, cex.axis = 2, cex.main = 2, cex.lab = 2, cex = 3, xlab = 'Week', ylab = 'RMSE (μg/L)')
points(da$week_in_future, da$RMSE_forecast, col = 'blue', cex = 3, pch = 15)
points(no_da$week_in_future, no_da$RMSE_null,  col = 'red', cex = 3)
points(da$week_in_future, da$RMSE_null, col = 'blue',  cex = 3)
legend('bottomleft', c('no DA forecast', 'DA forecast', 'no DA null', 'DA null'), cex = 1.6, col = c('red', 'blue', 'red', 'blue'), pch = c(15, 15,  1, 1), bty = 'n')

plot(no_da$week_in_future, no_da$RMSE_forecast_nonbloom, col = 'red', xlim = c(0,3), ylim = c(0,18), main = 'Non-Bloom',  pch = 15, cex.axis = 2, cex.main = 2, cex.lab = 2, cex = 3, xlab = 'Week', ylab = 'RMSE (μg/L)')
points(da$week_in_future, da$RMSE_forecast_nonbloom, col = 'blue', cex = 3, pch = 15)
points(no_da$week_in_future, no_da$RMSE_null_nonbloom,  col = 'red', cex = 3)
points(da$week_in_future, da$RMSE_null_nonbloom, col = 'blue',  cex = 3)

plot(no_da$week_in_future, no_da$RMSE_forecast_bloom, col = 'red', xlim = c(0,3), ylim = c(0,40), main = 'Bloom',  pch = 15, cex.axis = 2, cex.main = 2, cex.lab = 2, cex = 3, xlab = 'Week', ylab = 'RMSE (μg/L)')
points(da$week_in_future, da$RMSE_forecast_bloom, col = 'blue', cex = 3, pch = 15)
points(no_da$week_in_future, no_da$RMSE_null_bloom,  col = 'red', cex = 3)
points(da$week_in_future, da$RMSE_null_bloom, col = 'blue',  cex = 3)
dev.off()
