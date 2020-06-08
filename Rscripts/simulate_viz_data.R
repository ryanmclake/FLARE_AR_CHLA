library(tidyverse)

folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
timestep <- 'weekly' #specify the timestep
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/weekly_05Feb2020') #specify the folder within here
setwd(folder)

forecast <- read.csv(paste0(forecast_folder, '/2019_07_12_chla_weekly.csv'))
forecast$forecast_date <- as.Date(forecast$forecast_date)


plot(forecast$forecast_date, forecast$forecast_mean_chl, type = 'l', ylim = c(0, 20), xlab = "Date", ylab = "Chla (ug/L)")
points(forecast$forecast_date, forecast$forecast_CI95_lower, type = 'l', lty = 2 )
points(forecast$forecast_date, forecast$forecast_CI95_upper, type = 'l', lty = 2 )
points(forecast$forecast_date, forecast$forecast_mean_chl, type = 'p', col = 'orange', pch = 16, cex = 2)
legend('topleft', c('forecast mean', 'forecast confidence intervals'), col = c('orange', 'black'), pch = c(16,1), lty = c(1,2), bty = 'n')

# examine historical chl data to make some metric of a bloom
# as a temporary proxy for a 'bloom', use 3x (or 4x?) the standard deviation of the historical dataset for FCR
# this threshold is specific to FCR
# (x[2,,]^2)/0.55) + 0.0308
thresh <- read.csv('./data_arima_WW.csv')
thresh <- thresh %>% mutate(chl_EXOunits = ((Chla_sqrt^2)/0.55) + 0.0308   )
threshold <- 3*sd(thresh$chl_EXOunits)
mean(thresh$chl_EXOunits)
hist(thresh$chl_EXOunits)

set <- rnorm(420, mean = 6, sd = 4.2)
hist(set)
qnorm(set, )
pnorm(set, 13)
library(tigerstats)
pnormGC(13, mean = 11, sd = 4.2)

# want a distribution where 30% of observations are above 13
qnorm(0.7, mean = 11, sd = 4.2)
week1 <- rnorm(420, mean = 11, sd = 4.2)
hist(week1)

# find a distribution where 80% of observations are above 13
qnorm(0.2, mean = 17, sd = 4.2)
week2 <- rnorm(420, mean = 17, sd = 4.2)
hist(week2)


together <- rbind(week1, week2)
write.csv(together, './mock_data.csv', row.names = FALSE)

mean(together[1,])
mean(together[2,])
qnorm(0.975, mean(together[1,]), sd(together[1,]))
qnorm(0.025, mean(together[1,]), sd(together[1,]))

qnorm(0.975, mean(together[2,]), sd(together[2,]))
qnorm(0.025, mean(together[2,]), sd(together[2,]))

max(together[1,])
min(together[1,])

sim <- array( dim = c(4, 6))
colnames(sim) <- c('date', 'mean', 'upper', 'lower', 'max', 'min')
sim[,2] <- c(NA, 6, 10.7, 16.6)
sim[,3] <- c(NA, 6, 19.2, 24.2)
sim[,4] <- c(NA, 6, 2.2, 8.9)
sim[,5] <- c(NA, 6, max(together[1,]), max(together[2,]) )
sim[,6] <- c(NA, 6, 0, min(together[,2]))
sim[,1] <- c(as.Date('2019-05-03'), as.Date('2019-05-06', origin = "1970-01-01"), as.Date('2019-05-13', origin = "1970-01-01"), as.Date('2019-05-20', origin = "1970-01-01"))
sim <- as.data.frame(sim)

plot(as.Date(sim[,1], , origin = "1970-01-01"), sim[,2], type = 'l', ylim = c(0, 25), xlab = "Date", ylab = "Chla (ug/L)")
points(as.Date(sim[1,1], origin = "1970-01-01"), sim[1,2], col = 'red', pch = 16, cex = 2)
points(as.Date(sim[2,1] , origin = "1970-01-01"), sim[2,2], col = 'orange', pch = 16, cex = 2)
points(as.Date(sim[3,1], origin = "1970-01-01"), sim[3,2], col = 'orange', pch = 16, cex = 2)
points(as.Date(sim[,1], origin = "1970-01-01"), sim[,3], type = 'l', lty = 2)
points(as.Date(sim[,1], origin = "1970-01-01"), sim[,4], type = 'l', lty = 2)
legend('topleft', c('current Chla concentration', 'forecast mean', 'forecast confidence intervals'), col = c('red', 'orange', 'black'), cex = 1.2, pch = c(16, 16, 1), lty = c(0,0, 2), bty = 'n')


ggplot(data = sim, aes(x = as.Date(sim$date, origin = "1970-01-01"), y = sim$mean)) + geom_point()

png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Mock_ConfInt.png'), width = 1100, height = 800)
ggplot(sim, aes(x = as.Date(sim$date, origin = "1970-01-01"), y = sim$mean)) +
        geom_line(size = 2) +
        geom_point(aes(x = as.Date(sim$date, origin = "1970-01-01"), y = sim$mean), size = 8, stroke = 0, shape = 19, color = 'green4') +
        geom_ribbon(aes(ymin = upper, ymax = lower), fill = 'forestgreen', linetype = 2, alpha = 0.2) +
        xlab('Date') +
        ylab('Chlorophyll a (μg/L)') +
        theme(axis.text.x = element_text(size = 35),
              axis.text.y = element_text(size = 50),
              axis.title.x = element_text(size =45),
              axis.title.y = element_text(size = 45),
              legend.title = element_text(size = 35),
              legend.text = element_text(size = 30),
              panel.grid.major = element_blank(),
              legend.position = 'right',
              panel.grid.minor = element_blank())
dev.off()


png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Mock_MinMax.png'), width = 1100, height = 800)
ggplot(sim, aes(x = as.Date(sim$date, origin = "1970-01-01"), y = sim$mean)) +
  geom_line(size = 2) +
  geom_point(aes(x = as.Date(sim$date, origin = "1970-01-01"), y = sim$mean), size = 10, stroke = 0, shape = 19, color = 'green4') +
  geom_ribbon(aes(ymin = min, ymax = max), fill = 'forestgreen', linetype = 2, alpha = 0.2) +
  geom_vline(xintercept = as.numeric(as.Date("2019-05-06", "%Y-%m-%d")), size = 1.5) +
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  scale_x_date(date_labels = '%b-%d', breaks = as.Date(sim$date, origin = "1970-01-01"))+
  theme(axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        legend.position = 'right',
        panel.grid.minor = element_blank())
dev.off()


############################################################################################################################################################
### full ensemble plot

together$date <- c(as.Date())

mock <- read.csv('./mock_data.csv')
mock_long <- melt(mock)
colnames(mock_long) <- c('date', 'ensemble', 'value')
mock_long$date <- as.Date(mock_long$date)

plot(mock_long$date, mock_long$value, type = 'l')

png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Mock_AllEnsembles.png'), width = 1100, height = 800)
 ggplot(mock_long, aes(date, value)) +
  geom_path(col = 'grey28') +
   geom_point(x = as.Date('2019-03-09'), y = 6, col = 'green3', size = 14) +
   geom_point(x = as.Date('2019-03-16'), y = 10.7, col = 'green3', size = 14) +
   geom_point(x = as.Date('2019-03-23'), y = 16.6, col = 'green3', size = 14) +
   geom_vline(xintercept = as.numeric(as.Date("2019-03-09", "%Y-%m-%d")), size = 1.5) +
  xlab('Date') +
  ylab('Chlorophyll a (μg/L)') +
  scale_x_date(date_labels = '%b-%d', breaks = as.Date(sim$date, origin = "1970-01-01"))+
  theme(axis.text.x = element_text(size = 32),
        axis.text.y = element_text(size = 50),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        legend.position = 'right',
        panel.grid.minor = element_blank())
dev.off()

together
