# script to create a null persistence model and add process uncertainty propagated across the forecast horison 
# to compare to the forecast output

library(tidyverse)
library(lubridate)
folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"

source(paste0(folder,"/","Rscripts/extract_EXOchl_chain_dailyavg.R")) #this is the original file modified to take a daily avg rather than the midnight reading
data_location = "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3/SCCData"
temperature_location <- paste0(data_location, "/", "mia-data")
# specify full_time as the entire time series from 08-15-2018 to -7-15-2019
full_time <- seq(as.Date("2018-08-01"), as.Date("2019-07-15"), by = "1 day")
observed_depths_chla_fdom <- 1
temp_obs_fname <- "Catwalk.csv"
temp_obs_fname_wdir <- paste0(temperature_location, "/", temp_obs_fname) 
reference_tzone <- "GMT"

chla_obs <- extract_chla_chain_dailyavg(fname = temp_obs_fname_wdir,
                                        full_time,
                                        depths = 1.0,
                                        observed_depths_chla_fdom = observed_depths_chla_fdom,
                                        input_tz = "EST5EDT", 
                                        output_tz = reference_tzone)



data <- as.data.frame(full_time)
data2 <- as.data.frame(chla_obs[[1]][,1])
datamerge <- cbind(data, data2)
colnames(datamerge) <- c("date", "exo_chl_ugL")
datamerge$exo_chl_ugL[is.nan(datamerge$exo_chl_ugL)] <- NA
datamerge <- datamerge %>% mutate(week_lag_chl = lag(exo_chl_ugL, 7L)) %>% mutate(week2_lag_chl = lag(exo_chl_ugL, 14L))

# to create the null model, take the observed chl values and add process error
# read in the jags file to calculate the process error to add to the weekly lags for the null model
load("C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3/MCMC_output_ARIMA_Whitney.Rdata")
for(j in 1:nrow(datamerge)){
  
  p <- sample(seq(1,length(samples[[1]][,1])), 1, replace = TRUE) #changed 0 to 1 7/16
  datamerge[j, 5] <- samples[[1]][p,5]
}

# now add the process error to the lag for week 1 and week 2
datamerge <- datamerge %>% mutate(null_week1 = week_lag_chl + V5) %>% 
  mutate(null_week2 = week2_lag_chl + V5)


plot(datamerge$date, datamerge$exo_chl_ugL, ylab = 'Chla (ug/L)', lwd = 2, xlab = 'Date', cex.axis = 3, cex.lab = 3)
points(datamerge$date, datamerge$null_week1, type = 'l', col = 'red')

#png("./Figures/NullModelLag.png", width = 1016, height = 800)
par(mar = c(7,6,4,1)+0.1)
plot(datamerge$date, datamerge$exo_chl_ugL, ylab = 'Chla (ug/L)', lwd = 2, xlab = 'Date', cex.axis = 3, cex.lab = 3)
points(datamerge$date, datamerge$null_week1, type = 'l', col = 'red', lwd = 2)
points(datamerge$date, datamerge$null_week2, type = 'l', col = 'orange', lwd = 2)
legend('topright', c('obs chl', 'week1 null', 'week2 null'), col = c('black', 'red', 'orange'), lty = c(1,1), bty = 'n', cex = 1.5)
#dev.off()

# write the null model to csv to bring in to plot_arima code to calculate assessment stats
write.csv(datamerge, './FCR_forecasts/null_model/null_model_lagplusprocesserror.csv', row.names = FALSE)


