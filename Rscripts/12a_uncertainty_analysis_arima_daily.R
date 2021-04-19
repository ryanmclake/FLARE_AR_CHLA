# script to analyze different sources of uncertainty for arima forecast output
library(rsq)
library(tidyverse)
library(Metrics)
library(lubridate)
library(scales)
reference_tzone <- "GMT"

#set the location of the forecasts
folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
sim_name <- 'Mar2021_UC'
forecast_folder <- paste0(folder, "/FCR_forecasts", '/1day/', sim_name)

# the year's forecasts with all uncertainties 'on' in the forecast output
myfiles <- list.files(path = forecast_folder, pattern = "*1day.csv")
dataset <- read.csv(paste0(forecast_folder, "/", myfiles[1]))

# read in files
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0(forecast_folder, "/", myfiles[i]))
  dataset <- rbind(dataset, temp)
}

# Error in rbind(deparse.level, ...) : numbers of columns of arguments do not match
# occurs because last file name is the Forecast_Metrics.csv summary file, ignore this error!

# read in each batch on uncertainty files
################ uncertainty 2 = process uncertainty
myfiles_2 <- list.files(path = paste0(forecast_folder, "/uncertainty_2_process/"), pattern = "*1day.csv")
dataset_2 <- read.csv(paste0(forecast_folder, "/uncertainty_2_process/", myfiles_2[1]))
# read in files
for (i in 2:length(myfiles_2)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_2_process/", myfiles_2[i]))
  dataset_2 <- rbind(dataset_2, temp)
}

############### uncertainty 3 = weather uncertainty
myfiles_3 <- list.files(path = paste0(forecast_folder, "/uncertainty_3_weather/"), pattern = "*1day.csv")
dataset_3 <- read.csv(paste0(forecast_folder, '/uncertainty_3_weather/', myfiles_3[1]))
# read in files
for (i in 2:length(myfiles_3)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_3_weather/", myfiles_3[i]))
  dataset_3 <- rbind(dataset_3, temp)
}

############### uncertainty 4 = initial condition uncertainty
myfiles_4 <- list.files(path = paste0(forecast_folder, "/uncertainty_4_initial_condition/"), pattern = "*1day.csv")
dataset_4 <- read.csv(paste0(forecast_folder, "/uncertainty_4_initial_condition/", myfiles_4[1]))
# read in files
for (i in 2:length(myfiles_4)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_4_initial_condition/", myfiles_4[i]))
  dataset_4 <- rbind(dataset_4, temp)
}

############### uncertainty 5 = parameter uncertainty
myfiles_5 <- list.files(path = paste0(forecast_folder, "/uncertainty_5_parameter"), pattern = "*1day.csv")
dataset_5 <- read.csv(paste0(forecast_folder, "/uncertainty_5_parameter/", myfiles_5[1]))
# read in files
for (i in 2:length(myfiles_5)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_5_parameter/", myfiles_5[i]))
  dataset_5 <- rbind(dataset_5, temp)
}

############### uncertainty 5 = parameter uncertainty
myfiles_6 <- list.files(path = paste0(forecast_folder, "/uncertainty_6_discharge"), pattern = "*1day.csv")
dataset_6 <- read.csv(paste0(forecast_folder, "/uncertainty_6_discharge/", myfiles_6[1]))
# read in files
for (i in 2:length(myfiles_6)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_6_discharge/", myfiles_6[i]))
  dataset_6 <- rbind(dataset_6, temp)
}

plot(dataset$forecast_date, dataset$forecast_variance)
plot(as.Date(dataset_2$forecast_date), dataset_2$forecast_variance, xlim = c(as.Date('2020-06-11'), as.Date('2020-06-25')), type = 'l')
plot(as.Date(dataset_2$forecast_date), dataset_2$forecast_mean_chl)
plot(as.Date(dataset_2$forecast_date), dataset_2$obs_chl_EXO)
hist(dataset_5$forecast_variance)
ggplot(data = dataset_2, aes(x = as.Date(forecast_date), y = forecast_variance)) +
  geom_point(aes(col = forecast_run_day)) +
  xlim(c(as.Date('2020-06-11'), as.Date('2020-06-25'))) +
  theme(legend.title = element_blank())

ggplot(data = dataset_3, aes(x = as.Date(forecast_date), y = forecast_variance)) +
  geom_line() +
  facet_wrap(~day_in_future)

ggplot(data = dataset_4, aes(x = as.Date(forecast_date), y = forecast_variance)) +
  geom_line() +
  facet_wrap(~day_in_future)

ggplot(data = dataset_5, aes(x = as.Date(forecast_date), y = forecast_variance)) +
  geom_line() +
  facet_wrap(~day_in_future)

ggplot(data = dataset_6, aes(x = as.Date(forecast_date), y = sqrt(forecast_variance)*0.6 + 0.25)) +
  geom_line() +
  facet_wrap(~day_in_future)

process <- mean(dataset_2$forecast_variance, na.rm = TRUE)
weather <- mean(dataset_3$forecast_variance, na.rm = TRUE)
IC <- mean(dataset_4$forecast_variance, na.rm = TRUE)
parameter <- mean(dataset_5$forecast_variance, na.rm = TRUE)
discharge <- mean(dataset_6$forecast_variance, na.rm = TRUE)
all <- process+weather+IC+parameter+discharge

# what amount of variance are we not accounting for in our uncertainty analysis?
(mean(dataset$forecast_variance, na.rm = TRUE) - all)/mean(dataset$forecast_variance, na.rm = TRUE)


# some data wrangling to get all the variance columns in the same dataframe
dataset <- dataset %>% select(forecast_date, forecast_run_day, day_in_future, forecast_variance)
dataset$forecast_date <- as.Date(dataset$forecast_date)
dataset$forecast_run_day <- as.Date(dataset$forecast_run_day)

dataset_2 <- dataset_2 %>% select(forecast_date, forecast_run_day, day_in_future, forecast_variance)
colnames(dataset_2) <- c('forecast_date', 'forecast_run_day', 'day_in_future',  'process_variance')
dataset_2$forecast_date <- as.Date(dataset_2$forecast_date)
dataset_2$forecast_run_day <- as.Date(dataset_2$forecast_run_day)

dataset_3 <- dataset_3 %>% select(forecast_date, forecast_run_day, day_in_future,  forecast_variance)
colnames(dataset_3) <- c('forecast_date', 'forecast_run_day', 'day_in_future',  'weather_variance')
dataset_3$forecast_date <- as.Date(dataset_3$forecast_date)
dataset_3$forecast_run_day <- as.Date(dataset_3$forecast_run_day)

dataset_4 <- dataset_4 %>% select(forecast_date, forecast_run_day, day_in_future,  forecast_variance)
colnames(dataset_4) <- c('forecast_date', 'forecast_run_day', 'day_in_future',   'IC_variance')
dataset_4$forecast_date <- as.Date(dataset_4$forecast_date)
dataset_4$forecast_run_day <- as.Date(dataset_4$forecast_run_day)

dataset_5 <- dataset_5 %>% select(forecast_date, forecast_run_day, day_in_future,  forecast_variance)
colnames(dataset_5) <- c('forecast_date', 'forecast_run_day', 'day_in_future',   'parameter_variance')
dataset_5$forecast_date <- as.Date(dataset_5$forecast_date)
dataset_5$forecast_run_day <- as.Date(dataset_5$forecast_run_day)

dataset_6 <- dataset_6 %>% select(forecast_date, forecast_run_day, day_in_future,  forecast_variance)
colnames(dataset_6) <- c('forecast_date', 'forecast_run_day', 'day_in_future',   'discharge_variance')
dataset_6$forecast_date <- as.Date(dataset_6$forecast_date)
dataset_6$forecast_run_day <- as.Date(dataset_6$forecast_run_day)

uncert <- left_join(dataset_2, dataset_3, by = c('forecast_date', 'forecast_run_day', 'day_in_future'))
uncert <- left_join(uncert, dataset_4)
uncert <- left_join(uncert, dataset_5)
uncert <- left_join(uncert, dataset_6)
uncert <- left_join(uncert, dataset)

uncert_untransform <- uncert %>% #try undoing the CTD/EXO sqrt transformation
  mutate(process_variance = (sqrt(process_variance)*0.6 + 0.25)) %>% 
  mutate(weather_variance = (sqrt(weather_variance)*0.6 + 0.25)) %>% 
  mutate(IC_variance = (sqrt(IC_variance)*0.6 + 0.25)) %>% 
  mutate(parameter_variance = (sqrt(parameter_variance)*0.6 + 0.25)) %>% 
  mutate(discharge_variance = (sqrt(discharge_variance)*0.6 + 0.25)) 
  
# calculate the proportion of variance for all of the uncertainties
uncert <- uncert %>% mutate(process_prop = process_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>% 
  mutate(weather_prop = weather_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>% 
  mutate(IC_prop = IC_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>% 
  mutate(parameter_prop = parameter_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>%
  mutate(discharge_prop = discharge_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>% 
  mutate(total_var = parameter_variance + process_variance + weather_variance + IC_variance + discharge_variance)


# dataframe with just proportions
uncert_prop <- uncert %>% select(forecast_date, forecast_run_day, day_in_future, process_prop, weather_prop, IC_prop, parameter_prop, discharge_prop, total_var)

# remove dates after 15-Aug
uncert_prop <- uncert_prop[uncert_prop$forecast_run_day<"2020-08-15",]

## put into long format for easy plotting
uncert_proportion_long <- uncert_prop  %>% 
  gather(variable, measurement, process_prop:total_var)

##########################################################################################################################################################################
##### make some figures
##########################################################################################################################################################################

# simple line graph
for (i in 1:16) { # change the 16 to whatever number of timesteps you have
  temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==i,]
  print(ggplot(temp, aes(x = forecast_date, y = measurement, color = variable)) +
          geom_line() +
          ggtitle(paste0('Day ', i, ' Forecast')))
  
}


# pretty time series with geom_area()
for (i in 1:14) {
  temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==i,]
  temp <- temp[!temp$variable=='total_var',]
  png(paste0(forecast_folder, '/Daily_Day',i, '_Uncertainty_TimeSeries.png'), width = 1200, height = 785)
  print(ggplot(temp, aes(x = forecast_date, y = measurement, fill = variable)) +geom_area() + ylim(0,1.1) +
          xlab('Date') +
          ylab('Proportion of Variance') +
          scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                            values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                            name = "Uncertainty Type") +
          ggtitle(paste0('Day ', i, ' Daily Forecast')) +
          scale_x_date(labels = date_format('%b'), expand = c(0,0)) +
          scale_y_continuous(expand = c(0,0))+
          theme_bw() + 
          theme(axis.text.x = element_text(size = 40),
                axis.text.y = element_text(size = 40),
                axis.title.x = element_text(size =45),
                axis.title.y = element_text(size = 45),
                legend.title = element_text(size = 35),
                legend.text = element_text(size = 30),
                #panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(size = 50),
                legend.position = 'none')
  )
  dev.off() 
}
 



###############################################
## Figure 6 with total variance overplotted
# Day 1

temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==1,]
temp <- temp[temp$forecast_run_day>'2019-01-01',]
var <- temp[temp$variable=='total_var',]
temp <- temp[!temp$variable=='total_var',]

p <- ggplot() 
p <- p + geom_area(data = temp, aes(x = forecast_date, y = measurement, fill = variable))+ 
  xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  ggtitle(paste0('Daily Forecast, Day 1')) +
  scale_x_date(labels = date_format('%b'), expand = c(0,0), date_breaks = '3 months') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.title.x = element_text(size =35),
        axis.title.y = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 50),
        legend.position = 'none')
p <- p + geom_line(data = var, aes(x = forecast_date, y = measurement), lwd = 1.5) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~., name = expression (paste("Total Variance (",~μg/L^2,")"  )))) 
png(paste0(forecast_folder, '/Daily_Day',1, '_Uncertainty_Variance_TimeSeries.png'), width = 1200, height = 785)
p
dev.off()



# Day 7

temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==7,]
var <- temp[temp$variable=='total_var',]
temp <- temp[!temp$variable=='total_var',]

p <- ggplot() 
p <- p + geom_area(data = temp, aes(x = forecast_date, y = measurement, fill = variable))+ 
  xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  ggtitle(paste0('Daily Forecast, Day 7')) +
  scale_x_date(labels = date_format('%b'), expand = c(0,0), date_breaks = '3 months') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.title.x = element_text(size =35),
        axis.title.y = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 50),
        legend.position = 'none')
p <- p + geom_line(data = var, aes(x = forecast_date, y = measurement), lwd = 1.5) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~., name = expression (paste("Total Variance (",~μg/L^2,")"  )))) 
png(paste0(forecast_folder, '/Daily_Day',7, '_Uncertainty_Variance_TimeSeries.png'), width = 1200, height = 785)
p
dev.off()

# Day 14
temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==14,]
var <- temp[temp$variable=='total_var',]
temp <- temp[!temp$variable=='total_var',]

p <- ggplot() 
p <- p + geom_area(data = temp, aes(x = forecast_date, y = measurement, fill = variable))
p <- p + geom_line(data = var, aes(x = forecast_date, y = measurement/1.7), lwd = 1.5) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~.*1.7, name = expression (paste("Total Variance (",~μg/L^2,")"  ))))
p <- p + xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  ggtitle(paste0('Daily Forecast, Day 14')) +
  scale_x_date(labels = date_format('%b'), expand = c(0,0), date_breaks = '3 months' ) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.title.x = element_text(size =35),
        axis.title.y = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 50),
        legend.position = 'none')
png(paste0(forecast_folder, '/Daily_Day',14, '_Uncertainty_Variance_TimeSeries.png'), width = 1200, height = 785)
p
dev.off()

#######################################################################################################################################################################
# stacked bar plots of proportion variance for each forecast horizon
# create dataframe to write into inside the loop
mean_prop <- array(NA, dim = c(14, 6))
colnames(mean_prop) = c('horizon', 'mean_IC_prop', 'mean_parameter_prop','mean_process_prop','mean_weather_prop', 'mean_discharge_prop')

for (i in 1:14) {
  temp <- uncert_prop[uncert_prop$day_in_future==i,]
  mean_prop[i,1] <- i
  mean_prop[i, 2] = mean(temp$IC_prop, na.rm = TRUE)
  mean_prop[i, 3] = mean(temp$parameter_prop, na.rm = TRUE)
  mean_prop[i, 4] = mean(temp$process_prop, na.rm = TRUE)
  mean_prop[i, 5] = mean(temp$weather_prop, na.rm = TRUE)
  mean_prop[i, 6] = mean(temp$discharge_prop, na.rm = TRUE)
}


mean_prop <- as.data.frame(mean_prop)
## put into long format for easy plotting
mean_prop_long <- mean_prop  %>% 
  gather(variable, measurement, mean_IC_prop:mean_discharge_prop)
mean_prop_long$variable <- as.factor(mean_prop_long$variable)
mean_prop_long$horizon <- as.factor(mean_prop_long$horizon)


ggplot(mean_prop, aes(x = horizon, y = mean_IC_prop)) +
  geom_bar(stat = 'identity', position = 'stack')

# GET RID OF DAILY FORECASTS AT DAY 15 and 16
#mean_prop_long <- mean_prop_long[mean_prop_long$horizon<15,]

cols <- c('IC'="#92D050",'parameter'="#660066", 'process' = '#C55A11', 'weather' = '#FFC000')
png(paste0(forecast_folder, '/Uncertainty_Bar_AcrossHorizon_Daily.png'), width = 800, height = 885)
ggplot(mean_prop_long, aes(x = horizon, y = measurement, fill = variable )) + 
  geom_bar(stat = 'identity', position= 'stack') +
  scale_fill_manual(labels = c('discharge', 'initial conditions', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = 'Uncertainty Type') +
  xlab('Forecast Horizon (days)') +
  ylab('Proportion of Variance') +
  scale_x_discrete(breaks = c(7,14),
                   labels = c('7','14')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        legend.position = 'none')
dev.off()

# save figure of just the legend
png(paste0(forecast_folder, '/Uncertainty_Legend.png'), width = 800, height = 885)
ggplot(mean_prop_long, aes(x = horizon, y = measurement, fill = variable )) + 
  geom_bar(stat = 'identity', position= 'stack') +
  scale_fill_manual(labels = c('discharge', 'initial conditions', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = 'Uncertainty Source') +
  xlab('Forecast Horizon (days)') +
  ylab('Proportion of Variance') +
  scale_x_discrete(breaks = c(7,14),
                   labels = c('7','14')) +
  theme_bw() +
  theme(axis.text.x = element_blank(),
       # axis.text.y = element_blank(),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())#,
#legend.position = 'none')  
dev.off()


#  old MDietze colors
#scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather'),
#                  labels = c('driver: discharge', 'IC', 'parameter', 'process', 'driver: weather'),
#                  values = c('darkgreen', 'black', 'red', 'lightblue', 'green'),
#                  name = "Uncertainty Type") 


