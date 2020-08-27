# script to analyze different sources of uncertainty for arma weekly forecast output

library(scales)
library(rsq)
library(tidyverse)
library(Metrics)
reference_tzone <- "GMT"

# define specs for the timestep and simulation
timestep <- '14day' # character definition of the timestep
timestep_numeric <- 14 # maybe timestep_numeric and timestep_interval are actually the same thing and not both needed -_-
timestep_interval <- 14 # the interval in between timesteps, e.g. 4day would be 4; daily would be 1; weekly would be 7
max_timestep <- 1 #maximum number of timesteps that can be propagated to the max time horizon
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 16)
sim_name <- '22Jul2020'
folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"
forecast_folder <- paste0(folder, "/FCR_forecasts", '/', timestep, '/', sim_name)
bloom_threshold <- 17.1

# the year's forecasts with all uncertainties 'on' in the forecast output
myfiles <- list.files(path = forecast_folder, pattern = "*14day.csv")
dataset <- read.csv(paste0(forecast_folder, "/", myfiles[1]))

# read in files
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0(forecast_folder, "/", myfiles[i]))
  dataset <- rbind(dataset, temp)
}



# read in each batch on uncertainty files
################ uncertainty 2 = process uncertainty
myfiles_2 <- list.files(path = paste0(forecast_folder, "/uncertainty_2_process/"), pattern = "14day.csv")
dataset_2 <- read.csv(paste0(forecast_folder, "/uncertainty_2_process/", myfiles_2[1]))
# read in files
for (i in 2:length(myfiles_2)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_2_process/", myfiles_2[i]))
  dataset_2 <- rbind(dataset_2, temp)
}

############### uncertainty 3 = weather uncertainty
myfiles_3 <- list.files(path = paste0(forecast_folder, "/uncertainty_3_weather/"), pattern = "14day.csv")
dataset_3 <- read.csv(paste0(forecast_folder, '/uncertainty_3_weather/', myfiles_3[1]))
# read in files
for (i in 2:length(myfiles_3)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_3_weather/", myfiles_3[i]))
  dataset_3 <- rbind(dataset_3, temp)
}

############### uncertainty 4 = initial condition uncertainty
myfiles_4 <- list.files(path = paste0(forecast_folder, "/uncertainty_4_initial_condition/"), pattern = "14day.csv")
dataset_4 <- read.csv(paste0(forecast_folder, "/uncertainty_4_initial_condition/", myfiles_4[1]))
# read in files
for (i in 2:length(myfiles_4)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_4_initial_condition/", myfiles_4[i]))
  dataset_4 <- rbind(dataset_4, temp)
}

############### uncertainty 5 = parameter uncertainty
myfiles_5 <- list.files(path = paste0(forecast_folder, "/uncertainty_5_parameter"), pattern = "14day.csv")
dataset_5 <- read.csv(paste0(forecast_folder, "/uncertainty_5_parameter/", myfiles_5[1]))
# read in files
for (i in 2:length(myfiles_5)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_5_parameter/", myfiles_5[i]))
  dataset_5 <- rbind(dataset_5, temp)
}

############### uncertainty 6 = discharge driver uncertainty
myfiles_6 <- list.files(path = paste0(forecast_folder, "/uncertainty_6_discharge"), pattern = "14day.csv")
dataset_6 <- read.csv(paste0(forecast_folder, "/uncertainty_6_discharge/", myfiles_5[1]))
# read in files
for (i in 2:length(myfiles_6)) {
  temp <- read.csv(paste0(forecast_folder, "/uncertainty_6_discharge/", myfiles_6[i]))
  dataset_6 <- rbind(dataset_6, temp)
}

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

#############
uncert <- left_join(dataset_6, dataset_2, by = c('forecast_date', 'forecast_run_day', 'day_in_future'))
uncert <- left_join(uncert, dataset_3)
uncert <- left_join(uncert, dataset_4)
uncert <- left_join(uncert, dataset_5)

# calculate the proportion of variance for all of the uncertainties
uncert <- uncert %>% mutate(process_prop = process_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>% 
  mutate(weather_prop = weather_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>% 
  mutate(IC_prop = IC_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>% 
  mutate(parameter_prop = parameter_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance))%>% 
  mutate(discharge_prop = discharge_variance/(process_variance + weather_variance + IC_variance + parameter_variance + discharge_variance)) %>% 
  mutate(total_var = discharge_variance + process_variance + weather_variance + IC_variance + parameter_variance)


# dataframe with just proportions
uncert_prop <- uncert %>% select(forecast_date, forecast_run_day, day_in_future, process_prop, weather_prop, IC_prop, parameter_prop, discharge_prop, total_var)
plot(uncert_prop$forecast_date, uncert_prop$discharge_prop)
## put into long format for easy plotting
uncert_proportion_long <- uncert_prop  %>% 
  gather(variable, measurement, process_prop:total_var)



# subset to after Dec 31, 2018 to get rid of spin up period
uncert_proportion_long <- uncert_proportion_long[uncert_proportion_long$forecast_run_day > as.Date('2018-12-16'),]

##########################################################################################################################################################################
##### make some figures
##########################################################################################################################################################################

# simple line graph

  temp <- uncert_proportion_long[uncert_proportion_long$week==i,]
  ggplot(uncert_proportion_long, aes(x = forecast_date, y = measurement, color = variable)) +
          geom_line() +
          ggtitle('Fortnightly Forecast')
  


# pretty time series with geom_area()
  png(paste0(forecast_folder, '/Fortnightly_Uncertainty_TimeSeries_legend.png'), width = 1200, height = 785)
  temp <- uncert_proportion_long
  temp <- temp[!temp$variable=='total_var',]
  print(ggplot(temp, aes(x = forecast_date, y = measurement, fill = variable)) +geom_area(position = 'stack') + #ylim(0,1.1) +
          xlab('Date') +
          ylab('Proportion of Variance') +
          scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                            values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                            name = "Uncertainty Type") +
          ggtitle('Fortnightly Forecast, Day 14') +
          scale_y_continuous(expand = c(0,0)) +
          scale_x_date(expand = c(0,0), labels = date_format('%b %Y')) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 30),
                axis.text.y = element_text(size = 40),
                axis.title.x = element_text(size =45),
                axis.title.y = element_text(size = 45),
                legend.title = element_text(size = 35),
                legend.text = element_text(size = 30),
                #panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                plot.title = element_text(size = 40))
  )
  dev.off() 



### and create plots with the total variance plotted on top
##################################
temp <- uncert_proportion_long
var <- temp[temp$variable=='total_var',]
temp <- temp[!temp$variable=='total_var',]


p <- ggplot() 
p <- p + geom_area(data = temp, aes(x = forecast_date, y = measurement, fill = variable)) + 
  xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  ggtitle(paste0('Fortnightly Forecast, Day 14')) +
  scale_x_date( expand = c(0,0),labels = date_format('%b')) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 35),
        axis.text.y = element_text(size = 35),
        axis.title.x = element_text(size =35),
        axis.title.y = element_text(size = 35),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 50))
p <- p + geom_line(data = var, aes(x = forecast_date, y = measurement), lwd = 1.5) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~., name = expression (paste("Total Variance (",~μg/L^2,")"  )))) 

png(paste0(forecast_folder, '/Fortnightly_Uncertainty_Variance_TimeSeries.png'), width = 1200, height = 785)
p
dev.off()

############################################################################################################################################################  
# stacked bar plots of proportion variance for each forecast horizon
# create dataframe to write into inside the loop
mean_prop <- array(NA, dim = c(14, 6))
colnames(mean_prop) = c('horizon', 'mean_IC_prop', 'mean_parameter_prop','mean_process_prop','mean_weather_prop', 'mean_discharge_prop')

  temp <- uncert_prop
  mean_prop[,1] <- rep(1:14)
  mean_prop[14, 2] = mean(temp$IC_prop, na.rm = TRUE)
  mean_prop[14, 3] = mean(temp$parameter_prop, na.rm = TRUE)
  mean_prop[14, 4] = mean(temp$process_prop, na.rm = TRUE)
  mean_prop[14, 5] = mean(temp$weather_prop, na.rm = TRUE)
  mean_prop[14, 6] = mean(temp$discharge_prop, na.rm = TRUE)


# standard deviation of the proportion over time

  sd_discharge <- sd(temp$discharge_prop, na.rm = TRUE)
  print(paste0(sd_discharge))


mean_prop <- as.data.frame(mean_prop)
mean_prop <- mean_prop %>% mutate(day_in_future = horizon)

## put into long format for easy plotting
mean_prop_long <- mean_prop  %>% 
  gather(variable, measurement, mean_IC_prop:mean_discharge_prop)


ggplot(mean_prop, aes(x = horizon, y = mean_IC_prop)) +
  geom_bar(stat = 'identity', position = 'stack')
mean_prop_long$day_in_future <- as.factor(mean_prop_long$day_in_future)


png(paste0(forecast_folder, '/Uncertainty_Bar_AcrossHorizon_Fortnightly.png'), width = 800, height = 885)
ggplot(mean_prop_long, aes(x = day_in_future, y = measurement, fill = variable )) + 
  geom_bar(stat = 'identity', position= 'stack') +
  scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather') ,
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
        panel.border = element_blank()) 
dev.off()



#scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather'),
#                  labels = c('driver: discharge', 'IC', 'parameter', 'process', 'driver: weather'),
#                  values = c('darkgreen', 'black', 'red', 'lightblue', 'green'),
#                  name = "Uncertainty Type") +
  
  # separate into forecast horizon for individual analysis
  #for (i in 1:16) { # change the 16 to whatever number of timesteps you have
  #  temp <- uncert[uncert$day_in_future==i,]
  #write.csv(temp, paste0(forecast_folder, '/uncertainty_day_', i, '.csv'), row.names = FALSE)
  #}
  
  