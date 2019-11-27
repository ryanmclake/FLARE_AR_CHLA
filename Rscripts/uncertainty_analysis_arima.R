# script to analyze different sources of uncertainty for arima forecast output

library(rsq)
library(tidyverse)
library(Metrics)
reference_tzone <- "GMT"

#set the location of the forecasts
forecast_folder <- "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3"
setwd(forecast_folder)

# the year's forecasts with all uncertainties 'on' in the forecast output
myfiles <- list.files(path = "./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019", pattern = "*weekly.csv")
dataset <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/", myfiles[1]))

# read in files
for (i in 2:length(myfiles)) {
  temp <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/", myfiles[i]))
  dataset <- rbind(dataset, temp)
}



# read in each batch on uncertainty files
################ uncertainty 2 = process uncertainty
myfiles_2 <- list.files(path = "./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_2_process/", pattern = "*weekly.csv")
dataset_2 <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_2_process/", myfiles_2[1]))
# read in files
for (i in 2:length(myfiles_2)) {
  temp <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_2_process/", myfiles_2[i]))
  dataset_2 <- rbind(dataset_2, temp)
}

############### uncertainty 3 = weather uncertainty
myfiles_3 <- list.files(path = "./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_3_weather", pattern = "*weekly.csv")
dataset_3 <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_3_weather/", myfiles_3[1]))
# read in files
for (i in 2:length(myfiles_3)) {
  temp <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_3_weather/", myfiles_3[i]))
  dataset_3 <- rbind(dataset_3, temp)
}

############### uncertainty 4 = initial condition uncertainty
myfiles_4 <- list.files(path = "./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_4_initial_condition", pattern = "*weekly.csv")
dataset_4 <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_4_initial_condition/", myfiles_4[1]))
# read in files
for (i in 2:length(myfiles_4)) {
  temp <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_4_initial_condition/", myfiles_4[i]))
  dataset_4 <- rbind(dataset_4, temp)
}

############### uncertainty 5 = parameter uncertainty
myfiles_5 <- list.files(path = "./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_5_parameter", pattern = "*weekly.csv")
dataset_5 <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_5_parameter/", myfiles_5[1]))
# read in files
for (i in 2:length(myfiles_5)) {
  temp <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_5_parameter/", myfiles_5[i]))
  dataset_5 <- rbind(dataset_5, temp)
}

############### uncertainty 6 = discharge uncertainty
myfiles_6 <- list.files(path = "./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_6_discharge", pattern = "*weekly.csv")
dataset_6 <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_6_discharge/", myfiles_6[1]))
# read in files
for (i in 2:length(myfiles_6)) {
  temp <- read.csv(paste0("./FCR_forecasts/with_DA_and_saving_parms_Oct23_2019/uncertainty_6_discharge/", myfiles_6[i]))
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
# 3.5%!!!

# process > discharge > weather > parameter > initial condition


# now separate out by week and do the same thing
dataset2_week1 <- dataset_2[dataset_2$week==1,]
dataset2_week2 <- dataset_2[dataset_2$week==2,]

dataset3_week1 <- dataset_3[dataset_3$week==1,]
dataset3_week2 <- dataset_3[dataset_3$week==2,]

dataset4_week1 <- dataset_4[dataset_4$week==1,]
dataset4_week2 <- dataset_4[dataset_4$week==2,]

dataset5_week1 <- dataset_5[dataset_5$week==1,]
dataset5_week2 <- dataset_5[dataset_5$week==2,]

dataset6_week1 <- dataset_6[dataset_6$week==1,]
dataset6_week2 <- dataset_6[dataset_6$week==2,]

datasetweek1 <- dataset[dataset$week==1,]
datasetweek2 <- dataset[dataset$week==2,]

process_week1 <- mean(dataset2_week1$forecast_variance, na.rm = TRUE)
weather_week1 <- mean(dataset3_week1$forecast_variance, na.rm = TRUE)
IC_week1 <- mean(dataset4_week1$forecast_variance, na.rm = TRUE)
parameter_week1 <- mean(dataset5_week1$forecast_variance, na.rm = TRUE)
discharge_week1 <- mean(dataset6_week1$forecast_variance, na.rm = TRUE)

all_week1 <- process_week1+weather_week1+IC_week1+parameter_week1+discharge_week1
(mean(datasetweek1$forecast_variance, na.rm = TRUE) - all_week1)/mean(datasetweek1$forecast_variance, na.rm = TRUE)

dataset2_week1$forecast_date <- as.Date(dataset2_week1$forecast_date)
process_week1_bloom <- dataset2_week1[dataset2_week1$forecast_date > as.Date("2019-07-10") & dataset2_week1$forecast_date < as.Date("2019-08-10"),]
dataset2_week2$forecast_date <- as.Date(dataset2_week2$forecast_date)
process_week2_bloom <- dataset2_week2[dataset2_week2$forecast_date > as.Date("2019-07-10") & dataset2_week2$forecast_date < as.Date("2019-08-10"),]

dataset3_week1$forecast_date <- as.Date(dataset3_week1$forecast_date)
weather_week1_bloom <- dataset3_week1[dataset3_week1$forecast_date > as.Date("2019-07-10") & dataset3_week1$forecast_date < as.Date("2019-08-10"),]
dataset3_week2$forecast_date <- as.Date(dataset3_week2$forecast_date)
weather_week2_bloom <- dataset3_week2[dataset3_week2$forecast_date > as.Date("2019-07-10") & dataset3_week2$forecast_date < as.Date("2019-08-10"),]

dataset4_week1$forecast_date <- as.Date(dataset4_week1$forecast_date)
IC_week1_bloom <- dataset4_week1[dataset4_week1$forecast_date > as.Date("2019-07-10") & dataset4_week1$forecast_date < as.Date("2019-08-10"),]
dataset4_week2$forecast_date <- as.Date(dataset4_week2$forecast_date)
IC_week2_bloom <- dataset4_week2[dataset4_week2$forecast_date > as.Date("2019-07-10") & dataset4_week2$forecast_date < as.Date("2019-08-10"),]

dataset5_week1$forecast_date <- as.Date(dataset5_week1$forecast_date)
parameter_week1_bloom <- dataset5_week1[dataset5_week1$forecast_date > as.Date("2019-07-10") & dataset5_week1$forecast_date < as.Date("2019-08-10"),]
dataset5_week2$forecast_date <- as.Date(dataset5_week2$forecast_date)
parameter_week2_bloom <- dataset5_week2[dataset5_week2$forecast_date > as.Date("2019-07-10") & dataset5_week2$forecast_date < as.Date("2019-08-10"),]

dataset6_week1$forecast_date <- as.Date(dataset6_week1$forecast_date)
discharge_week1_bloom <- dataset6_week1[dataset6_week1$forecast_date > as.Date("2019-07-10") & dataset6_week1$forecast_date < as.Date("2019-08-10"),]
dataset6_week2$forecast_date <- as.Date(dataset6_week2$forecast_date)
discharge_week2_bloom <- dataset6_week2[dataset6_week2$forecast_date > as.Date("2019-07-10") & dataset6_week2$forecast_date < as.Date("2019-08-10"),]

process_week1_bloom_variance <- mean(process_week1_bloom$forecast_variance, na.rm = TRUE)
process_week2_bloom_variance <- mean(process_week2_bloom$forecast_variance, na.rm = TRUE)
weather_week1_bloom_variance <- mean(weather_week1_bloom$forecast_variance, na.rm = TRUE)
weather_week2_bloom_variance <- mean(weather_week2_bloom$forecast_variance, na.rm = TRUE)
IC_week1_bloom_variance <- mean(IC_week1_bloom$forecast_variance, na.rm = TRUE)
IC_week2_bloom_variance <- mean(IC_week2_bloom$forecast_variance, na.rm = TRUE)
parameter_week1_bloom_variance <- mean(parameter_week1_bloom$forecast_variance, na.rm = TRUE)
parameter_week2_bloom_variance <- mean(parameter_week2_bloom$forecast_variance, na.rm = TRUE)
discharge_week1_bloom_variance <- mean(discharge_week1_bloom$forecast_variance, na.rm = TRUE)
discharge_week2_bloom_variance <- mean(discharge_week2_bloom$forecast_variance, na.rm = TRUE)

all_week2_bloom <- process_week2_bloom_variance + weather_week2_bloom_variance + IC_week2_bloom_variance + parameter_week2_bloom_variance + 
  discharge_week2_bloom_variance
datasetweek1$forecast_date <- as.Date(datasetweek1$forecast_date)
datasetweek1_bloom <- datasetweek1[datasetweek1$forecast_date > as.Date("2019-07-10") & datasetweek1$forecast_date < as.Date("2019-08-10"),]
datasetweek2$forecast_date <- as.Date(datasetweek2$forecast_date)
datasetweek2_bloom <- datasetweek2[datasetweek2$forecast_date > as.Date("2019-07-10") & datasetweek2$forecast_date < as.Date("2019-08-10"),]


uncert_bloom <- data.frame(
  'uncertainty_type' = rep(c("process", "weather", "IC", "parameter", "discharge", "all"), 2), 
  'week' = c(1,1,1,1,1,1,2,2,2,2,2,2),
  'variance' = c(process_week1_bloom_variance, weather_week1_bloom_variance, IC_week1_bloom_variance, parameter_week1_bloom_variance, 
                 discharge_week1_bloom_variance, mean(datasetweek1_bloom$forecast_variance, na.rm = TRUE),
                 process_week2_bloom_variance, weather_week2_bloom_variance, IC_week2_bloom_variance, parameter_week2_bloom_variance, 
                 discharge_week2_bloom_variance, mean(datasetweek2_bloom$forecast_variance, na.rm = TRUE)))

# calculate the proportion of variance for each source of uncertainty
uncert_bloom <- uncert_bloom %>% mutate(prop_var = ifelse(week==1, variance/mean(datasetweek1_bloom$forecast_variance, na.rm = TRUE), variance/mean(datasetweek2_bloom$forecast_variance, na.rm = TRUE))    )

# get rid of the all uncertainty actually
uncert_bloom <- uncert_bloom[!uncert_bloom$uncertainty_type=='all',]
uncert_bloom$week <- as.factor(uncert_bloom$week)

# stacked bar plot showing mean uncertainty for each forecast horizon
# first as a proportion
png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/UncertaintyProportion_Partitioned_Bloom.png', width = 800, height = 785)
ggplot(uncert_bloom, aes(x = week, y = prop_var, fill = uncertainty_type )) + 
  geom_bar(stat = 'identity', position= 'stack') +
  scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather'),
                    labels = c('driver: discharge', 'IC', 'parameter', 'process', 'driver: weather'),
                    values = c('darkgreen', 'black', 'red', 'lightblue', 'green'),
                    name = "Uncertainty Type") +
  xlab('Forecast Week') +
  ylab('Proportion of Variance') +
  theme(axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  ) 
dev.off()


# and now with the individual values to show how variance increases in week 2
png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Uncertainty_Partitioned_bloom_prettycolors.png', width = 800, height = 785)
ggplot(uncert_bloom, aes(x = week, y = variance, fill = uncertainty_type )) + 
  geom_bar(stat = 'identity', position= 'stack')+
  scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  
  theme(axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  ) +
  xlab('Forecast Week') +
  ylab('Total Variance')
dev.off()


process_week2 <- mean(dataset2_week2$forecast_variance, na.rm = TRUE)
weather_week2 <- mean(dataset3_week2$forecast_variance, na.rm = TRUE)
IC_week2 <- mean(dataset4_week2$forecast_variance, na.rm = TRUE)
parameter_week2 <- mean(dataset5_week2$forecast_variance, na.rm = TRUE)
discharge_week2 <- mean(dataset6_week2$forecast_variance, na.rm = TRUE)

all_week2 <- process_week2+weather_week2+IC_week2+parameter_week2+discharge_week2
(mean(datasetweek2$forecast_variance, na.rm = TRUE) - all_week2)/mean(datasetweek2$forecast_variance, na.rm = TRUE)

# put the different variances together in a dataframe for graphing purposes
uncert <- data.frame(
  'uncertainty_type' = rep(c("process", "weather", "IC", "parameter", "discharge", "all"), 2), 
  'week' = c(1,1,1,1,1,1,2,2,2,2,2,2),
  'variance' = c(process_week1, weather_week1, IC_week1, parameter_week1, discharge_week1,mean(datasetweek1$forecast_variance, na.rm = TRUE),
  process_week2, weather_week2, IC_week2, parameter_week2, discharge_week2, mean(datasetweek2$forecast_variance, na.rm = TRUE)))

# calculate the proportion of variance for each source of uncertainty
uncert <- uncert %>% mutate(prop_var = ifelse(week==1, variance/mean(datasetweek1$forecast_variance, na.rm = TRUE), variance/mean(datasetweek2$forecast_variance, na.rm = TRUE))    )

# get rid of the all uncertainty actually
uncert <- uncert[!uncert$uncertainty_type=='all',]
uncert$week <- as.factor(uncert$week)

# stacked bar plot showing mean uncertainty for each forecast horizon
# first as a proportion
png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/UncertaintyProportion_Partitioned_withIC.png', width = 800, height = 785)
ggplot(uncert, aes(x = week, y = prop_var, fill = uncertainty_type )) + 
  geom_bar(stat = 'identity', position= 'stack') +
  scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather'),
                    labels = c('driver: discharge', 'IC', 'parameter', 'process', 'driver: weather'),
                    values = c('darkgreen', 'black', 'red', 'lightblue', 'green'),
                    name = "Uncertainty Type") +
  xlab('Forecast Week') +
  ylab('Proportion of Variance') +
  theme(axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
       # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        ) 
dev.off()


# and now with the individual values to show how variance increases in week 2
png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Uncertainty_Partitioned_withIC_prettycolors.png', width = 800, height = 785)
ggplot(uncert, aes(x = week, y = variance, fill = uncertainty_type )) + 
  geom_bar(stat = 'identity', position= 'stack')+
  scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather') ,
                                      values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                                      name = "Uncertainty Type") +
                    
  theme(axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
       ) +
  xlab('Forecast Week') +
  ylab('Total Variance')
dev.off()
# levels = c('IC', 'process', 'parameter', 'discharge', 'weather')
# colors that match the symbols in the talk
#scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather') ,
#                  values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
#                  name = "Uncertainty Type") +


# subset to just the bloom period to look at variance during that time only


# colors that match dietze
#scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather'),
#                  labels = c('driver: discharge', 'IC', 'parameter', 'process', 'driver: weather'),
#                  values = c('darkgreen', 'black', 'red', 'lightblue', 'green'),
#                  name = "Uncertainty Type") +
############################################################################################################################################################
# line plot showing each uncertainty type over time for week 1
# do some data wrangling to get everything into one dataframe
data_week1 <- dataset3_week1 %>% rename(weather_mean = forecast_mean_chl) %>%  
  rename(weather_var = forecast_variance) %>% 
  select(forecast_date, weather_mean, weather_var, obs_chl_EXO)

data2_week1 <- dataset2_week1 %>% rename(process_mean = forecast_mean_chl) %>% 
  rename(process_var = forecast_variance) %>% 
  select(forecast_date, process_mean, process_var, obs_chl_EXO)

data4_week1 <- dataset4_week1 %>% rename(IC_mean = forecast_mean_chl) %>% 
  rename(IC_var = forecast_variance) %>% 
  select(forecast_date, IC_mean, IC_var, obs_chl_EXO)

data5_week1 <- dataset5_week1 %>% rename(parameter_mean = forecast_mean_chl) %>% 
  rename(parameter_var = forecast_variance) %>% 
  select(forecast_date, parameter_mean, parameter_var, obs_chl_EXO)

data6_week1 <- dataset6_week1 %>% rename(discharge_mean = forecast_mean_chl) %>% 
  rename(discharge_var = forecast_variance) %>% 
  select(forecast_date, discharge_mean, discharge_var, obs_chl_EXO)

data_week1 <- left_join(data_week1, data2_week1)
data_week1 <- left_join(data_week1, data4_week1)
data_week1 <- left_join(data_week1, data5_week1)
data_week1 <- left_join(data_week1, data6_week1)
data_week1 <- left_join(data_week1, datasetweek1)

data_week1$forecast_date <- as.Date(data_week1$forecast_date)
datasetweek1$forecast_date <- as.Date(datasetweek1$forecast_date)
plot(datasetweek1$forecast_date, datasetweek1$forecast_variance, type = 'l', col = 'red')


plot(data_week1$forecast_date, data_week1$weather_var, col = 'green', type = 'l', ylim = c(0, 0.3), xlab = 'Date', ylab = 'Forecast Variance')
points(data_week1$forecast_date, data_week1$process_var, col = 'skyblue1', type = 'l')
points(data_week1$forecast_date, data_week1$discharge_var, col = 'green4', type = 'l')
points(data_week1$forecast_date, data_week1$parameter_var, col = 'red', type = 'l')
#points(datasetweek1$forecast_date, datasetweek1$forecast_variance, type = 'l', col = 'black')
legend('topleft', c('driver: weather', 'process', 'driver: discharge', 'parameter'), col = c('green', 'skyblue1', 'green4', 'red'), lty = c(1,1), bty ='o' , cex = 0.7)

# plot of chl time series with different types of uncertainty isolated
plot(data_week1$forecast_date, data_week1$weather_mean, col = 'blue', type = 'l', ylim = c(0,15))
points(data_week1$forecast_date, data_week1$process_mean, col = 'green', type = 'l')
points(data_week1$forecast_date, data_week1$discharge_mean, col = 'brown', type = 'l')
points(data_week1$forecast_date, data_week1$parameter_mean, col = 'purple', type = 'l')
points(datasetweek1$forecast_date, datasetweek1$forecast_mean_chl, type = 'l', col = 'red')
points(data_week1$forecast_date, data_week1$obs_chl_EXO)

# calculate the proportion of variance rather than the raw value
data_week1_proportion <- data_week1 %>% select(forecast_date, weather_var, process_var, discharge_var, parameter_var, IC_var, forecast_variance) %>% 
  mutate(weather_prop = weather_var/(weather_var + process_var + discharge_var + parameter_var + IC_var)) %>% 
  mutate(process_prop = process_var/(weather_var + process_var + discharge_var + parameter_var + IC_var)) %>% 
  mutate(discharge_prop = discharge_var/(weather_var + process_var + discharge_var + parameter_var + IC_var)) %>% 
  mutate(parameter_prop = parameter_var/(weather_var + process_var + discharge_var + parameter_var + IC_var)) %>% 
  mutate(IC_prop = IC_var/(weather_var + process_var + discharge_var + parameter_var + IC_var))
  
data_week1_proportion_long <- data_week1_proportion %>% select(forecast_date, weather_prop, process_prop, discharge_prop, parameter_prop, IC_prop) %>% 
  gather(variable, measurement, weather_prop:IC_prop)
ggplot(data_week1_proportion_long, aes(x = forecast_date, y = measurement, color = variable)) +geom_line()

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Uncertainty_TimeSeries_withIC_prettycolors.png', width = 1200, height = 785)
ggplot(data_week1_proportion_long, aes(x = forecast_date, y = measurement, fill = variable)) +geom_area() + ylim(0,1.1) +
  xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather') ,
                                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                                      name = "Uncertainty Type") +
# colors thaat match dietze
#  scale_fill_manual(labels = c('driver: discharge', 'parameter', 'process', 'driver: weather', 'initial conditions'),
#                   values = c('darkgreen',  'red', 'lightblue', 'green', 'black'),
#                   name = "Uncertainty Type") +
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        )
dev.off()  


# subset to bloom period only and look at variance
plot(data_week1$forecast_date, data_week1$obs_chl_EXO, xlim = c(as.Date('2019-07-10'), as.Date('2019-08-10')))
data_week1_bloom <- data_week1_proportion[data_week1_proportion$forecast_date>as.Date("2019-07-10") & data_week1_proportion$forecast_date<as.Date("2019-08-10"),]
data_bloom_long <- data_week1_bloom %>% select(forecast_date, weather_prop, process_prop, discharge_prop, parameter_prop, IC_prop) %>% 
  gather(variable, measurement, weather_prop:IC_prop)

png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Uncertainty_BLOOMONLY_prettycolors.png', width = 1200, height = 785)
ggplot(data_bloom_long, aes(x = forecast_date, y = measurement, fill = variable)) +geom_area() + ylim(0,1.1) +
  xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(breaks = c('discharge', 'IC', 'parameter', 'process', 'weather') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  # colors thaat match dietze
  #  scale_fill_manual(labels = c('driver: discharge', 'parameter', 'process', 'driver: weather', 'initial conditions'),
  #                   values = c('darkgreen',  'red', 'lightblue', 'green', 'black'),
  #                   name = "Uncertainty Type") +
  theme(axis.text.x = element_text(size = 30),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
  )
dev.off()  



# convert to long format to make ggplot
data_week1_long <- data_week1 %>% select(forecast_date, weather_var, process_var, discharge_var, parameter_var, IC_var) %>% 
  gather(variable, measurement, weather_var:IC_var)
ggplot(data_week1_long, aes(x = forecast_date, y = measurement, fill = variable)) + geom_area()

install.packages('ggridges')
library(ggridges)
data_week1_long <- na.omit(data_week1_long)
ggplot(data_week1_long, aes(x = forecast_date, y = measurement, color = variable)) + geom_line()

###############################################################################################################################################################################
###############################################################################################################################################################################
# line plot showing each uncertainty type over time for week 2
# do some data wrangling to get everything into one dataframe
data_week2 <- dataset3_week2 %>% rename(weather_mean = forecast_mean_chl) %>%  
  rename(weather_var = forecast_variance) %>% 
  select(forecast_date, weather_mean, weather_var, obs_chl_EXO)

data2_week2 <- dataset2_week2 %>% rename(process_mean = forecast_mean_chl) %>% 
  rename(process_var = forecast_variance) %>% 
  select(forecast_date, process_mean, process_var, obs_chl_EXO)

data4_week2 <- dataset4_week2 %>% rename(IC_mean = forecast_mean_chl) %>% 
  rename(IC_var = forecast_variance) %>% 
  select(forecast_date, IC_mean, IC_var, obs_chl_EXO)

data5_week2 <- dataset5_week2 %>% rename(parameter_mean = forecast_mean_chl) %>% 
  rename(parameter_var = forecast_variance) %>% 
  select(forecast_date, parameter_mean, parameter_var, obs_chl_EXO)

data6_week2 <- dataset6_week2 %>% rename(discharge_mean = forecast_mean_chl) %>% 
  rename(discharge_var = forecast_variance) %>% 
  select(forecast_date, discharge_mean, discharge_var, obs_chl_EXO)

data_week2 <- left_join(data_week2, data2_week2)
data_week2 <- left_join(data_week2, data4_week2)
data_week2 <- left_join(data_week2, data5_week2)
data_week2 <- left_join(data_week2, data6_week2)

data_week2$forecast_date <- as.Date(data_week2$forecast_date)
datasetweek2$forecast_date <- as.Date(datasetweek2$forecast_date)
plot(datasetweek2$forecast_date, datasetweek2$forecast_variance, type = 'l', col = 'red')



plot(data_week2$forecast_date, data_week2$weather_var, col = 'green', type = 'l', ylim = c(0, 0.4), xlab = 'Date', ylab = 'Forecast Variance')
points(data_week2$forecast_date, data_week2$process_var, col = 'skyblue1', type = 'l')
points(data_week2$forecast_date, data_week2$discharge_var, col = 'green4', type = 'l')
points(data_week2$forecast_date, data_week2$parameter_var, col = 'red', type = 'l')
#points(datasetweek1$forecast_date, datasetweek1$forecast_variance, type = 'l', col = 'black')
legend('topleft', c('driver: weather', 'process', 'driver: discharge', 'parameter'), col = c('green', 'skyblue1', 'green4', 'red'), lty = c(1,1), bty ='o' , cex = 0.7)

plot(data_week1$forecast_date, data_week1$weather_mean, col = 'blue', type = 'l', ylim = c(0,15))
points(data_week1$forecast_date, data_week1$process_mean, col = 'green', type = 'l')
points(data_week1$forecast_date, data_week1$discharge_mean, col = 'brown', type = 'l')
points(data_week1$forecast_date, data_week1$parameter_mean, col = 'purple', type = 'l')
points(datasetweek1$forecast_date, datasetweek1$forecast_mean_chl, type = 'l', col = 'red')
points(data_week1$forecast_date, data_week1$obs_chl_EXO)


# calculate the proportion of variance rather than the raw value
data_week2_proportion <- data_week2 %>% select(forecast_date, weather_var, process_var, discharge_var, parameter_var, IC_var) %>% 
  mutate(weather_prop = weather_var/(weather_var + process_var + discharge_var + parameter_var + IC_var)) %>% 
  mutate(process_prop = process_var/(weather_var + process_var + discharge_var + parameter_var + IC_var)) %>% 
  mutate(discharge_prop = discharge_var/(weather_var + process_var + discharge_var + parameter_var + IC_var)) %>% 
  mutate(parameter_prop = parameter_var/(weather_var + process_var + discharge_var + parameter_var + IC_var)) 

data_week2_proportion_long <- data_week2_proportion %>% select(forecast_date, weather_prop, process_prop, discharge_prop, parameter_prop) %>% 
  gather(variable, measurement, weather_prop:parameter_prop)


png('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/Uncertainty_TimeSeries_Week2.png', width = 1200, height = 785)
ggplot(data_week2_proportion_long, aes(x = forecast_date, y = measurement, fill = variable)) +geom_area() + ylim(0,1.1) +
  xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(labels = c('driver: discharge', 'parameter', 'process', 'driver: weather'),
                    values = c('darkgreen',  'red', 'lightblue', 'green'),
                    name = "Uncertainty Type") +
  theme(axis.text.x = element_text(size = 40),
        axis.text.y = element_text(size = 40),
        axis.title.x = element_text(size =45),
        axis.title.y = element_text(size = 45),
        legend.title = element_text(size = 35),
        legend.text = element_text(size = 30),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = '#9DC3E6', color = '#9DC3E6'),
        plot.background = element_rect(fill = '#9DC3E6', color = '#9DC3E6'))
dev.off()  


###############################################################################################################################################################################

# because discharge uncertainty has some large increases, look at the weir discharge data from the past year
folder <- "C:/Users/wwoel/Desktop/FLARE/FLARE_3/FLARE_3"

# specify full_time as the entire time series from 08-15-2018 to -7-15-2019
full_time <- seq(as.Date("2018-08-01"), as.Date("2019-07-15"), by = "1 day")
working_glm <- paste0(folder, "/", "GLM_working")  
reference_tzone <- "GMT"
source(paste0(folder,"/","Rscripts/create_inflow_outflow_file_old.R"))


create_inflow_outflow_file(full_time ,
                           working_glm = working_glm, 
                           input_tz = "EST5EDT",
                           output_tz = reference_tzone)


discharge_avg <- read.csv(paste0(folder,"/", "GLM_working/FCR_inflow.csv"))
discharge_avg$time <- as.Date(discharge_avg$time)
colnames(discharge_avg) <- c('DateTime', 'Flow_avg_forecast', 'SD')

# and the observed inflow data
baro <- read_csv("C:/Users/wwoel/Desktop/Reservoirs_2/Data/DataNotYetUploadedToEDI/Raw_inflow/baro.csv")
inflow <- read_csv("C:/Users/wwoel/Desktop/Reservoirs_2/Data/DataNotYetUploadedToEDI/Raw_inflow/inflow.csv")
baro$DateTime <- as.Date(baro$DateTime)
inflow$DateTime <- as.Date(inflow$DateTime)

# subset to the length of full_time
baro <- subset(baro, baro$DateTime > as.Date('2018-08-01'))
inflow <- subset(inflow, inflow$DateTime > as.Date('2018-08-01'))
inflow <- subset(inflow, inflow$DateTime < as.Date('2019-06-04'))

daily_baro <- baro %>% group_by(DateTime) %>% mutate(daily_avg_baro = mean(Baro_pressure_psi))
daily_baro <- daily_baro[!duplicated(daily_baro$DateTime),]
daily_baro <- daily_baro %>% select(DateTime, daily_avg_baro)

daily_flow <- inflow %>% group_by(DateTime) %>% mutate(daily_avg_rawflow = mean(Pressure_psi))
daily_flow <- daily_flow[!duplicated(daily_flow$DateTime),]
daily_flow <- daily_flow %>% select(DateTime, daily_avg_rawflow)

discharge_obs <- left_join(daily_baro, daily_flow)
discharge_obs <- discharge_obs %>% mutate(pressure_corrected = daily_avg_rawflow - daily_avg_baro)

### CALCULATE THE FLOW RATES AT INFLOW ### #( numbers taken from MEL 2018-07-06)
#################################################################################################
discharge_obs <- discharge_obs %>% mutate(flow3 = pressure_corrected*0.70324961490205 - 0.1603375 + 0.03048) %>% 
  mutate(flow4 = (0.62 * (2/3) * (1.1) * 4.43 * (flow3 ^ 1.5) * 35.3147)) %>%  # Flow CFS - MEL: I have not changed this; should be rating curve with area of weir)
  mutate(flow_cms = flow4*0.028316847)



plot(discharge_obs$DateTime, discharge_obs$flow_cms, type = 'l')
points(discharge_avg$DateTime, discharge_avg$Flow_avg_forecast, col = 'red', type = 'l')

discharge_stuff <- left_join(discharge_obs, discharge_avg)
discharge_stuff <- discharge_stuff %>% mutate(diff = flow_cms - Flow_avg_forecast)
plot(discharge_stuff$DateTime, discharge_stuff$diff, type = 'l', xlab = 'Date', ylab = 'observed - predicted', main = 'Obs discharge minus 5 year avg')
abline(h = 0)
