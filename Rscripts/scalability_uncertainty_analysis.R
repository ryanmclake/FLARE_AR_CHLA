# uncertainty analysis BVR


library(tidyverse)
library(ggpubr)


folder <- getwd()
timestep <- '7day'
max_timestep <- 2

############ read in BVR files ##########################################################
# read in the individual forecast files named for the day on which the forecast is made
bvr_folder <- file.path(folder, 'FCR_forecasts', '7day', 'BVR_scalability_Aug2020-2021')
bvr_files <- list.files(path = bvr_folder, pattern = paste0('*', timestep, ".csv"))
bvr_fcasts <- read.csv(paste0(bvr_folder, "/", bvr_files[1]))
bvr_fcasts$timestep <- seq(1, max_timestep, by =1 )


# read in files
for (i in 2:length(bvr_files)) {
  temp_2 <- read.csv(paste0(bvr_folder,"/", bvr_files[i]))
  temp_2$timestep <-  seq(1, max_timestep, by =1 ) # this is the number of timesteps continued into the future, maybe should call this horizon?
  bvr_fcasts <- rbind(bvr_fcasts, temp_2)
}

bvr_fcasts$forecast_date <- as.Date(bvr_fcasts$forecast_date, "%Y-%m-%d")
bvr_fcasts <- bvr_fcasts[order(bvr_fcasts$forecast_date),]
bvr_fcasts$forecast_run_day <- as.Date(bvr_fcasts$forecast_run_day, "%Y-%m-%d")
bvr_fcasts <- na.omit(bvr_fcasts)

# read in each batch of uncertainty files
################ uncertainty 2 = process uncertainty
myfiles_2 <- list.files(path = paste0(bvr_folder, "/uncertainty_2_process/"), pattern = "*7day.csv")
dataset_2 <- read.csv(paste0(bvr_folder, "/uncertainty_2_process/", myfiles_2[1]))
# read in files
for (i in 2:length(myfiles_2)) {
  temp <- read.csv(paste0(bvr_folder, "/uncertainty_2_process/", myfiles_2[i]))
  dataset_2 <- rbind(dataset_2, temp)
}


############### uncertainty 3 = weather uncertainty
myfiles_3 <- list.files(path = paste0(bvr_folder, "/uncertainty_3_weather/"), pattern = "*7day.csv")
dataset_3 <- read.csv(paste0(bvr_folder, '/uncertainty_3_weather/', myfiles_3[1]))
# read in files
for (i in 2:length(myfiles_3)) {
  temp <- read.csv(paste0(bvr_folder, "/uncertainty_3_weather/", myfiles_3[i]))
  dataset_3 <- rbind(dataset_3, temp)
}

############### uncertainty 4 = initial condition uncertainty
myfiles_4 <- list.files(path = paste0(bvr_folder, "/uncertainty_4_initial_condition/"), pattern = "*7day.csv")
dataset_4 <- read.csv(paste0(bvr_folder, "/uncertainty_4_initial_condition/", myfiles_4[1]))
# read in files
for (i in 2:length(myfiles_4)) {
  temp <- read.csv(paste0(bvr_folder, "/uncertainty_4_initial_condition/", myfiles_4[i]))
  dataset_4 <- rbind(dataset_4, temp)
}

############### uncertainty 5 = parameter uncertainty
myfiles_5 <- list.files(path = paste0(bvr_folder, "/uncertainty_5_parameter"), pattern = "*7day.csv")
dataset_5 <- read.csv(paste0(bvr_folder, "/uncertainty_5_parameter/", myfiles_5[1]))
# read in files
for (i in 2:length(myfiles_5)) {
  temp <- read.csv(paste0(bvr_folder, "/uncertainty_5_parameter/", myfiles_5[i]))
  dataset_5 <- rbind(dataset_5, temp)
}

############### uncertainty 6 = discharge driver uncertainty
myfiles_6 <- list.files(path = paste0(bvr_folder, "/uncertainty_6_discharge"), pattern = "*7day.csv")
dataset_6 <- read.csv(paste0(bvr_folder, "/uncertainty_6_discharge/", myfiles_6[1]))
# read in files
for (i in 2:length(myfiles_6)) {
  temp <- read.csv(paste0(bvr_folder, "/uncertainty_6_discharge/", myfiles_6[i]))
  dataset_6 <- rbind(dataset_6, temp)
}


dataset <- bvr_fcasts %>% select(forecast_date, forecast_run_day, day_in_future, forecast_variance)
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


##########################################################################################################################################################################
# time series with the total variance plotted on top
##################################
# week 1 aka day 7

temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==7,]
temp <- temp[temp$forecast_run_day>'2019-01-01',]
var <- temp[temp$variable=='total_var',]
temp <- temp[!temp$variable=='total_var',]
#### THERE IS ONE STUPID DUPLICATED DATE THAT IS STACKING ON TOP AND CREATING A WHITE LINE BELOW
#### BRUTE FORCE REMOVING IT FROM EACH UNCERTAINTY SOURCE
process_prop <- temp[temp$variable=='process_prop',]
process_prop <- process_prop[!duplicated(process_prop$forecast_date),]

weather_prop <- temp[temp$variable=='weather_prop',]
weather_prop <- weather_prop[!duplicated(weather_prop$forecast_date),]

discharge_prop <- temp[temp$variable=='discharge_prop',]
discharge_prop <- discharge_prop[!duplicated(discharge_prop$forecast_date),]

IC_prop <- temp[temp$variable=='IC_prop',]
IC_prop <- IC_prop[!duplicated(IC_prop$forecast_date),]

parameter_prop <- temp[temp$variable=='parameter_prop',]
parameter_prop <- parameter_prop[!duplicated(parameter_prop$forecast_date),]

temp <- rbind(process_prop, weather_prop)
temp <- rbind(temp, discharge_prop)
temp <- rbind(temp, IC_prop)
temp <- rbind(temp, parameter_prop)
temp$variable <- as.factor(temp$variable)

p <- ggplot() 
p <- p + 
  geom_area(data = temp, aes(x = forecast_date, y = measurement, fill = variable)) + 
  ylab('Proportion of Variance') +
  scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  ggtitle(paste0('BVR Weekly Forecast, Day 7')) +
  scale_x_date( expand = c(0,0),labels = scales::date_format('%b'), date_breaks = '3 months') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30),
        legend.position = 'none')
p <- p + geom_line(data = var, aes(x = forecast_date, y = measurement/0.5), lwd = 1.5) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~.*0.5)) 

png(paste0(bvr_folder, '/Weekly_Week',1, '_Uncertainty_Variance_TimeSeries.png'), width = 1200, height = 785)
p
dev.off()

#####################################
# week 2 aka day 14

temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==14,]
temp <- temp[temp$forecast_run_day>'2019-01-01',]
var <- temp[temp$variable=='total_var',]
temp <- temp[!temp$variable=='total_var',]
#### THERE IS ONE STUPID DUPLICATED DATE THAT IS STACKING ON TOP AND CREATING A WHITE LINE BELOW
#### BRUTE FORCE REMOVING IT FROM EACH UNCERTAINTY SOURCE
process_prop <- temp[temp$variable=='process_prop',]
process_prop <- process_prop[!duplicated(process_prop$forecast_date),]

weather_prop <- temp[temp$variable=='weather_prop',]
weather_prop <- weather_prop[!duplicated(weather_prop$forecast_date),]

discharge_prop <- temp[temp$variable=='discharge_prop',]
discharge_prop <- discharge_prop[!duplicated(discharge_prop$forecast_date),]

IC_prop <- temp[temp$variable=='IC_prop',]
IC_prop <- IC_prop[!duplicated(IC_prop$forecast_date),]

parameter_prop <- temp[temp$variable=='parameter_prop',]
parameter_prop <- parameter_prop[!duplicated(parameter_prop$forecast_date),]

temp <- rbind(process_prop, weather_prop)
temp <- rbind(temp, discharge_prop)
temp <- rbind(temp, IC_prop)
temp <- rbind(temp, parameter_prop)


p2 <- ggplot() 
p2 <- p2 + geom_area(data = temp, aes(x = forecast_date, y = measurement, fill = variable)) + 
  ylab('Proportion of Variance') +
  scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  ggtitle(paste0('BVR Weekly Forecast, Day 14')) +
  scale_x_date(expand = c(0,0),labels = scales::date_format('%b'), date_breaks = '3 months') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30),
        legend.position = 'none')
p2 <- p2 + geom_line(data = var, aes(x = forecast_date, y = measurement/0.5), lwd = 1.5) +
  scale_y_continuous(expand = c(0,0),sec.axis = sec_axis(~.*0.5, name = expression (paste("Total Variance (",~μg/L^2,")"  )) )) 

png(paste0(bvr_folder, '/Weekly_Week',2, '_Uncertainty_Variance_TimeSeries.png'), width = 1200, height = 785)
p2
dev.off()

###############################################################################################################################################################
## now bring in FCR for UC figs
# uncertainty analysis fcr

############ read in fcr files ##########################################################
# read in the individual forecast files named for the day on which the forecast is made
fcr_folder <- file.path(folder, 'FCR_forecasts', '7day', 'FCR_scalability_Aug2020-2021')
fcr_files <- list.files(path = fcr_folder, pattern = paste0('*', timestep, ".csv"))
fcr_fcasts <- read.csv(paste0(fcr_folder, "/", fcr_files[1]))
fcr_fcasts$timestep <- seq(1, max_timestep, by =1 )


# read in files
for (i in 2:length(fcr_files)) {
  temp_2 <- read.csv(paste0(fcr_folder,"/", fcr_files[i]))
  temp_2$timestep <-  seq(1, max_timestep, by =1 ) # this is the number of timesteps continued into the future
  fcr_fcasts <- rbind(fcr_fcasts, temp_2)
}

fcr_fcasts$forecast_date <- as.Date(fcr_fcasts$forecast_date, "%Y-%m-%d")
fcr_fcasts <- fcr_fcasts[order(fcr_fcasts$forecast_date),]
fcr_fcasts$forecast_run_day <- as.Date(fcr_fcasts$forecast_run_day, "%Y-%m-%d")
fcr_fcasts <- na.omit(fcr_fcasts)

# read in each batch on uncertainty files
################ uncertainty 2 = process uncertainty
myfiles_2 <- list.files(path = paste0(fcr_folder, "/uncertainty_2_process/"), pattern = "*7day.csv")
dataset_2 <- read.csv(paste0(fcr_folder, "/uncertainty_2_process/", myfiles_2[1]))
# read in files
for (i in 2:length(myfiles_2)) {
  temp <- read.csv(paste0(fcr_folder, "/uncertainty_2_process/", myfiles_2[i]))
  dataset_2 <- rbind(dataset_2, temp)
}


############### uncertainty 3 = weather uncertainty
myfiles_3 <- list.files(path = paste0(fcr_folder, "/uncertainty_3_weather/"), pattern = "*7day.csv")
dataset_3 <- read.csv(paste0(fcr_folder, '/uncertainty_3_weather/', myfiles_3[1]))
# read in files
for (i in 2:length(myfiles_3)) {
  temp <- read.csv(paste0(fcr_folder, "/uncertainty_3_weather/", myfiles_3[i]))
  dataset_3 <- rbind(dataset_3, temp)
}

############### uncertainty 4 = initial condition uncertainty
myfiles_4 <- list.files(path = paste0(fcr_folder, "/uncertainty_4_initial_condition/"), pattern = "*7day.csv")
dataset_4 <- read.csv(paste0(fcr_folder, "/uncertainty_4_initial_condition/", myfiles_4[1]))
# read in files
for (i in 2:length(myfiles_4)) {
  temp <- read.csv(paste0(fcr_folder, "/uncertainty_4_initial_condition/", myfiles_4[i]))
  dataset_4 <- rbind(dataset_4, temp)
}

############### uncertainty 5 = parameter uncertainty
myfiles_5 <- list.files(path = paste0(fcr_folder, "/uncertainty_5_parameter"), pattern = "*7day.csv")
dataset_5 <- read.csv(paste0(fcr_folder, "/uncertainty_5_parameter/", myfiles_5[1]))
# read in files
for (i in 2:length(myfiles_5)) {
  temp <- read.csv(paste0(fcr_folder, "/uncertainty_5_parameter/", myfiles_5[i]))
  dataset_5 <- rbind(dataset_5, temp)
}

############### uncertainty 6 = discharge driver uncertainty
myfiles_6 <- list.files(path = paste0(fcr_folder, "/uncertainty_6_discharge"), pattern = "*7day.csv")
dataset_6 <- read.csv(paste0(fcr_folder, "/uncertainty_6_discharge/", myfiles_6[1]))
# read in files
for (i in 2:length(myfiles_6)) {
  temp <- read.csv(paste0(fcr_folder, "/uncertainty_6_discharge/", myfiles_6[i]))
  dataset_6 <- rbind(dataset_6, temp)
}


dataset <- fcr_fcasts %>% select(forecast_date, forecast_run_day, day_in_future, forecast_variance)
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


##########################################################################################################################################################################
# time series with the total variance plotted on top
##################################
# week 1 aka day 7

temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==7,]
temp <- temp[temp$forecast_run_day>'2019-01-01',]
var <- temp[temp$variable=='total_var',]
temp <- temp[!temp$variable=='total_var',]
#### THERE IS ONE STUPID DUPLICATED DATE THAT IS STACKING ON TOP AND CREATING A WHITE LINE BELOW
#### BRUTE FORCE REMOVING IT FROM EACH UNCERTAINTY SOURCE
process_prop <- temp[temp$variable=='process_prop',]
process_prop <- process_prop[!duplicated(process_prop$forecast_date),]

weather_prop <- temp[temp$variable=='weather_prop',]
weather_prop <- weather_prop[!duplicated(weather_prop$forecast_date),]

discharge_prop <- temp[temp$variable=='discharge_prop',]
discharge_prop <- discharge_prop[!duplicated(discharge_prop$forecast_date),]

IC_prop <- temp[temp$variable=='IC_prop',]
IC_prop <- IC_prop[!duplicated(IC_prop$forecast_date),]

parameter_prop <- temp[temp$variable=='parameter_prop',]
parameter_prop <- parameter_prop[!duplicated(parameter_prop$forecast_date),]

temp <- rbind(process_prop, weather_prop)
temp <- rbind(temp, discharge_prop)
temp <- rbind(temp, IC_prop)
temp <- rbind(temp, parameter_prop)
temp$variable <- as.factor(temp$variable)

pf <- ggplot() 
pf <- pf + 
  geom_area(data = temp, aes(x = forecast_date, y = measurement, fill = variable)) + 
  xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  ggtitle(paste0('FCR Weekly Forecast, Day 7')) +
  scale_x_date( expand = c(0,0),labels = scales::date_format('%b'), date_breaks = '3 months') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_text(size =25),
        axis.title.y = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30),
        legend.position = 'none')
pf <- pf + geom_line(data = var, aes(x = forecast_date, y = measurement/0.5), lwd = 1.5) +
  scale_y_continuous(expand = c(0,0), sec.axis = sec_axis(~.*0.5)) 

png(paste0(fcr_folder, '/Weekly_Week',1, '_Uncertainty_Variance_TimeSeries.png'), width = 1200, height = 785)
pf
dev.off()

#####################################
# week 2 aka day 14

temp <- uncert_proportion_long[uncert_proportion_long$day_in_future==14,]
temp <- temp[temp$forecast_run_day>'2019-01-01',]
var <- temp[temp$variable=='total_var',]
temp <- temp[!temp$variable=='total_var',]
#### THERE IS ONE STUPID DUPLICATED DATE THAT IS STACKING ON TOP AND CREATING A WHITE LINE BELOW
#### BRUTE FORCE REMOVING IT FROM EACH UNCERTAINTY SOURCE
process_prop <- temp[temp$variable=='process_prop',]
process_prop <- process_prop[!duplicated(process_prop$forecast_date),]

weather_prop <- temp[temp$variable=='weather_prop',]
weather_prop <- weather_prop[!duplicated(weather_prop$forecast_date),]

discharge_prop <- temp[temp$variable=='discharge_prop',]
discharge_prop <- discharge_prop[!duplicated(discharge_prop$forecast_date),]

IC_prop <- temp[temp$variable=='IC_prop',]
IC_prop <- IC_prop[!duplicated(IC_prop$forecast_date),]

parameter_prop <- temp[temp$variable=='parameter_prop',]
parameter_prop <- parameter_prop[!duplicated(parameter_prop$forecast_date),]

temp <- rbind(process_prop, weather_prop)
temp <- rbind(temp, discharge_prop)
temp <- rbind(temp, IC_prop)
temp <- rbind(temp, parameter_prop)


pf2 <- ggplot() 
pf2 <- pf2 + geom_area(data = temp, aes(x = forecast_date, y = measurement, fill = variable)) + 
  xlab('Date') +
  ylab('Proportion of Variance') +
  scale_fill_manual(labels = c('discharge', 'IC', 'parameter', 'process', 'meteorological') ,
                    values = c('#4472C4', '#92D050', '#660066', '#C55A11', '#FFC000'),
                    name = "Uncertainty Type") +
  ggtitle(paste0('FCR Weekly Forecast, Day 14')) +
  scale_x_date(expand = c(0,0),labels = scales::date_format('%b'), date_breaks = '3 months') +
  theme_bw() +
  theme(axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25),
        axis.title.x = element_text(size =25),
        axis.title.y = element_text(size = 25),
        legend.title = element_text(size = 25),
        legend.text = element_text(size = 20),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 30),
        legend.position = 'none')
pf2 <- pf2 + geom_line(data = var, aes(x = forecast_date, y = measurement/0.5), lwd = 1.5) +
  scale_y_continuous(expand = c(0,0),sec.axis = sec_axis(~.*0.5, name = expression (paste("Total Variance (",~μg/L^2,")"  )) )) 

png(paste0(fcr_folder, '/Weekly_Week',2, '_Uncertainty_Variance_TimeSeries.png'), width = 1200, height = 785)
pf2
dev.off()

png(paste0(fcr_folder, '/all_UC.png'), width = 1200, height = 785)
ggarrange(p, p2, pf, pf2)
dev.off()


