library(tidyverse)
library(MuMIn)


data <- read.csv('./historical_model_selection/ARIMA_data/arima_data_fortnightly.csv')

# read in correlation matrix for selected predictable variable names
varall <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/selected_correlation_matrix_chlasqrt_2013_2016_fortnightly_data.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data %>% select(colnames(varall))
# subset to get rid of NA's at beginning so that the model will run. this truncates dataset to May 15 2013
dataall$Date <- as.Date(dataall$Date)
dataall <- dataall[dataall$Date>"2013-05-09" & dataall$Date<"2014-01-01",]

global_model <- glm(Chla_sqrt~Chla_ARlag_timestep_sqrt + mean_flow + Rain_sum_log + WindSpeed_median_log + ShortWave_mean,
                    data = dataall, family = gaussian, na.action = 'na.fail')
glm_global <- dredge(global_model, rank = "AICc", fixed = "Chla_ARlag_timestep_sqrt")  
select_global <- subset(glm_global, delta<2 )
