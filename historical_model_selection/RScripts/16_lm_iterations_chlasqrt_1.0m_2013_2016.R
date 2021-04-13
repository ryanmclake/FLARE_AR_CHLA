# read in selected correlation matrices for each year and entire dataset to pair selected variables back with dataset
# and then run iteratire linear model selection

#install.packages("MuMIn")
#install.packages("rsq")
library(MuMIn)
library(knitr)
library(rsq)
library(tidyverse)
library(Metrics)

#all data
data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")

##################################################################################################################################################3
####################################2016 dataset, selected variables #################################
# read in correlation matrix for selected predictable variable names
varall <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_2016_selected.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data[,colnames(varall)]
dataall$Date <- as.Date(dataall$Date)
# subset after 06-27-2013 because there are NAs in one of the variables, so the model won't run
data1316 <- dataall[dataall$Date>"2013-06-27",]
# remove TP_log because there are NAs after 2014 which will cause dredge to fail
data1316 <- data1316 %>% select(-TP_log)

# build a global model with the selected variables and then use dredge to see which combinations have the lowest AICc values
model_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  + SpCond_uScm + DO_mgL + Turb_NTU_log + NH4_inf_log 
                  + Temp_inf_max + WindSpeed_median_log + RelHum_median + ShortWave_mean,
                  data = data1316, family = gaussian, na.action = 'na.fail')
glm_1316 <- dredge(model_1316, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_1316 <- subset(glm_1316, delta<2 )

# build the two individual models selected from dredge
mod1_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + DO_mgL +ShortWave_mean + SpCond_uScm + Temp_inf_max
                 + Turb_NTU_log, data = data1316, family = gaussian, na.action = 'na.fail')
mod2_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + ShortWave_mean + SpCond_uScm + Turb_NTU_log,
                 data = data1316, family = gaussian, na.action = 'na.fail')
mod3_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + DO_mgL + ShortWave_mean + SpCond_uScm + Turb_NTU_log,
                 data = data1316, family = gaussian, na.action = 'na.fail')
mod4_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + ShortWave_mean + SpCond_uScm + Temp_inf_max + Turb_NTU_log,
                 data = data1316, family = gaussian, na.action = 'na.fail')



# make predictions with the models
pred1_1316 <- predict(mod1_1316, newdata = data1316)
pred2_1316 <- predict(mod2_1316, newdata = data1316)
pred3_1316 <- predict(mod3_1316, newdata = data1316)
pred4_1316 <- predict(mod4_1316, newdata = data1316)


# plot the predictions for the 2014 training dataset
plot(data1316$Date, data1316$Chla_sqrt, type = 'l')
points(data1316$Date, pred1_1316, col = 'red', type = 'l')
points(data1316$Date, pred2_1316, col = 'orange', type = 'l')
points(data1316$Date, pred3_1316, col = 'gold', type = 'l')
points(data1316$Date, pred4_1316, col = 'blue', type = 'l')
title("Selected models 2013-2016 dataset")


plot(data1316$Chla_sqrt, type = 'l')
points(pred1_1316, col = 'red', type = 'l')
points(data1316$Date, pred2_1316, col = 'orange', type = 'l')
points(data1316$Date, pred3_1316, col = 'gold', type = 'l')
points(data1316$Date, pred4_1316, col = 'blue', type = 'l')
title("Selected models 2013-2016 dataset")

# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_1316, type = 'sse')), digits = 3)
round((rsq(mod2_1316, type = 'sse')), digits = 3)
round((rsq(mod3_1316, type = 'sse')), digits = 3)
round((rsq(mod4_1316, type = 'sse')), digits = 3)



rmse(pred1_1316, data1316$Chla_sqrt)
rmse(pred2_1316, data1316$Chla_sqrt)
rmse(pred3_1316, data1316$Chla_sqrt)
rmse(pred4_1316, data1316$Chla_sqrt)

