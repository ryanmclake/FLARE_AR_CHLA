# read in selected correlation matrices for each year and entire dataset to pair selected variables back with dataset
# and then run iteratire linear model selection

#install.packages("MuMIn")
#install.packages("rsq")
library(MuMIn)
library(knitr)
library(rsq)
library(tidyverse)

#all data
data <- read.csv("./historical_model_selection/ARIMA_data/data_arima_WW.csv")

##################################################################################################################################################3
####################################2013 dataset, predictable variables only #################################
# read in correlation matrix for selected predictable variable names
varall <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_selected_predictabledrivers.csv")
varall <- varall %>% select(-X)
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data# %>% select(colnames(varall))
# subset to get rid of NA's at beginning so that the model will run. this truncates dataset to May 15 2013
dataall$Date <- as.Date(dataall$Date)
dataall <- dataall[dataall$Date>"2013-05-09" & dataall$Date<"2014-01-01",]

#write.csv(dataall, "./historical_model_selection/ARIMA_data/selected model files/2013_2016_selected_variables.csv", row.names = FALSE)


################################################################################################################################
#######################################################################################################################
#### there are several different temperature metrics that are correlated with each other ##############################
#### build a global model for each of the different temperature variables 

# Model including TempC and other predictable varibales that are not correlated to TempC
model_tempC <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ Temp_C + mean_flow + Rain_sum_log + WindSpeed_max_log + 
                     ShortWave_mean  , data = dataall, family = gaussian, na.action = "na.fail" )
glm_tempC <- dredge(model_tempC, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_tempC <- subset(glm_tempC, delta<2 )

################################################################################################################################
################################# do again using temp inf mean ########################################################

# Model including Temp inf mean and other predictable varibales that are not correlated to Temp inf mean
model_tempinf <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ Temp_inf_mean + mean_flow + Rain_sum_log + WindSpeed_max_log + 
                       ShortWave_mean  , data = dataall, family = gaussian, na.action = "na.fail" )
glm_tempinf <- dredge(model_tempinf, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_tempinf <- subset(glm_tempinf, delta<2 )


################################################################################################################################
################################# do again using air temp mean ########################################################

# Model including air temp mean and other predictable varibales that are not correlated to Temp inf mean
# SW mean needs to be removed
model_airtemp <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ AirTemp_mean_log + mean_flow + Rain_sum_log + WindSpeed_max_log, 
                     data = dataall, family = gaussian, na.action = "na.fail" )
glm_airtemp <- dredge(model_airtemp, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_airtemp <- subset(glm_airtemp, delta<2 )

# looking at select_airtemp, AICc values for this set of models are much higher so the models are not as good
# as those including tempC and tempinf, so these model are not built individually

# build individual models for those within 2 AICc units
#mod1_tempinf is the same as mod1_tempC (mean flow and SW mean)
mod1_tempC <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow + ShortWave_mean,
                data = dataall, family = gaussian, na.action = "na.fail")
pred1_tempC <- predict(mod1_tempC, newdata=dataall)

# mod2_tempinf is the same as mod1_tempC (mean flow, log of rain sum, and SW mean)
mod2_tempC <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow + Rain_sum_log + ShortWave_mean,
                  data = dataall, family = gaussian, na.action = "na.fail")
pred2_tempC <- predict(mod2_tempC, newdata=dataall)

mod3_tempinf <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow + ShortWave_mean + Temp_inf_mean,
                    data = dataall, family = gaussian, na.action = "na.fail")
pred3_tempinf <-  predict(mod3_tempinf, newdata=dataall)


# plot actual chla and models predictions on the same plot
plot(dataall$Date, dataall$Chla_sqrt, type = 'l')
points(dataall$Date, pred1_tempC, col = 'red', type = 'l')
points(dataall$Date, pred2_tempC, col = 'orange', type = 'l')
points(dataall$Date, pred3_tempinf, col = 'purple', type = 'l')

# plot actual versus predicted for each model
plot(pred1_tempC, dataall$Chla_sqrt, xlim= c(0, 3.0), ylim = c(0,3))
abline(0,1)
title("Obs vs. Pred, MeanFlow+SWmean")

plot(pred2_tempC, dataall$Chla_sqrt, xlim= c(0, 3.0), ylim = c(0,3))
abline(0,1)
title("Obs vs. Pred, MeanFlow+Rain+SWmean")

plot(pred3_tempinf, dataall$Chla_sqrt, xlim= c(0, 3.0), ylim = c(0,3))
abline(0,1)
title("Obs vs. Pred, MeanFlow+SWmean+Tempinf")


# calculate R^2 for each model (variation from the 1:1 line or Efron's R^2)
r2_mod1_tempC <- round((rsq(mod1_tempC, type = 'sse')), digits = 3)
r2_mod2_tempC <-  round((rsq(mod2_tempC, type = 'sse')), digits = 3)
r2_mod3_tempC <- round((rsq(mod3_tempinf, type = 'sse')), digits = 3)

# calculate RMSE for each model
rmse(pred1_tempC, dataall$Chla_sqrt)
rmse(pred2_tempC, dataall$Chla_sqrt)
rmse(pred3_tempinf, dataall$Chla_sqrt)


#####################################################################################################################################
######################################################################################################################################
################### now compare to variables selected regardless of predictability ###########################################################

#all data
data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")

var13 <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_selected.csv")
# insert an empty date column so that it can be matched with the dataset
var13$Date <- NA
var13 <- var13%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
data13 <- data[,colnames(var13)]
data13$Date <- as.Date(data13$Date)
data13 <- data13[data13$Date>"2013-06-20" & data13$Date<"2014-01-01",]

# remove TP_load_log because there is not data the entire time
model_2013 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  + NO3NO2_log + SRP_log +  Temp_inf_mean +
                    RelHum_max_log + Rain_sum_log + WindSpeed_mean_log + ShortWave_mean,
                  data = data13, family = gaussian, na.action = 'na.fail')
glm_2013 <- dredge(model_2013, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_2013 <- subset(glm_2013, delta<2 )

# build the two individual models selected from dredge
mod1_2013 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + SRP_log + ShortWave_mean,
                 data = data13, family = gaussian, na.action = 'na.fail')
mod2_2013 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  SRP_log +  Temp_inf_mean,
                 data = data13, family = gaussian, na.action = 'na.fail')


# make predictions with the models
pred1_2013 <- predict(mod1_2013, newdata = data13)
pred2_2013 <- predict(mod2_2013, newdata = data13)

# plot the predictions
plot(data13$Date, data13$Chla_sqrt, type = 'l')
points(data13$Date, pred1_2013, col = 'red', type = 'l')
points(data13$Date, pred2_2013, col = 'orange', type = 'l')


# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_2013, type = 'sse')), digits = 3)
round((rsq(mod2_2013, type = 'sse')), digits = 3)

rmse(pred1_2013, data13$Chla_sqrt)
rmse(pred2_2013, data13$Chla_sqrt)


# run the model truncated to before TP_load turns to NAs, 9-25-2013
# subset to just 2013 and to get rid of NA's at beginning so that the model will run. 
data13_short <- data13[data13$Date>"2013-06-20" & data13$Date<"2013-10-04",]

model_2013_short <- glm(Chla_sqrt~Chla_ARlag1_sqrt  + TP_load_log + NO3NO2_log + SRP_log +  Temp_inf_mean +
                    RelHum_max_log + Rain_sum_log + WindSpeed_mean_log + ShortWave_mean,
                  data = data13_short, family = gaussian, na.action = 'na.fail')
glm_2013_short <- dredge(model_2013_short, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_2013_short <- subset(glm_2013_short, delta<2 )

# build the individual model selected from dredge
mod1_2013_short <- glm(Chla_sqrt~Chla_ARlag1_sqrt  + TP_load_log + RelHum_max_log + SRP_log, 
                       data = data13_short, family = gaussian, na.action = 'na.fail' )

# make predictions with the models
pred1_2013_short <- predict(mod1_2013_short, newdata = data13_short)

# plot the predictions
plot(data13_short$Date, data13_short$Chla_sqrt, type = 'l')
points(data13_short$Date, pred1_2013_short, col = 'red', type = 'l')


# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_2013_short, type = 'sse')), digits = 3)
rmse(pred1_2013_short, data13_short$Chla_sqrt)

# plot all the model predictions together
plot(data13_short$Date, data13_short$Chla_sqrt, type = 'l')
points(data13_short$Date, pred1_2013_short, col = 'red', type = 'l')
points(data13$Date, pred1_2013, col = 'purple', type = 'l')
points(data13$Date, pred2_2013, col = 'orange', type = 'l')

