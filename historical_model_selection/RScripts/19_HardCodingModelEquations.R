library(MuMIn)
library(knitr)
library(rsq)
library(tidyverse)
library(Metrics)

#all data
data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)
data <- data[data$Date>"2013-05-09",]

##################################################################################################################################################3
####################################2013 dataset, predictable variables only #################################
# read in correlation matrix for selected predictable variable names
varpred <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_selected_predictabledrivers.csv")
varpred <- varpred %>% select(-X)
# insert an empty date column so that it can be matched with the dataset
varpred$Date <- NA
varpred <- varpred%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
datapred <- data[,colnames(varpred)]
# subset to get rid of NA's at beginning so that the model will run and to 2013 only
datapred$Date <- as.Date(datapred$Date)
datapred <- datapred[datapred$Date>"2013-05-09" & datapred$Date<"2014-01-01",]
# datapred is the dataset for predictable variables only within 2013

################################################################################################################################
#######################################################################################################################
#### there are several different temperature metrics that are correlated with each other ##############################
#### build a global model for each of the different temperature variables 

# Model including TempC and other predictable varibales that are not correlated to TempC
model_tempC <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ Temp_C + mean_flow + Rain_sum_log + WindSpeed_max_log + 
                     ShortWave_mean  , data = datapred, family = gaussian, na.action = "na.fail" )
glm_tempC <- dredge(model_tempC, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_tempC <- subset(glm_tempC, delta<2 )

# build individual models for those within 2 AICc units
mod1_tempC <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow + ShortWave_mean + Temp_C,
                  data = datapred, family = gaussian, na.action = "na.fail")
pred1_tempC <- predict(mod1_tempC, newdata=datapred)

mod2_tempC <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow + Temp_C + WindSpeed_max_log,
                  data = datapred, family = gaussian, na.action = "na.fail")
pred2_tempC <- predict(mod2_tempC, newdata=datapred)

mod3_tempC <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow + Temp_C + WindSpeed_max_log +ShortWave_mean,
                  data = datapred, family = gaussian, na.action = "na.fail")
pred3_tempC <- predict(mod3_tempC, newdata=datapred)


# plot predicted and observed for 2013 only
plot(datapred$Date, (datapred$Chla_sqrt)^2, type = 'l', xlab = 'Date', ylab = 'Chla (ug/L)')
#points(datapred$Date, (pred1_tempC)^2, col = 'red', type = 'l')
points(datapred$Date, (pred2_tempC)^2, col = 'orange', type = 'l')
points(datapred$Date, (pred3_tempC)^2, col = 'purple', type = 'l')
title('Predictable Variables, 2013 dataset')
legend('topright', c("Observed", "Mod1"),cex = 0.75, col = c('black',  'orange'), lty = c(1,1), bty = 'n')

# plot predicted and observed for 2013-2016
# first make predictions for the models for the entire dataset
pred1_tempC_alldata <- predict(mod1_tempC, newdata = data)
pred2_tempC_alldata <- predict(mod2_tempC, newdata = data)
pred3_tempC_alldata <- predict(mod3_tempC, newdata = data)

plot( (data$Chla_sqrt)^2, type = 'l', xlab = 'Date', ylab = 'Chla (ug/L)')
#points( (pred1_tempC_alldata)^2, col = 'red', type = 'l')
points( (pred2_tempC_alldata)^2, col = 'orange', type = 'l')
#points( (pred3_tempC_alldata)^2, col = 'purple', type = 'l')
title('Predictable Variables Model, 2013-2016 dataset')
legend('topright', inset = 0.08,c("Observed", "Mod1"),cex = 0.75, col = c('black', 'orange'), lty = c(1,1), bty = 'n')


# there is a proble with initial conditions, which is why the model isn't doing well after 2013 (the May-Oct jump)
# try hard-coding in the equation and running for just one year
# the problem seems to be more because the model was built on just one year and not the whole dataset

data14 <- data[data$Date>"2014-01-01" & data$Date < "2014-12-31",]
data14$Chla_ARlag1_sqrt[data14$Date=="2014-05-06"] <- 1
data14 <- data14 %>% mutate(pred = (1.66 + 0.53*Chla_ARlag1_sqrt + 15.08*mean_flow - 0.001*ShortWave_mean 
                                            - 0.04*Temp_C)) %>% select(Date, Chla_sqrt, pred, everything()) %>%
  mutate(pred1316 = -1.56 + 0.44*Chla_ARlag1_sqrt + 0.07*DO_mgL - 0.002*ShortWave_mean + 0.05*SpCond_uScm +
         0.03*Temp_inf_max + 0.11*Turb_NTU_log)

plot(data14$Date, data14$Chla_sqrt , type = 'l', col = 'black')
points(data14$Date, data14$pred, type ='l', col = 'red')
points(data14$Date, data14$pred1316, type = 'l', col = 'blue')



data15 <- data[data$Date>"2015-01-01" & data$Date < "2015-12-31",]
data15$Chla_ARlag1_sqrt[data15$Date=="2015-05-05"] <- 1
# found an extra May 18, 2015? remove here but need to go back and find out where this originates
data15 <- data15[-3,]
data15 <- data15 %>% mutate(pred = (1.66 + 0.53*Chla_ARlag1_sqrt + 15.08*mean_flow - 0.001*ShortWave_mean 
                                    - 0.04*Temp_C)) %>%
  mutate(pred1316 = -1.56 + 0.44*Chla_ARlag1_sqrt + 0.07*DO_mgL - 0.002*ShortWave_mean + 0.05*SpCond_uScm +
           0.03*Temp_inf_max + 0.11*Turb_NTU_log)


plot(data15$Date, data15$Chla_sqrt , type = 'l', col = 'black')
points(data15$Date, data15$pred, type ='l', col = 'red')
points(data15$Date, data15$pred1316, type = 'l', col = 'blue')

data13 <- data[data$Date>"2013-01-01" & data$Date< "2013-12-31",]
data13 <- data13 %>% mutate(pred = (1.66 + 0.53*Chla_ARlag1_sqrt + 15.08*mean_flow - 0.001*ShortWave_mean 
                                    - 0.04*Temp_C)) %>% select(Date, Chla_sqrt, pred, everything()) %>%
  mutate(pred1316 = -1.56 + 0.44*Chla_ARlag1_sqrt + 0.07*DO_mgL - 0.002*ShortWave_mean + 0.05*SpCond_uScm +
           0.03*Temp_inf_max + 0.11*Turb_NTU_log)


plot(data13$Date, data13$Chla_sqrt , type = 'l', col = 'black')
points(data13$Date, data13$pred, type ='l', col = 'red')
points(data13$Date, data13$pred1316, type = 'l', col = 'blue')

data16 <- data[data$Date>"2016-01-01" & data$Date < "2016-12-31",]
#data14$Chla_ARlag1_sqrt[data14$Date=="2016-05-06"] <- 1
data16 <- data16 %>% mutate(pred = (1.66 + 0.53*Chla_ARlag1_sqrt + 15.08*mean_flow - 0.001*ShortWave_mean 
                                    - 0.04*Temp_C)) %>% select(Date, Chla_sqrt, pred, everything()) %>%
  mutate(pred1316 = -1.56 + 0.44*Chla_ARlag1_sqrt + 0.07*DO_mgL - 0.002*ShortWave_mean + 0.05*SpCond_uScm +
           0.03*Temp_inf_max + 0.11*Turb_NTU_log)


plot(data16$Date, data16$Chla_sqrt , type = 'l', col = 'black')
points(data16$Date, data16$pred, type ='l', col = 'red')
points(data16$Date, data16$pred1316, type = 'l', col = 'blue')

data <- data %>% mutate(pred1316 = -1.56 + 0.44*Chla_ARlag1_sqrt + 0.07*DO_mgL - 0.002*ShortWave_mean + 0.05*SpCond_uScm +
                          0.03*Temp_inf_max + 0.11*Turb_NTU_log)

plot(data$Date, data$Chla_sqrt, type = 'l')
points(data$Date, data$pred1316, type = 'l', col = 'blue')

###########################################################################################################################
# now using model that was built on entire time period (2013-2016) for selected predictable variables

data <- data %>% mutate(predictable1 = 1.652033 + 0.4592437*Chla_ARlag1_sqrt - 3.050436*mean_flow - 
                          0.002515990*ShortWave_mean) %>%
  mutate(predictable2 = 1.133807 + 0.1902048*AirTemp_mean_log + 0.4669873*Chla_ARlag1_sqrt - 2.860906*mean_flow -
           0.002868180*ShortWave_mean) %>%
  mutate(predictable3 = 1.733681 + 0.4627520*Chla_ARlag1_sqrt -2.928539*mean_flow -0.002518041*ShortWave_mean -
           0.08286349*WindSpeed_mean_log)


plot(data$Chla_sqrt, type = 'l')
points(data$predictable1, type = 'l', col = 'red')
points(data$predictable2, type = 'l', col = 'yellow')
points(data$predictable3, type = 'l', col = 'purple')


round((rsq(mod1_1316, type = 'sse')), digits = 3)

R2 <- 1 - (sum((data$Chla_sqrt-data$predictable1 )^2)/sum((data$Chla_sqrt-mean(data$Chla_sqrt))^2))
my_RMSE<-sqrt(mean((data$predictable - data$Chla_sqrt)^2))
