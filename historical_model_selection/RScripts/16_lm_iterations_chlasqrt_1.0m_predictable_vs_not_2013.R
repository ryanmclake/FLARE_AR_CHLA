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



# see how model does without inflow
mod4_tempC <- glm(Chla_sqrt~Chla_ARlag1_sqrt + Temp_C + WindSpeed_max_log +ShortWave_mean,
                  data = datapred, family = gaussian, na.action = "na.fail")
pred4_tempC <- predict(mod4_tempC, newdata= datapred)

plot(datapred$Date, (datapred$Chla_sqrt)^2, type = 'l', xlab = 'Date', ylab = 'Chla (ug/L)')
points(datapred$Date, (pred4_tempC)^2, col = 'red', type = 'l')
# calculate R2
round((rsq(mod4_tempC, type = 'sse')), digits = 3)
# calculate RMSE for each model
rmse(pred4_tempC, datapred$Chla_sqrt)



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

# try these models with no chlat-1
mod1_tempC_noAR <- glm(Chla_sqrt~ mean_flow + ShortWave_mean + Temp_C,
                      data = data, family = gaussian, na.action = "na.fail")
pred1_tempC_noAR <- predict(mod1_tempC_noAR, newdata=data)

mod2_tempC_noAR <- glm(Chla_sqrt~mean_flow + Temp_C + WindSpeed_max_log,
                      data = data, family = gaussian, na.action = "na.fail")
pred2_tempC_noAR <- predict(mod2_tempC_noAR, newdata=data)

mod3_tempC_noAR <- glm(Chla_sqrt~ mean_flow + Temp_C + WindSpeed_max_log +ShortWave_mean,
                      data = data, family = gaussian, na.action = "na.fail")
pred3_tempC_noAR <- predict(mod3_tempC_noAR, newdata=data)

plot( (data$Chla_sqrt)^2, type = 'l', xlab = 'Date', ylab = 'Chla (ug/L)')
points( (pred1_tempC_noAR)^2, col = 'red', type = 'l')
points( (pred2_tempC_noAR)^2, col = 'orange', type = 'l')
points( (pred3_tempC_noAR)^2, col = 'purple', type = 'l')
title('Predictable Variables no Chla(t-1) Models, 2013-2016 dataset')
legend('topright', c("Observed", "Mod1", "Mod2", "Mod3"),cex = 0.75, col = c('black', 'red', 'orange', 'purple'), lty = c(1,1), bty = 'n')

# calculate R2 for each model
round((rsq(mod1_tempC, type = 'sse', adj = TRUE)), digits = 3)
round((rsq(mod2_tempC, type = 'sse')), digits = 3)
round((rsq(mod3_tempC, type = 'sse')), digits = 3)
round((rsq(mod3_tempC_noAR, type = 'sse')), digits = 3)


# calculate RMSE for each model
rmse(pred1_tempC, datapred$Chla_sqrt)
rmse(pred2_tempC, datapred$Chla_sqrt)
rmse(pred3_tempC, datapred$Chla_sqrt)

# calculate RMSE for each model
rmse(pred1_tempC_alldata, data$Chla_sqrt)
rmse(pred2_tempC_alldata, data$Chla_sqrt)
rmse(pred3_tempC_alldata, data$Chla_sqrt)


################################################################################################################################
################################# do again using temp inf mean ########################################################

# Model including Temp inf mean and other predictable varibales that are not correlated to Temp inf mean
model_tempinf <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ Temp_inf_mean + mean_flow + Rain_sum_log + WindSpeed_max_log + 
                       ShortWave_mean  , data = datapred, family = gaussian, na.action = "na.fail" )
glm_tempinf <- dredge(model_tempinf, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_tempinf <- subset(glm_tempinf, delta<2 )

# build individual models for those within 2 AICc units
mod1_tempinf <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow + ShortWave_mean + Temp_inf_mean,
                  data = datapred, family = gaussian, na.action = "na.fail")
pred1_tempinf <- predict(mod1_tempinf, newdata=datapred)

# there are two other selected models for tempinf that could be built

# plot predicted vs observed
plot(datapred$Date, datapred$Chla_sqrt, type = 'l')
points(datapred$Date, pred1_tempinf, col = 'red', type = 'l')

# use this model to predict the entire dataset and compare to observed
pred1_tempinf_alldata <- predict.glm(mod1_tempinf, newdata = data)
plot(data$Date, data$Chla_sqrt, type = 'l')
points(data$Date, pred1_tempinf_alldata, col = 'red', type = 'l')

# calculate R2
round((rsq(mod1_tempinf, type = 'sse')), digits = 3)


# calculate RMSE
rmse(pred1_tempinf, datapred$Chla_sqrt)


################################################################################################################################
################################# do again using air temp mean ########################################################

# Model including air temp mean and other predictable varibales that are not correlated to Temp inf mean
# SW mean needs to be removed
model_airtemp <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ AirTemp_mean_log + mean_flow + Rain_sum_log + WindSpeed_max_log, 
                     data = datapred, family = gaussian, na.action = "na.fail" )
glm_airtemp <- dredge(model_airtemp, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_airtemp <- subset(glm_airtemp, delta<2 )

# build the individual models selected
mod1_tempair <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow  + AirTemp_mean_log,
                    data = datapred, family = gaussian, na.action = "na.fail")
pred1_tempair <-  predict(mod1_tempair, newdata=datapred)


# plot actual chla and models predictions on the same plot
plot(datapred$Date, datapred$Chla_sqrt, type = 'l')
points(datapred$Date, pred1_tempair, col = 'red', type = 'l')

# calculate R^2 for each model (variation from the 1:1 line or Efron's R^2)
round((rsq(mod1_tempair, type = 'sse')), digits = 3)

# calculate RMSE for each model
rmse(pred1_tempair, datapred$Chla_sqrt)

# use this model to predict the entire dataset and compare to observed
pred1_tempair_alldata <- predict.glm(mod1_tempair, newdata = data)
plot(data$Date, data$Chla_sqrt, type = 'l')
points(data$Date, pred1_tempair_alldata, col = 'red', type = 'l')

# what if the model is built on the whole dataset?
data <- data[data$Date>"2013-05-09",]
mod1_tempair_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+ mean_flow  + AirTemp_mean_log,
                    data = data, family = gaussian, na.action = "na.fail")
pred1_tempair_1316 <-  predict(mod1_tempair, newdata=data)
plot(data$Date, data$Chla_sqrt, type = 'l')
points(data$Date, pred1_tempair_1316, col = 'red', type = 'l')
# no difference from predicting with the model which was trained using 2013 data only

#####################################################################################################################################
######################################################################################################################################
################### now compare to variables selected regardless of predictability ###########################################################

var13 <- read.csv("./correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_selected.csv")
# insert an empty date column so that it can be matched with the dataset
var13$Date <- NA
var13 <- var13%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
data13 <- data[,colnames(var13)]
data13$Date <- as.Date(data13$Date)
data13 <- data13[data13$Date<"2014-01-01",]

# remove TP_load_log and use mean_flow instead because there is not data the entire time for TP load
model_2013 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  Temp_inf_mean + mean_flow +
                    RelHum_max_log + Rain_sum_log + WindSpeed_mean_log + ShortWave_mean,
                  data = data13, family = gaussian, na.action = 'na.fail')
glm_2013 <- dredge(model_2013, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_2013 <- subset(glm_2013, delta<2 )

# build the individual models selected from dredge (with the lowest degrees of freedom)
mod1_2013 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow + Temp_inf_mean + ShortWave_mean,
                 data = data13, family = gaussian, na.action = 'na.fail')
mod2_2013 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  mean_flow + ShortWave_mean,
                 data = data13, family = gaussian, na.action = 'na.fail')
mod3_2013 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  mean_flow + Temp_inf_mean,
                 data = data13, family = gaussian, na.action = 'na.fail')


# make predictions with the models
pred1_2013 <- predict(mod1_2013, newdata = data13)
pred2_2013 <- predict(mod2_2013, newdata = data13)
pred3_2013 <- predict(mod3_2013, newdata = data13)

# plot the predictions
plot(data13$Date, (data13$Chla_sqrt)^2, lwd = 3.0, type = 'l', xlab = 'Date', ylim = c(0,14),ylab = "Chlorophyll a (ug/L)", cex.main = 1.5)
points(data13$Date, (pred1_2013)^2, col = 'red', type = 'l', lwd = 3)
legend(cex = 1.5, 'topleft', c('Observed', 'Modeled'), col = c('black', 'red'), lty = c(1,1), bty = 'n')
#points(data13$Date, (pred2_2013)^2, col = 'orange', type = 'l')
#points(data13$Date, (pred3_2013)^2, col = 'purple', type = 'l')
title("2013", cex = 1.5)

# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_2013, type = 'sse')), digits = 3)
round((rsq(mod2_2013, type = 'sse')), digits = 3)
round((rsq(mod3_2013, type = 'sse')), digits = 3)

rmse(pred1_2013, data13$Chla_sqrt)
rmse(pred2_2013, data13$Chla_sqrt)
rmse(pred3_2013, data13$Chla_sqrt)

# use 2014 data only to see how this model does outside 2013
data14 <- data[data$Date > "2014-05-07" & data$Date < "2015-01-01",]
pred1_on2014 <- predict.glm(mod1_2013, newdata = data14)
plot(data14$Date, data14$Chla_sqrt, type = 'l')
points(data14$Date, pred1_on2014, type = 'l', col = 'blue')

# 2015 data only 
data15 <- data[data$Date > "2015-01-01" & data$Date < "2016-01-01",]
pred1_on2015 <- predict.glm(mod1_2013, newdata = data15)
plot(data15$Date, data15$Chla_sqrt)
points(data15$Date, pred1_on2015,  col = 'red')


# use this model to predict the entire dataset and compare to observed
pred1_2013_alldata <- predict.glm(mod1_2013, newdata = data)
pred2_2013_alldata <- predict.glm(mod2_2013, newdata = data)

plot(data$Date, data$Chla_sqrt, type = 'l')
points(data$Date, pred1_2013_alldata, col = 'red', type = 'l')
points(data$Date, pred2_2013_alldata, col = 'orange', type = 'l')
title("2013 selected models for 2013, 2013-2016 dataset")


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
title("TP load+SRP+Max RelHumidity, 2013")

# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_2013_short, type = 'sse')), digits = 3)
rmse(pred1_2013_short, data13_short$Chla_sqrt)

# use this model to predict the entire dataset and compare to observed
# select 2015 data only
data15 <- data[data$Date < "2016-01-01" & data$Date > "2015-04-01",]
pred1_2013short_alldata <- predict.glm(mod1_2013_short, newdata = data15)
plot(data15$Date, data15$Chla_sqrt, type = 'l')
plot(data15$Date, pred1_2013short_alldata, col = 'red', type = 'l')

# these models seem to all do pretty well within the 2013 dataset
# what about a model with just a random driver variable? how well does that do?

mod_DO <- glm(Chla_sqrt~Chla_ARlag1_sqrt  + DO_mgL,
              data = data, family = gaussian, na.action = 'na.omit')
pred_DO <- predict.glm(mod_DO, newdata = data)
plot(data$Date, data$Chla_sqrt, type = 'l')
points(data$Date, pred_DO, type = 'l', col = 'red')

mod_AR <- glm(Chla_sqrt~Chla_ARlag1_sqrt,
              data = data13, family = gaussian, na.action = 'na.omit')
pred_AR <- predict.glm(mod_AR, newdata = data)
plot( data$Chla_sqrt, type = 'l')
points( pred_AR, type = 'l', col = 'red')
title("Model including only Chl(t-1)")


pred_AR_2 <- predict.glm(mod_AR, newdata = data2)

# what about the variables chosen at 0.1m?

mod1 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Turb_NTU_log, data = data2, family = gaussian, na.action = na.fail)
pred.1 <- predict(mod1, newdata=data2)
mod2 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+NO3NO2_log+ShortWave_mean+Turb_NTU_log, data = data2, 
            family = gaussian, na.action = "na.omit")
pred.2 <- predict(mod2, newdata=data2)
mod3 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+Rain_sum_log+ShortWave_mean+Turb_NTU_log,data = data2, family = gaussian, na.action = na.fail)
pred.3 <- predict(mod3, newdata=data2)
mod4 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Temp_inf_max+Turb_NTU_log, data = data2, family = gaussian, na.action = "na.fail")
pred.4 <- predict(mod4, newdata=data2)
mod5 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+NO3NO2_log+Rain_sum_log+ShortWave_mean+Turb_NTU_log, data = data2, family = gaussian, na.action = "na.omit")
pred.5 <- predict(mod5, newdata=data2)

plot(data2$Date, (data2$Chla_sqrt)^2, type = 'l', xlab = 'Date', ylab = 'Chla (ug/L)')
#points( (pred.1)^2, col = 'red', type = 'l')
#points( (pred.2)^2, col = 'orange', type = 'l')
#points( (pred.3)^2, col = 'purple', type = 'l')
points((pred.4)^2, col = 'red', type = 'l')
points((pred.5)^2, col = 'orange', type = 'l')
title('Selected Variables Models, 2013-2016 dataset')
legend('topright', c("Observed", "Mod1", "Mod2"),cex = 0.75, col = c('black', 'red', 'orange'), lty = c(1,1), bty = 'n')



plot((data2$Chla_sqrt)^2, type = 'l', ylab = 'Chla (ug/L)')
points((pred_AR_2)^2, col = 'red', type ='l')
points((pred.1)^2, col = 'blue', type = 'l')
points((pred.3)^2, type = 'l', col = 'green')
points((pred.4)^2, type = 'l', col = 'yellow')
legend('topright', c("Observed", 'Mod Chl(t-1)', 'Mod1', 'Mod2', 'Mod3'),col= c("black", 'red', 'blue', 'green', 'yellow'),lty = c(1,1),bty='n' )


# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod_AR, type = 'sse', adj = TRUE)), digits = 3)
round((rsq(mod_DO, type = 'sse')), digits = 3)


rmse(pred_AR, data$Chla_sqrt)

# what about just mean_flow
mod_flow <- glm(Chla_sqrt~ mean_flow,
                data = datapred, family = gaussian, na.action = 'na.omit')
pred_flow <- predict(mod_flow, newdata = data)
plot(data$Chla_sqrt, type = 'l')
points(pred_flow, type = 'l', col = 'red')
title("Model including only mean flow")
round((rsq(mod_flow, type = 'sse')), digits = 3)


rmse(pred_flow, data$Chla_sqrt)

