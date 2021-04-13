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
varall <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_2016_selected_predictable.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data[,colnames(varall)]
dataall$Date <- as.Date(dataall$Date)
dataall <- dataall[dataall$Date>"2013-05-09",]
# because rmse retains native units, create a chl column that is not square rooted
dataall <- dataall %>% mutate(Chla_ugL = (Chla_sqrt)^2)


# build a global model with all possible predictor variables
model_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  mean_flow + AirTemp_mean_log + WindSpeed_mean_log + 
                    +RelHum_median + ShortWave_mean,
                  data = dataall, family = gaussian, na.action = 'na.fail')
glm_1316 <- dredge(model_1316, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_1316 <- subset(glm_1316, delta<2 )

write.csv(glm_1316, './historical_model_selection/ARIMA_data/weekly_all_models.csv', row.names = FALSE)

# now build selected models
mod1_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow +ShortWave_mean, 
                 data = dataall, family = gaussian, na.action = 'na.fail')

hist(mod1_1316$residuals)
confint(mod1_1316)
sd(mod1_1316$residuals)
summary(mod1_1316)

data_arima <- dataall %>% select(Date, Chla_sqrt, Chla_ARlag1_sqrt, mean_flow, ShortWave_mean)
#write.csv(data_arima, "./historical_model_selection/ARIMA_data/data_arima_WW.csv", row.names = FALSE)



#################################################################
#other models 
mod2_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow + AirTemp_mean_log + ShortWave_mean, 
                 data = dataall, family = gaussian, na.action = 'na.fail')
mod3_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow + WindSpeed_mean_log + ShortWave_mean, 
                 data = dataall, family = gaussian, na.action = 'na.fail')

# make predictions
pred1_1316 <- predict(mod1_1316, newdata = dataall)
pred2_1316 <- predict(mod2_1316, newdata = dataall)
pred3_1316 <- predict(mod3_1316, newdata = dataall)

# plot the predictions for the entire dataset
plot(dataall$Date, dataall$Chla_ugL, type = 'l')
points(dataall$Date, (pred1_1316)^2, col = 'red', type = 'l')
points(dataall$Date, (pred2_1316)^2, col = 'orange', type = 'l')
points(dataall$Date, (pred3_1316)^2, col = 'gold', type = 'l')

# calculate R2 and RMSE
round((rsq(mod1_1316, type = 'sse')), digits = 3)
round((rsq(mod2_1316, type = 'sse')), digits = 3)
round((rsq(mod3_1316, type = 'sse')), digits = 3)


r2 <- function (x, y) cor(x, y) ^ 2
r2(dataall$Chla_ugL, (pred1_1316)^2)
r2(dataall$Chla_ugL, (pred2_1316)^2)
r2(dataall$Chla_ugL, (pred3_1316)^2)


rmse((pred1_1316)^2, dataall$Chla_ugL)
rmse((pred1_1316)^2, (dataall$Chla_sqrt)^2)
rmse((pred2_1316)^2, dataall$Chla_ugL)
rmse((pred3_1316)^2, dataall$Chla_ugL)


# work with the bias calculations
bias_arima <- sd((pred1_1316^2))/sd(dataall$Chla_ugL) # non-biased estimate = 1
bias(dataall$Chla_ugL, (pred1_1316)^2)
mean(dataall$Chla_ugL-(pred1_1316^2))
actual <- c(1, 2, 3, 4)
predicted <- c(1, 2, 3, 4)
bias(actual, predicted)

par(mfrow = c(2,2))
plot((dataall$Chla_ARlag1_sqrt)^2, (dataall$Chla_sqrt)^2, xlab = "Chla t-1 (ug/L)", ylab ="Chla (ug/L)" )
plot(dataall$mean_flow, dataall$Chla_ugL, xlab = "Mean flow (m3/s)", ylab = "Chla (ug/L)")
plot(dataall$ShortWave_mean, dataall$Chla_ugL, xlab = "Mean Shortwave (W/m2)", ylab = "Chla (ug/L)")

