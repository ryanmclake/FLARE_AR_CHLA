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
data <- read.csv("model_transformed_chlasqrt_2013_2016.csv")

##################################################################################################################################################3
####################################2016 dataset, selected variables #################################
# read in correlation matrix for selected predictable variable names
varall <- read.csv("./correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2016_selected.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data[,colnames(varall)]
# subset to 2015 data only 
dataall$Date <- as.Date(dataall$Date)
data16 <- dataall[dataall$Date>"2016-01-01" & dataall$Date<"2017-01-01",]
# remove Kd because there are NAs which will cause dredge to fail
data16 <- data16 %>% select(-Kd)

# build a global model with the selected variables and then use dredge to see which combinations have the lowest AICc values
model_2016 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  + SpCond_uScm + TP_inf_log +
                    SRP_inf + NH4NO3_SRP_inf + NH4_load_log + TN_TP_inf_log + Rain_sum_log +
                     WindSpeed_median_log + RelHum_max_log,
                  data = data16, family = gaussian, na.action = 'na.fail')
glm_2016 <- dredge(model_2016, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_2016 <- subset(glm_2016, delta<2 )

# build the two individual models selected from dredge
mod1_2016 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + RelHum_max_log + SRP_inf + TN_TP_inf_log ,
                 data = data16, family = gaussian, na.action = 'na.fail')
mod2_2016 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + RelHum_max_log + SRP_inf + TN_TP_inf_log + NH4_load_log,
                 data = data16, family = gaussian, na.action = 'na.fail')
mod3_2016 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + RelHum_max_log + SRP_inf + TN_TP_inf_log + Rain_sum_log,
                 data = data16, family = gaussian, na.action = 'na.fail')



# make predictions with the models
pred1_2016 <- predict(mod1_2016, newdata = data16)
pred2_2016 <- predict(mod2_2016, newdata = data16)
pred3_2016 <- predict(mod3_2016, newdata = data16)


# and a prediction for the entire dataset using the model trained on 2014 only
pred1_2016_all <- predict(mod1_2016, newdata = dataall)
pred2_2016_all <- predict(mod2_2016, newdata = dataall)
pred3_2016_all <- predict(mod3_2016, newdata = dataall)



# plot the predictions for the 2014 training dataset
plot(data16$Date, (data16$Chla_sqrt)^2, lwd = 3, type = 'l', xlab = "Date", ylab = "Chlorophyll a (ug/L)", ylim = c(0,14))
#points(data16$Date, (pred1_2016)^2, col = 'violetred3', type = 'l')
points(data16$Date, (pred2_2016)^2, lwd = 3, col = 'red', type = 'l')
legend(cex = 1.5, 'topleft', c('Observed', 'Modeled'), col = c('black', 'red'), lty = c(1,1), bty = 'n')
#points(data16$Date, (pred3_2016)^2, col = 'gold', type = 'l')
title("2016")



# plot the predictions for the 2014 model on the entire dataset
plot(dataall$Date, dataall$Chla_sqrt, type = 'l')
points(dataall$Date, pred1_2016_all, col = 'red', type = 'l')
points(dataall$Date, pred2_2016_all, col = 'orange', type = 'l')
points(dataall$Date, pred3_2016_all, col = 'gold', type = 'l')



# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_2016, type = 'sse')), digits = 3)
round((rsq(mod2_2016, type = 'sse')), digits = 3)
round((rsq(mod3_2016, type = 'sse')), digits = 3)



rmse(pred1_2016, data16$Chla_sqrt)
rmse(pred2_2016, data16$Chla_sqrt)
rmse(pred3_2016, data16$Chla_sqrt)

