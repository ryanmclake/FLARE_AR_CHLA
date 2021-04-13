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
####################################2014 dataset, selected variables #################################
# read in correlation matrix for selected predictable variable names
varall <- read.csv("./correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2015_selected.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data[,colnames(varall)]
# subset to 2015 data only 
dataall$Date <- as.Date(dataall$Date)
data15 <- dataall[dataall$Date>"2015-01-01" & dataall$Date<"2016-01-01",]
data15 <- data15 %>% select(-Kd)

#write.csv(dataall, "./selected model files/2013_2016_selected_variables.csv", row.names = FALSE)

# build a global model and then use dredge to see which combinations have the lowest AICc values

# remove TP_load_log because there is not data the entire time
model_2015 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  + DO_mgL + SpCond_uScm + Turb_NTU_log +
                    SRP_inf + TN_TP_inf_log + NH4_load_log + Temp_inf_min + Rain_sum_log +
                     WindSpeed_mean_log + ShortWave_max,
                  data = data15, family = gaussian, na.action = 'na.fail')
glm_2015 <- dredge(model_2015, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_2015 <- subset(glm_2015, delta<2 )

# build the two individual models selected from dredge
mod1_2015 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + NH4_load_log + SpCond_uScm ,
                 data = data15, family = gaussian, na.action = 'na.fail')
mod2_2015 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  NH4_load_log + SpCond_uScm + TN_TP_inf_log,
                 data = data15, family = gaussian, na.action = 'na.fail')
mod3_2015 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  NH4_load_log + SpCond_uScm + Temp_inf_min,
                 data = data15, family = gaussian, na.action = 'na.fail')
mod4_2015 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  NH4_load_log + SpCond_uScm + Turb_NTU_log,
                 data = data15, family = gaussian, na.action = 'na.fail')
mod5_2015 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  NH4_load_log + SpCond_uScm + DO_mgL,
                 data = data15, family = gaussian, na.action = 'na.fail')
mod6_2015 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  NH4_load_log + SpCond_uScm + SRP_inf,
                 data = data15, family = gaussian, na.action = 'na.fail')



# make predictions with the models
pred1_2015 <- predict(mod1_2015, newdata = data15)
pred2_2015 <- predict(mod2_2015, newdata = data15)
pred3_2015 <- predict(mod3_2015, newdata = data15)
pred4_2015 <- predict(mod4_2015, newdata = data15)
pred5_2015 <- predict(mod5_2015, newdata = data15)
pred6_2015 <- predict(mod6_2015, newdata = data15)


# and a prediction for the entire dataset using the model trained on 2014 only
pred1_2015_all <- predict(mod1_2015, newdata = dataall)
pred2_2015_all <- predict(mod2_2015, newdata = dataall)
pred3_2015_all <- predict(mod3_2015, newdata = dataall)
pred4_2015_all <- predict(mod4_2015, newdata = dataall)
pred5_2015_all <- predict(mod5_2015, newdata = dataall)
pred6_2015_all <- predict(mod6_2015, newdata = dataall)



# plot the predictions for the 2014 training dataset
plot(data15$Date, (data15$Chla_sqrt)^2, lwd = 3, type = 'l', xlab = "Date", ylab = "Chlorophyll a (ug/L)", ylim = c(0,14))
#points(data15$Date, (pred1_2015)^2, col = 'red', type = 'l')
points(data15$Date, (pred2_2015)^2, col = 'red', lwd = 3, type = 'l')
legend(cex = 1.5, 'topleft', c('Observed', 'Modeled'), col = c('black', 'red'), lty = c(1,1), bty = 'n')
#points(data15$Date, (pred3_2015)^2, col = 'gold', type = 'l')
#points(data15$Date, (pred4_2015)^2, col = 'blue', type = 'l')
#points(data15$Date, (pred5_2015)^2, col = 'green', type = 'l')
#points(data15$Date, (pred6_2015)^2, col = 'purple', type = 'l')
title("2015")




# plot the predictions for the 2014 model on the entire dataset
plot(dataall$Date, dataall$Chla_sqrt, type = 'l')
points(dataall$Date, pred1_2015_all, col = 'red', type = 'l')
points(dataall$Date, pred2_2015_all, col = 'orange', type = 'l')
points(dataall$Date, pred3_2015_all, col = 'gold', type = 'l')
points(dataall$Date, pred4_2015_all, col = 'blue', type = 'l')
points(dataall$Date, pred5_2015_all, col = 'green', type = 'l')
points(dataall$Date, pred6_2015_all, col = 'purple', type = 'l')
title("Selected models for 2015, 2013-2016 dataset")


# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_2015, type = 'sse')), digits = 3)
round((rsq(mod2_2015, type = 'sse')), digits = 3)
round((rsq(mod3_2015, type = 'sse')), digits = 3)
round((rsq(mod4_2015, type = 'sse')), digits = 3)
round((rsq(mod5_2015, type = 'sse')), digits = 3)
round((rsq(mod6_2015, type = 'sse')), digits = 3)



rmse(pred1_2015, data15$Chla_sqrt)
rmse(pred2_2015, data15$Chla_sqrt)
rmse(pred3_2015, data15$Chla_sqrt)
rmse(pred4_2015, data15$Chla_sqrt)
rmse(pred5_2015, data15$Chla_sqrt)
rmse(pred6_2015, data15$Chla_sqrt)

