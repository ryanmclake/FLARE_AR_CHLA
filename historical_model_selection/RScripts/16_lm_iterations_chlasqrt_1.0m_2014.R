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
varall <- read.csv("./correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2014_selected.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data[,colnames(varall)]
# subset to 2014 data only 
dataall$Date <- as.Date(dataall$Date)
data14 <- dataall[dataall$Date>"2014-01-01" & dataall$Date<"2015-01-01",]

#write.csv(dataall, "./selected model files/2013_2016_selected_variables.csv", row.names = FALSE)

# build a global model and then use dredge to see which combinations have the lowest AICc values

# remove TP_load_log because there is not data the entire time
model_2014 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  + TP_log + TN_log + NH4_inf_log + 
                    DOC_inf_log + SRP_inf + NH4NO3NO2_SRP_log + flow_min +  Temp_inf_max +
                     WindSpeed_median_log + ShortWave_max,
                  data = data14, family = gaussian, na.action = 'na.fail')
glm_2014 <- dredge(model_2014, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select_2014 <- subset(glm_2014, delta<2 )

# build the two individual models selected from dredge
mod1_2014 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + DOC_inf_log + flow_min + ShortWave_max,
                 data = data14, family = gaussian, na.action = 'na.fail')
mod2_2014 <- glm(Chla_sqrt~Chla_ARlag1_sqrt  +  DOC_inf_log +  flow_min,
                 data = data14, family = gaussian, na.action = 'na.fail')


# make predictions with the models
pred1_2014 <- predict(mod1_2014, newdata = data14)
pred2_2014 <- predict(mod2_2014, newdata = data14)
# and a prediction for the entire dataset using the model trained on 2014 only
pred1_2014_all <- predict(mod1_2014, newdata = dataall)
pred2_2014_all <- predict(mod2_2014, newdata = dataall)



# plot the predictions for the 2014 training dataset
plot(data14$Date, (data14$Chla_sqrt)^2, type = 'l', xlab = "Date", lwd = 3, ylab = 'Chlorophyll a (ug/L)', ylim = c(0,14))
points(data14$Date, (pred1_2014)^2, col = 'red', type = 'l', lwd = 3)
legend(cex = 1.5, 'topleft', c('Observed', 'Modeled'), col = c('black', 'red'), lty = c(1,1), bty = 'n')
#points(data14$Date, (pred2_2014)^2, col = 'slateblue', type = 'l')
title("2014")

# plot the predictions for the 2014 model on the entire dataset
plot(dataall$Date, dataall$Chla_sqrt, type = 'l')
points(dataall$Date, pred1_2014_all, col = 'red', type = 'l')
points(dataall$Date, pred2_2014_all, col = 'orange', type = 'l')
title("Selected models for 2014, 2013-2016 dataset")


# calculate diagnostic statistics, R2 and RMSE
round((rsq(mod1_2014, type = 'sse')), digits = 3)
round((rsq(mod2_2014, type = 'sse')), digits = 3)

rmse(pred1_2014, data14$Chla_sqrt)
rmse(pred2_2014, data14$Chla_sqrt)

