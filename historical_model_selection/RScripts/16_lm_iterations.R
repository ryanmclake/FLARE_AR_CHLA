# read in selected correlation matrices for each year and entire dataset to pair selected variables back with dataset
# and then run iteratire linear model selection

#install.packages("MuMIn")
#install.packages("rsq")
library(MuMIn)
library(knitr)
library(rsq)
library(tidyverse)

#all data
data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_2013_2016.csv")

##################################################################################################################################################3
##########################################2013-2016 dataset
varall <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/2013_2016_selected_correlations.csv")
varall$Date <- NA
varall$Temp_C <- NA
varall <- varall%>%select(Date, everything())
# get rid of the variables that have NAs for 2013 and/or 2014
varall <- varall%>%select(-(Kd:TN_inf_log))
dataall <- data[,colnames(varall)]
# subset to get rid of NA's at beginning? this truncates dataset to after June20
dataall$Date <- as.Date(dataall$Date)
dataall <- dataall[dataall$Date>"2013-06-20",]
#write.csv(dataall, "./selected model files/2013_2016_selected_variables.csv", row.names = FALSE)

# build a global model for all years
modelall <- glm(Chla_sqrt~Chla_ARlag1_sqrt+Turb_NTU_log+NH4_inf_log+flow_min+Temp_inf_min+RelHum_max_log+Rain_sum_log+
                  WindSpeed_max_log+ShortWave_mean, data = dataall, family = gaussian, na.action = "na.fail" )
glmall <- dredge(modelall, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
selectall <- subset(glmall, delta<2 )
selectall <- subset(selectall, selectall$df<8)

selectall <- subset(glmall, delta<2 )

# build individual models for those within 2 AICc units
mod1.all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+flow_min+NH4_inf_log+ShortWave_mean+Turb_NTU_log,
                data = dataall, family = gaussian, na.action = "na.fail")
pred.1.all <- predict(mod1.all, newdata=dataall)

mod2.all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+flow_min + ShortWave_mean+Turb_NTU_log, 
                data = dataall, family = gaussian, na.action = "na.fail")
pred.2.all <- predict(mod2.all, newdata=dataall)

mod3.all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+flow_min+RelHum_max_log+Turb_NTU_log+ShortWave_mean, data = dataall, family = gaussian, na.action = "na.fail" )
pred.3.all <- predict(mod3.all, newdata=dataall)

mod4.all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+Turb_NTU_log+flow_min+Temp_inf_min+ShortWave_mean, data = dataall, family = gaussian, na.action = "na.fail" )
pred.4.all <- predict(mod4.all, newdata=dataall)

mod5.all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+Turb_NTU_log+flow_min+Rain_sum_log+ShortWave_mean, data = dataall, family = gaussian, na.action = "na.fail" )
pred.5.all <- predict(mod5.all, newdata=dataall)

mod6.all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+Turb_NTU_log+flow_min+RelHum_max_log, data = dataall, family = gaussian, na.action = "na.fail" )
pred.6.all <- predict(mod6.all, newdata=dataall)

#make model with water temp instead of temp inf min
mod7.all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+Turb_NTU_log+flow_min+Temp_C+ShortWave_mean, data = dataall, family = gaussian, na.action = "na.fail" )
pred.7.all <- predict(mod7.all, newdata=dataall)
round((rsq(mod7.all, type = 'sse')), digits = 3)

plot(mod7.all)
plot(mod6.all)

plot(dataall$Chla_sqrt, type = 'l', col = 'black', ylab = "Sqrt Chla")
points(pred.1.all, type = 'l', col = 'red')
points(pred.2.all, type = 'l', col = 'orange')
points(pred.3.all, type = 'l', col = 'yellow')
points(pred.4.all, type = 'l', col = 'green')
points(pred.5.all, type = 'l', col = 'blue')
points(pred.6.all, type = 'l', col = 'purple')
points(pred.7.all, type = 'l', col = 'brown')
title("Models 2013-2016")
legend('bottomright', c('obs', 'mod1', 'mod2', 'mod3', 'mod4', 'mod5'), cex = 0.4,lty = c(1,1),bty='n',col = c("black", 'red', 'orange', 'yellow', 'green', 'blue', 'purple'))

par(mfrow = c(3,2))
plot(pred.1.all, dataall$Chla_sqrt, xlim = c(0,2.5), ylim = c(0,3))
abline(0,1)
title("Mod1 Obs vs. Pred")

plot(pred.2.all, dataall$Chla_sqrt, xlim= c(0, 2.5), ylim = c(0,3))
abline(0,1)
title("Mod2 Obs vs. Pred")

plot(pred.3.all, dataall$Chla_sqrt, xlim= c(0, 2.5), ylim = c(0,3))
abline(0,1)
title("Mod3 Obs vs. Pred")

plot(pred.4.all, dataall$Chla_sqrt, xlim= c(0, 2.5), ylim = c(0,3))
abline(0,1)
title("Mod4 Obs vs. Pred")

plot(pred.5.all, dataall$Chla_sqrt, xlim= c(0, 2.5), ylim = c(0,3))
abline(0,1)
title("Mod5 Obs vs. Pred")

plot(pred.6.all, dataall$Chla_sqrt, xlim = c(0, 2.5), ylim = c(0,3))
abline(0,1)
# diagnostics tables in r markdown file, Meeting_11282018.Rmd


########################################################################################################################################################
################################# 2013  ########################################################################################################33

var13 <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/2013_selected_correlations.csv")
var13$Date <- NA
#include water temp also to use instead of temp_inf_max
var13$Temp_C <- NA
var13 <- var13%>%select(Date, everything())
data13 <- data[,colnames(var13)]
data13$Date <- as.Date(data13$Date)
# subset to get rid of NA's at beginning?
data13all <- data13[data13$Date>"2013-06-20",]
write.csv(data13all, "./historical_model_selection/ARIMA_data/selected model files/2013_selected_variables_allyears.csv", row.names = FALSE)
#subset to just 2013
data13 <- data13[data13$Date>"2013-06-20" & data13$Date<"2014-01-01",]
write.csv(data13, "./historical_model_selection/ARIMA_data/selected model files/2013_selected_variables_2013only.csv", row.names = FALSE)

# build a global model for 2013
model13 <- glm(Chla_sqrt~Chla_ARlag1_sqrt +Turb_NTU_log+NO3NO2_log+SRP_log+mean_flow+Temp_inf_max+Rain_sum_log+WindSpeed_mean_log
               +ShortWave_mean, data = data13, family = gaussian, na.action = "na.fail" )
glm13 <- dredge(model13, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select13 <- subset(glm13, delta<2 )

# models <2 units different AICc values have been selected
# now build those models individually and run summary statistics
mod1 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Turb_NTU_log, data = data13, family = gaussian, na.action = na.fail)
pred.1 <- predict(mod1, newdata=data13)
mod2 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+NO3NO2_log+ShortWave_mean+Turb_NTU_log, data = data13, 
            family = gaussian, na.action = "na.fail")
pred.2 <- predict(mod2, newdata=data13)
mod3 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+Rain_sum_log+ShortWave_mean+Turb_NTU_log,data = data13, family = gaussian, na.action = na.fail)
pred.3 <- predict(mod3, newdata=data13)
mod4 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Temp_inf_max+Turb_NTU_log, data = data13, family = gaussian, na.action = "na.fail")
pred.4 <- predict(mod4, newdata=data13)
mod5 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+NO3NO2_log+Rain_sum_log+ShortWave_mean+Turb_NTU_log, data = data13, family = gaussian, na.action = "na.fail")
pred.5 <- predict(mod5, newdata=data13)
# replace inflow temp with water temp
mod6 <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Temp_C+Turb_NTU_log, data = data13, family = gaussian, na.action = "na.fail")
pred.6 <- predict(mod6, newdata=data13)
round((rsq(mod6, type = 'sse')), digits = 3)
plot(pred.6)
plot(pred.5)

# plot predicted and observed
plot(data13$Chla_sqrt, type = 'l', col = "black", ylab = "Sqrt Chla", ylim = c(0,3.3))
points(pred.1, type = 'l', col = "red")
points(pred.2, type = 'l', col = 'orange')
points(pred.3, type = 'l', col = 'pink')
points(pred.4, type = 'l', col = 'yellow')
points(pred.5, type = 'l', col = 'green')
points(pred.6, type = 'l', col = 'blue')
legend('bottomright', c('obs', 'mod1', 'mod2', 'mod3', 'mod4', 'mod5'), cex = 0.8,lty = c(1,1),bty='n',col = c("black", 'red', 'orange','pink', 'yellow', 'green'))
title("2013 models")

par(mfrow = c(3,2))
plot(pred.1, data13$Chla_sqrt, xlim = c(0,2.5), ylim = c(0,3))
abline(0,1)
title("Mod1 Obs vs. Pred")

plot(pred.2, data13$Chla_sqrt, xlim= c(0, 2.5), ylim = c(0,3))
abline(0,1)
title("Mod2 Obs vs. Pred")

plot(pred.3, data13$Chla_sqrt, xlim= c(0, 2.5), ylim = c(0,3))
abline(0,1)
title("Mod3 Obs vs. Pred")

plot(pred.4, data13$Chla_sqrt, xlim= c(0, 2.5), ylim = c(0,3))
abline(0,1)
title("Mod4 Obs vs. Pred")

plot(pred.5, data13$Chla_sqrt, xlim= c(0, 2.5), ylim = c(0,3))
abline(0,1)
title("Mod5 Obs vs. Pred")

plot(pred.6, data13$Chla_sqrt, xlim = c(0, 2.5), ylim = c(0,3))
abline(0,1)
### diagnostic table in R Markdown file, Meeting_11282018

####################################################################################################################################################
# 2013 model on 2013-2016 dataset

var13 <- read.csv("./historical_model_selection/ARIMA_data/correlation matrices/2013_selected_correlations.csv")
var13$Date <- NA
var13 <- var13%>%select(Date, everything())
data13.all <- data[,colnames(var13)]
data13.all$Date <- as.Date(data13.all$Date)
# subset to get rid of NA's at beginning?
data13.all <- data13.all[data13.all$Date>"2013-06-20",]

model13.all <- glm(Chla_sqrt~Chla_ARlag1_sqrt +Turb_NTU_log+NO3NO2_log+SRP_log+mean_flow+Temp_inf_max+Rain_sum_log+WindSpeed_mean_log
               +ShortWave_mean, data = data13.all, family = gaussian, na.action = "na.fail" )
glm13.all <- dredge(model13.all, rank = "AICc", fixed = "Chla_ARlag1_sqrt")
select13.all <- subset(glm13.all, delta<2 )


# models <2 units different AICc values have been selected
# now build those models individually and run summary statistics
mod1.13all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Turb_NTU_log, data = data13.all, family = gaussian, na.action = na.fail)
pred.1.13all <- predict(mod1.13all, newdata=data13.all)
mod2.13all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+NO3NO2_log+ShortWave_mean+Turb_NTU_log, data = data13.all, 
            family = gaussian, na.action = "na.fail")
pred.2.13all <- predict(mod2.13all, newdata=data13.all)
mod3.13all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+Rain_sum_log+ShortWave_mean+Turb_NTU_log,data = data13.all, family = gaussian, na.action = na.fail)
pred.3.13all <- predict(mod3.13all, newdata=data13.all)
mod4.13all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+ShortWave_mean+Temp_inf_max+Turb_NTU_log, data = data13.all, family = gaussian, na.action = "na.fail")
pred.4.13all <- predict(mod4.13all, newdata=data13.all)
mod5.13all <- glm(Chla_sqrt~Chla_ARlag1_sqrt+mean_flow+NO3NO2_log+Rain_sum_log+ShortWave_mean+Turb_NTU_log, data = data13.all, family = gaussian, na.action = "na.fail")
pred.5.13all <- predict(mod5.13all, newdata=data13.all)

# plot predicted and observed
plot(data13.all$Chla_sqrt, type = 'l', col = "black", ylab = "Sqrt Chla", ylim = c(0,3.3))
points(pred.1.13all, type = 'l', col = "red")
points(pred.2.13all, type = 'l', col = 'orange')
points(pred.3.13all, type = 'l', col = 'pink')
points(pred.4.13all, type = 'l', col = 'yellow')
points(pred.5.13all, type = 'l', col = 'green')
legend('bottomright', c('obs', 'mod1', 'mod2', 'mod3', 'mod4', 'mod5'), cex = 0.8,lty = c(1,1),bty='n',col = c("black", 'red', 'orange','pink', 'yellow', 'green'))
title("2013 models, 2013-2016 dataset")

