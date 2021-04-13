# plot GLM modeled vs obs
library(tidyverse)
library(Metrics)
library(MuMIn)


# first bring in the observed data fit to the right size
# now bring in arima output to calculate rmse and r2 in the same way
data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)


# read in correlation matrix for selected predictable variable names
varall <- read.csv("./historical_model_selection/ARIMA_Data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_2016_selected_predictable.csv")
# insert an empty date column so that it can be matched with the dataset
varall$Date <- NA
varall <- varall%>%select(Date, everything())
# subset dataset by the column names in the correlation matrix
dataall <- data[,colnames(varall)]
dataall$Date <- as.Date(dataall$Date)
dataall <- dataall[dataall$Date>"2013-05-09",]
# because rmse retains native units, create a chl column that is not square rooted
dataall <- dataall %>% mutate(Chla_ugL = (Chla_sqrt)^2)

###################################################################################################################
########## dataall$Chla_ugL is the observed data to compare to model outputs #######################################3
######################################################################################################################

# now build selected arima model
mod1_1316 <- glm(Chla_sqrt~Chla_ARlag1_sqrt + mean_flow +ShortWave_mean, 
                 data = dataall, family = gaussian, na.action = 'na.fail')
# make predictions
pred1_1316 <- predict(mod1_1316, newdata = dataall)

#############################################################################################################
# now bring in the glm model output

glm <- read.csv("./historical_model_selection/ARIMA_data/GLM_output/Chla1mForWhitney_28June2019.csv")
glm$Date <- as.Date(glm$DateTime)
plot(srf_glm$DateTime, srf_glm$Observed_PHY_TCHLA, type = 'l', lwd = 2)
points(srf_glm$DateTime, srf_glm$Modeled_PHY_TCHLA, lwd = 2, col = 'darkcyan', type = 'l')
legend('topleft', c('Observed', 'GLM Modeled'), lty = c(1,1), col = c('black', 'darkcyan'), bty = 'n')

# need the glm model output to be the same size as dataall
glm_chl <- left_join(dataall, srf_glm)
glm_chl <- na.omit(glm_chl)

rmse(dataall$Chla_ugL, (pred1_1316)^2)
r2 <- function (x, y) cor(x, y) ^ 2
r2(dataall$Chla_ugL, (pred1_1316)^2)
plot(dataall$Chla_ugL, (pred1_1316)^2)
plot(dataall$Date, dataall$Chla_ugL)

rmse(glm_chl$Chla_ugL, glm_chl$Modeled_PHY_TCHLA)
r2(glm_chl$Chla_ugL, glm_chl$Modeled_PHY_TCHLA)

