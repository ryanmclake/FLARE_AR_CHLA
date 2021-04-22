# script to calculate correlation coefficients between variables in the model and to get a first look at 
# what data transformations are necessary

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("Hmisc")
library(Hmisc)
library(tidyverse)

data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")
# get rid of desciptor columns (depth, week identifiers, etc.), so only possible drivers are left
data <- data %>%
  select(-Depth,-Date, -(week_julian:week_cum))
##############################################################################################################################################
####################  create correlation matrices for each year #########################################################################
# not using chart.Correlation because there are too many variables to assess in a table like this
#x <- chart.Correlation(data, method = "spearman", histogram = TRUE)

cor <- rcorr(as.matrix(data), type = "spearman")
spear <- cor$r
write.csv(spear, "./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013_2016.csv", row.names = FALSE)



# subset by year to create correlation matrices for each year

# 2013
dataall <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")
dataall$Date <- as.Date(dataall$Date)
data13 <- dataall[dataall$Date<"2013-12-31",]
data13 <- data13 %>% select(-Depth,-Date, -(week_julian:week_cum))

cor_13 <- rcorr(as.matrix(data13), type = "spearman")
spear_13 <- cor_13$r
write.csv(spear_13, "./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2013.csv", row.names = FALSE)

# 2014
data14 <- dataall[dataall$Date<"2014-12-31" & dataall$Date>"2013-12-31",]
data14 <- data14 %>% select(-Depth,-Date, -(week_julian:week_cum))

cor_14 <- rcorr(as.matrix(data14), type = "spearman")
spear_14 <- cor_14$r
write.csv(spear_14, "./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2014.csv", row.names = FALSE)


# 2015
data15 <- dataall[dataall$Date<"2015-12-31" & dataall$Date>"2014-12-31",]
data15 <- data15 %>% select( -Depth,-Date, -(week_julian:week_cum))

cor_15 <- rcorr(as.matrix(data15), type = "spearman")
spear_15 <- cor_15$r
write.csv(spear_15, "./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2015.csv", row.names = FALSE)

# 2016
data16 <- dataall[dataall$Date<"2016-12-31" & dataall$Date>"2015-12-31",]
data16 <- data16 %>% select(-Depth,-Date, -(week_julian:week_cum))

cor_16 <- rcorr(as.matrix(data16), type = "spearman")
spear_16 <- cor_16$r
write.csv(spear_16, "./historical_model_selection/ARIMA_data/correlation matrices/chlasqrt_1.0m/correlation_matrix_chlasqrt_2016.csv", row.names = FALSE)




################################################################################################################################################
# eliminate variables that are correlated through 1) visually seeing which has a stronger relationship with chl, 
# if no clear relationship:
# 2) choosing the variable with the higher spearman's r value with chl
# if no clear difference between the r values:
# 3) choosing the variable that would have more of a biological importance on chl (e.g., water temp should be more important
# than air temp)
# take a look at some of the variables that are correlated vs. chl

#####################################################################################################################################################
############ 2013 dataset selection #########################################
# create a function for ease in comparing variables
# before using function, attach the dataset
attach(data13)

compare <- function(variab){
  x <- lm(Chla_sqrt~variab)
  plot(variab, Chla_sqrt)
  abline(x)
  summary(x)
}


compare(data13$Turb_NTU_log)
compare(NH4_inf_log)
compare(TP_inf_log)
compare(mean_flow)
compare(SRP_load)
compare(TP_load_log)

compare(flow_max_log)
compare(flow_min)
compare(flow_median)
compare(WindSpeed_median_log)
compare(NO3NO2_load)
compare(mean_wrt_log)
compare(SRP_inf)
compare(TP_inf_log)
compare(TP_log)
compare(NH4_load_log)

compare(ShortWave_max)
compare(RelHum_median)
compare(AirTemp_max_log)
compare(AirTemp_mean_log)
compare(AirTemp_median_log)
compare(NH4NO3_SRP_inf)
compare(NO3NO2_inf)
compare(NH4_log)
compare(SpCond_uScm)
compare(ShortWave_mean)

compare(Temp_inf_mean)
compare(Temp_C)
compare(Temp_inf_min)
compare(Temp_inf_max)

compare(RelHum_max_log)
compare(RelHum_median)

compare(NO3NO2_log)
compare(DO_mgL)

compare(WindSpeed_max_log)
compare(WindSpeed_mean_log)

plot(data13$NH4_inf_log, data13$Chla_sqrt)


plot(data13$Temp_C, data13$Chla_sqrt)
plot(data13$DO_mgL, data13$Chla_sqrt)

plot(data13$NH4_inf_log, data13$Chla_sqrt)
abline(lm(data13$Chla_sqrt~data13$NH4_inf_log))

plot(data13$Turb_NTU_log, data13$Chla_sqrt)
abline(lm(data13$Chla_sqrt~data13$Turb_NTU_log))

plot(SRP_log, Chla_sqrt)
abline(lm(Chla_sqrt~SRP_log))
x <- lm(Chla_sqrt~SRP_log)
summary(x)

plot(flow_max_log, Chla_sqrt)
abline(lm(Chla_sqrt~flow_max_log))

plot(mean_wrt_log, Chla_sqrt)
abline(lm(Chla_sqrt~mean_wrt_log))
wrt <- lm(Chla_sqrt~mean_wrt_log)
summary(wrt)

plot(NH4_load_log, Chla_sqrt)
abline(lm(Chla_sqrt~NH4_load_log))
ammonload <- lm(Chla_sqrt~NH4_load_log)
summary(ammonload)

plot(TP_load_log, Chla_sqrt)
abline(lm(Chla_sqrt~TP_load_log))
tpload <- lm(Chla_sqrt~TP_load_log)
summary(tpload)

plot(NH4NO3_SRP_inf, Chla_sqrt)
abline(lm(Chla_sqrt~NH4NO3_SRP_inf))
nitsrpinf <- lm(Chla_sqrt~NH4NO3_SRP_inf)
summary(nitsrpinf)

plot(SRP_inf, Chla_sqrt)
abline(lm(Chla_sqrt~SRP_inf))
srpinf <- lm(Chla_sqrt~SRP_inf)
summary(srpinf)

plot(TP_inf_log, Chla_sqrt)
abline(lm(Chla_sqrt~TP_inf_log))
y <- lm(Chla_sqrt~TP_inf_log)
summary(y)

plot(NH4_log, Chla_sqrt)
abline(lm(Chla_sqrt~NH4_log))

plot(data13$Temp_C, data13$Chla_sqrt)
abline(lm(Chla_sqrt~Temp_C))

plot(NO3NO2_inf, Chla_sqrt)
abline(lm(Chla_sqrt~NO3NO2_inf))

plot(WindSpeed_max_log, Chla_sqrt)
abline(lm(Chla_sqrt~WindSpeed_max_log))
wsmax <- lm(Chla_sqrt~WindSpeed_max_log)
summary(wsmax)

plot(WindSpeed_mean_log, Chla_sqrt)
abline(lm(Chla_sqrt~WindSpeed_mean_log))
wsmean <- lm(Chla_sqrt~WindSpeed_mean_log)
summary(wsmean)

plot(DO_mgL, Chla_sqrt)
abline(lm(Chla_sqrt~DO_mgL))

plot(WindSpeed_median_log, Chla_sqrt)
abline(lm(Chla_sqrt~WindSpeed_median_log))
wsmed <- lm(Chla_sqrt~WindSpeed_median_log)
summary(wsmed)

plot(Temp_inf_max, Chla_sqrt)
abline(lm(Chla_sqrt~Temp_inf_max))
infmax <- lm(Chla_sqrt~Temp_inf_max)
summary(infmax)

plot(AirTemp_max_log, Chla_sqrt)
abline(lm(Chla_sqrt~AirTemp_max_log))

plot(Temp_inf_mean, Chla_sqrt)
abline(lm(Chla_sqrt~Temp_inf_mean))
infmean <- lm(Chla_sqrt~Temp_inf_mean)
summary(infmean)


#shortwave max or mean
plot(ShortWave_max, Chla_sqrt)
abline(lm(Chla_sqrt~ShortWave_max))
swmax <- lm(Chla_sqrt~ShortWave_max)
summary(swmax)

plot(ShortWave_mean, Chla_sqrt)
abline(lm(Chla_sqrt~ShortWave_mean))
swmean <- lm(Chla_sqrt~ShortWave_mean)
summary(swmean)

#shortwave mean, sp cond, or NO3NO2 load
plot(SpCond_uScm, Chla_sqrt)
abline(lm(Chla_sqrt~SpCond_uScm))
spcond <- lm(Chla_sqrt~SpCond_uScm)
summary(spcond)

plot(NO3NO2_load, Chla_sqrt)
abline(lm(Chla_sqrt~NO3NO2_load))

#relative humidity max, mean, or median
plot(RelHum_max_log, Chla_sqrt)
abline(lm(Chla_sqrt~RelHum_max_log))
hummax <- lm(Chla_sqrt~RelHum_max_log)
summary(hummax)

plot(RelHum_mean, Chla_sqrt)
abline(lm(Chla_sqrt~RelHum_mean))
hummean <- lm(Chla_sqrt~RelHum_mean)
summary(hummean)

# relhum mean vs. rainsum
plot(RelHum_mean, Chla_sqrt)
abline(lm(Chla_sqrt~RelHum_mean))
hummean <- lm(Chla_sqrt~RelHum_mean)
summary(hummean)

plot(Rain_sum_log, Chla_sqrt)
abline(lm(Chla_sqrt~Rain_sum_log))
rainsum <- lm(Chla_sqrt~Rain_sum_log)
summary(rainsum)

#srp load vs. no3no2 vs. mean flow vs. nh4no3no2_srp
plot(log(SRP_load), Chla_sqrt)
abline(lm(Chla_sqrt~log(SRP_load)))
srp <- lm(Chla_sqrt~SRP_load)
summary(srp)

plot(mean_flow, Chla_sqrt)
abline(lm(Chla_sqrt~mean_flow))
meanflow <- lm(Chla_sqrt~mean_flow)
summary(meanflow)

plot(NO3NO2_log, Chla_sqrt)
abline(lm(Chla_sqrt~NO3NO2_log))
nitrite <- lm(Chla_sqrt~NO3NO2_log)
summary(nitrite)


plot(NH4NO3NO2_SRP_log, Chla_sqrt)
abline(lm(Chla_sqrt~NH4NO3NO2_SRP_log))
nitrite_srp <- lm(Chla_sqrt~NH4NO3NO2_SRP_log)
summary(nitrite_srp)

#NO3NO2_log and TP_log
plot(NO3NO2_log, Chla_sqrt)
abline(lm(Chla_sqrt~NO3NO2_log))
nitrite <- lm(Chla_sqrt~NO3NO2_log)
summary(nitrite)

plot(TP_log, Chla_sqrt)
abline(lm(Chla_sqrt~TP_log))
tp <- lm(Chla_sqrt~TP_log)
summary(tp)


#####################################################################################################################################################
############ 2014 dataset selection #########################################
# create a function for ease in comparing variables
# before using function, attach the dataset
attach(data14)

compare <- function(variab){
  x <- lm(Chla_sqrt~variab)
  plot(variab, Chla_sqrt)
  abline(x)
  summary(x)
}

compare(mean_flow)
compare(SpCond_uScm)

compare(Turb_NTU_log)
compare(Temp_C)
compare(NH4_inf_log)
compare(NO3NO2_load)
compare(NH4_load_log)

compare(flow_min)
compare(flow_median)
compare(flow_max_log)
compare(mean_wrt_log)
compare(DOC_load_log)
compare(NH4_load_log)
compare(ShortWave_mean)
compare(AirTemp_max_log)
compare(AirTemp_mean_log)
compare(AirTemp_median_log)
compare(NH4_log)
compare(mean_flow)
compare(Turb_NTU_log)
compare(SpCond_uScm)

compare(ShortWave_max)
compare(RelHum_median)
compare(RelHum_mean)
compare(RelHum_max_log)
compare(Rain_sum_log)

compare(WindSpeed_max_log)
compare(WindSpeed_mean_log)
compare(WindSpeed_median_log)

compare(Temp_inf_max)
compare(Temp_inf_mean)
compare(Temp_inf_min)
compare(SRP_load)
compare(NO3NO2_load)
compare(SRP_log)
compare(DO_mgL)
compare(Temp_C)

compare(TN_TP_log)
compare(TN_log)
compare(NH4NO3_SRP_inf)
compare(TP_log)
compare(NO3NO2_log)

compare(NH4_inf_log)
compare(NO3NO2_inf)

detach(data14)
#########################################################################################################################3
################ 2015 dataset #############################################################################################
attach(data15)

compare <- function(variab){
  x <- lm(Chla_sqrt~variab)
  plot(variab, Chla_sqrt)
  abline(x)
  summary(x)
}

compare(flow_max_log)
compare(DOC_load_log)
compare(NH4_load_log)
compare(NH4_inf_log)
compare(DOC_inf_log)
compare(mean_flow)
compare(NO3NO2_load)
compare(SRP_load)
compare(TN_load_log)
compare(TP_load_log)
compare(flow_min)
compare(flow_median)
compare(mean_wrt_log)


compare(RelHum_max_log)
compare(Rain_sum_log)
compare(RelHum_median)
compare(RelHum_mean)


compare(Temp_C)
compare(SRP_inf)
compare(TN_inf_log)
compare(TP_inf_log)
compare(NH4NO3_SRP_inf)

compare(Turb_NTU_log)
compare(NH4NO3_SRP_inf)

compare(ShortWave_max)
compare(AirTemp_max_log)
compare(ShortWave_mean)

compare(WindSpeed_max_log)
compare(WindSpeed_mean_log)
compare(WindSpeed_median_log)


compare(Temp_inf_min)
compare(Temp_inf_mean)
compare(Temp_inf_max)
compare(AirTemp_mean_log)
compare(AirTemp_median_log)


##############################################################################################################################
############### 2016 dataset ###################################################################################################

attach(data16)

# run the function above for easily running the metrics for comparing variables


compare(NH4_load_log)
compare(NH4_inf_log)
compare(mean_flow)
compare(NO3NO2_load)
compare(SRP_load)
compare(TN_load_log)
compare(TP_load_log)
compare(mean_wrt_log)
compare(DOC_load_log)
compare(flow_min)
compare(flow_max_log)
compare(flow_median)


compare(RelHum_max_log)
compare(DOC_inf_log)
compare(RelHum_median)
compare(RelHum_mean)
compare(ShortWave_mean)
compare(ShortWave_max)

compare(WindSpeed_max_log)
compare(WindSpeed_median_log)
compare(WindSpeed_mean_log)

compare(SRP_inf)
compare(Turb_NTU_log)
compare(Temp_C)
compare(NH4NO3_SRP_inf)
compare(AirTemp_max_log)
compare(AirTemp_mean_log)
compare(AirTemp_median_log)
compare(Temp_inf_max)
compare(Temp_inf_mean)
compare(Temp_inf_min)

compare(SpCond_uScm)
compare(DO_mgL)

compare(TN_inf_log)
compare(TP_inf_log)
detach(data16)

#########################################################################################################################################################
####################### 2013-2016 dataset #################################################

attach(data)
compare <- function(variab){
  x <- lm(Chla_sqrt~variab)
  plot(variab, Chla_sqrt)
  abline(x)
  summary(x)
}

compare(SpCond_uScm)
compare(TP_inf_log)
compare(SRP_inf)
compare(mean_flow)
compare(NO3NO2_load)
compare(SRP_load)
compare(NH4_load_log)
compare(DOC_load_log)
compare(mean_wrt_log)
compare(flow_max_log)
compare(flow_median)
compare(flow_min)

compare(ShortWave_mean)
compare(DOC_inf_log)
compare(RelHum_max_log)
compare(AirTemp_median_log)
compare(AirTemp_max_log)
compare(RelHum_mean)
compare(ShortWave_max)

compare(RelHum_median)
compare(Rain_sum_log)

compare(Temp_inf_max)
compare(NO3NO2_inf)
compare(NH4NO3_SRP_inf)
compare(Temp_inf_min)
compare(Temp_inf_mean)
compare(AirTemp_mean_log)
compare(Temp_C)

compare(WindSpeed_max_log)
compare(WindSpeed_mean_log)
compare(WindSpeed_median_log)
