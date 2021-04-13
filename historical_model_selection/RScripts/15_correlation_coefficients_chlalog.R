# script to calculate correlation coefficients between variables in the model and to get a first look at 
# what data transformations are necessary

#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
#install.packages("Hmisc")
library(Hmisc)
library(tidyverse)


data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlalog_2013_2016.csv")
# get rid of desciptor columns (depth, week identifiers, etc.), so only possible drivers are left
data <- data %>%
  select(-Depth,-Date, -(week_julian:week_cum))
##############################################################################################################################################
####################  create correlation matrices for each year #########################################################################
# not using chart.Correlation because there are too many variables to assess in a table like this
#x <- chart.Correlation(data, method = "spearman", histogram = TRUE)

cor <- rcorr(as.matrix(data), type = "spearman")
spear <- cor$r
write.csv(spear, "./historical_model_selection/ARIMA_data/correlation matrices/chlalog_1.0m/correlation_matrix_chlalog_2013_2016.csv", row.names = FALSE)



# subset by year to create correlation matrices for each year

# 2013
dataall <- read.csv("model_transformed_chlalog_2013_2016.csv")
dataall$Date <- as.Date(dataall$Date)
data13 <- dataall[dataall$Date<"2013-12-31",]
data13 <- data13 %>% select(-Depth,-Date, -(week_julian:week_cum))

cor_13 <- rcorr(as.matrix(data13), type = "spearman")
spear_13 <- cor_13$r
write.csv(spear_13, "./historical_model_selection/ARIMA_data/correlation matrices/chlalog_1.0m/correlation_matrix_chlalog_2013.csv", row.names = FALSE)

# 2014
data14 <- dataall[dataall$Date<"2014-12-31" & dataall$Date>"2013-12-31",]
data14 <- data14 %>% select(-Depth,-Date, -(week_julian:week_cum))

cor_14 <- rcorr(as.matrix(data14), type = "spearman")
spear_14 <- cor_14$r
write.csv(spear_14, "./historical_model_selection/ARIMA_data/correlation matrices/chlalog_1.0m/correlation_matrix_chlalog_2014.csv", row.names = FALSE)


# 2015
data15 <- dataall[dataall$Date<"2015-12-31" & dataall$Date>"2014-12-31",]
data15 <- data15 %>% select(-Depth,-Date, -(week_julian:week_cum))

cor_15 <- rcorr(as.matrix(data15), type = "spearman")
spear_15 <- cor_15$r
write.csv(spear_15, "./historical_model_selection/ARIMA_data/correlation matrices/chlalog_1.0m/correlation_matrix_chlalog_2015.csv", row.names = FALSE)

# 2016
data16 <- dataall[dataall$Date<"2016-12-31" & dataall$Date>"2015-12-31",]
data16 <- data16 %>% select( -Depth,-Date, -(week_julian:week_cum))

cor_16 <- rcorr(as.matrix(data16), type = "spearman")
spear_16 <- cor_16$r
write.csv(spear_16, "./historical_model_selection/ARIMA_data/correlation matrices/chlalog_1.0m/correlation_matrix_chlalog_2016.csv", row.names = FALSE)




################################################################################################################################################
# eliminate variables that are correlated through 1) visually seeing which has a stronger relationship with chl, 
# if no clear relationship:
# 2) choosing the variable with the higher spearman's r value with chl
# if no clear difference between the r values:
# 3) choosing the variable that would have more of a biological importance on chl (e.g., water temp should be more important
# than air temp)
# take a look at some of the variables that are correlated vs. chl

#####################################################################################################################################################
############ 2013 dataset #########################################
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


#########################################################################################################################################################
####################### 2013-2016 dataset #################################################

data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")
attach(data)

# create a function for ease in comparing variables
compare <- function(variab){
  x <- lm(Chla_sqrt~variab)
  plot(variab, Chla_sqrt)
  abline(x)
  summary(x)
}

# looking at predictable variables only
compare(mean_flow)
compare(flow_max_log)
compare(flow_median)
compare(flow_min)

compare(Temp_C)
compare(AirTemp_mean_log)

compare(WindSpeed_max_log)
compare(WindSpeed_mean_log)
compare(WindSpeed_median_log)

compare(AirTemp_max_log)
compare(ShortWave_max)
compare(ShortWave_mean)
compare(RelHum_max_log)
compare(RelHum_mean)
compare(AirTemp_median_log)

compare(RelHum_median)
compare(Rain_sum_log)

# TP_inf_log vs. TN_inf_log
plot(TP_inf_log, Chla_sqrt)
abline(lm(Chla_sqrt~TP_inf_log))
tpinf <- lm(Chla_sqrt~TP_inf_log)
summary(tpinf)
plot(TN_inf_log, Chla_sqrt)
abline(lm(Chla_sqrt~TN_inf_log))
tninf <- lm(Chla_sqrt~TN_inf_log)
summary(tninf)

#mean_wrt vs. sp cond
meanwrt <- lm(Chla_sqrt~mean_wrt_log)
plot(mean_wrt_log, Chla_sqrt)
abline(meanwrt)
summary(meanwrt)

spcond <- lm(Chla_sqrt~SpCond_uScm)
plot(SpCond_uScm[SpCond_uScm>10], Chla_sqrt[SpCond_uScm>10])
abline(spcond)
summary(spcond)

# TP_log or TN_TP_log
tplog <- lm(Chla_sqrt~TP_log)
plot(TP_log, Chla_sqrt)
abline(tplog)
summary(tplog)
tntplog <- lm(Chla_sqrt~TN_TP_log)
plot(TN_TP_log, Chla_sqrt)
abline(tntplog)
summary(tntplog)


# variables based on flow
compare(flow_min) #kept this one
compare(SRP_inf)
compare(mean_flow)
compare(NO3NO2_load)
compare(SRP_load)
compare(TN_load_log)
compare(TP_load_log)
compare(NH4_load_log)
compare(flow_median)
compare(flow_max_log)
compare(mean_wrt_log)
compare(DOC_load_log)
compare(SpCond_uScm)

# variables based on temperature
compare(AirTemp_max_log)
compare(Temp_inf_mean)
compare(AirTemp_mean_log)
compare(AirTemp_median_log)
compare(Temp_inf_max)
compare(Temp_inf_min)
compare(Temp_C)
compare(ShortWave_max)
compare(ShortWave_mean)
compare(NH4NO3_SRP_inf)

# humidity variables
compare(RelHum_max_log)
compare(RelHum_mean)
compare(RelHum_median)

# windspeed
compare(WindSpeed_max_log)
compare(WindSpeed_mean_log)
compare(WindSpeed_median_log)

# nutrient varibales
compare(DOC_log)
compare(TN_log)
compare(NH4NO3NO2_SRP_log)
compare(NH4_log)
compare(NO3NO2_log)
compare(SRP_log)
compare(DOC_inf_log)
compare(NO3NO2_inf)
compare(SRP_log)
compare(TN_TP_inf_log)
compare(DO_mgL)
