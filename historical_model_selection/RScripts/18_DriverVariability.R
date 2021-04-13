# looking at variability of drivers in FCR time series
data <- read.csv("./historical_model_selection/ARIMA_data/model_transformed_chlasqrt_2013_2016.csv")
data$Date <- as.Date(data$Date)


met <- read.csv("./historical_model_selection/Data/MET/Met_FCR_daily.csv")
met$Date <- as.Date(met$Date)

# weather variables
plot(data$Date, data$AirTemp_mean_log)
plot(met$Date, met$AirTemp_mean, type = 'l')
plot(met$Date, met$AirTemp_max, type = 'l')
plot(data$Date, exp(data$AirTemp_max_log), type = 'l')
plot(data$Date, exp(data$Rain_sum_log))
plot(met$Date, met$Rain_sum, type = 'l')
plot(data$Date, exp(data$WindSpeed_mean_log), type = 'l')
plot(data$Date, data$RelHum_mean)
plot(data$Date, data$ShortWave_mean)


# in-lake variables
plot(data$Date, data$Temp_C)
plot(data$Date, data$SpCond_uScm)
plot(data$Date, data$Turb_NTU_log)
plot(data$Date, exp(data$TP_log))
plot(data$Date, data$TN_log)
plot(data$Date, data$NH4_log)
plot(data$Date, data$NO3NO2_log)
plot(data$Date, data$SRP_log)
plot(data$Date, data$DOC_log)


#inflow
plot(data$Date, data$TP_inf_log)
plot(data$Date, data$TN_inf_log)
plot(data$Date, data$NH4_inf_log)
plot(data$Date, data$DOC_inf_log)
plot(data$Date, data$NO3NO2_inf)
plot(data$Date, data$SRP_inf)
plot(data$Date, data$mean_flow, type = 'l')
plot(data$Date, data$flow_median)
plot(data$Date, data$flow_max_log)
plot(data$Date, data$flow_min)
plot(data$Date, data$Temp_inf_mean)
