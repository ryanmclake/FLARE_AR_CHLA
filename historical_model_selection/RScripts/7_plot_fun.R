## look at FCR data to explore drivers of chla
library(tidyverse)

data <- read.csv("./historical_model_selection/ARIMA_data/Misc data files/FCR_Master_2013_2017.csv")

#remove the flag columns for ease of graphing
data <- data%>% select(-X)
data$Date <- as.Date(data$Date)


surfdata <- data[data$Depth<1 & data$Site==50,]
ggplot(surfdata, aes(x = Date, y = Chla_ugL)) + geom_point()
ggplot(surfdata, aes(x = Temp_C, y = Chla_ugL)) + geom_point()
ggplot(surfdata, aes(x = DO_mgL, y = Chla_ugL)) + geom_point()
ggplot(surfdata, aes(x = SpCond_calc, y = Chla_ugL)) + geom_point() + xlim(c(25,40)) +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = Turb_NTU, y = Chla_ugL)) + geom_point() + xlim(c(0,26))


####### met data and surface chla
ggplot(surfdata, aes(x = AirTemp_max, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
#these are really similar to airtemp max ggplot(surfdata, aes(x = AirTemp_mean, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
#these are really similar to airtemp max  ggplot(surfdata, aes(x = AirTemp_median, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = Rain_sum, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = RelHum_mean, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = RelHum_max, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = RelHum_median, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = ShortWave_max, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = ShortWave_mean, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = WindSpeed_max, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = WindSpeed_mean, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = WindSpeed_median, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)


######## fluoro data and chla
ggplot(surfdata, aes(x = Total_chlorophyll, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = Green_algae, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = Cyanobacteria, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
plot(surfdata$Total_chlorophyll, y = surfdata$Chla_ugL)
points(surfdata$Cyanobacteria, y = surfdata$Chla_ugL, col = "blue")
points(surfdata$Green_algae, y = surfdata$Chla_ugL, col = "green")



######## chemistry data and chla
ggplot(surfdata, aes(x = TP_inf, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x) + ylim(c(0, 10))
ggplot(surfdata, aes(x = TN_inf, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)

ggplot(surfdata, aes(x = TP_ugL, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = TN_ugL, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)

ggplot(surfdata, aes(x = TP_inf, y = TP_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x) 
ggplot(surfdata, aes(x = TN_inf, y = TN_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)


ggplot(surfdata, aes(x = NH4_inf, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = NH4_ugL, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)

ggplot(surfdata, aes(x = NO3NO2_ugL, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = NO3NO2_inf, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)

ggplot(surfdata, aes(x = SRP_inf, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = SRP_ugL, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)

ggplot(surfdata, aes(x = DOC_mgL, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x) + xlim (c(0,8))
ggplot(surfdata, aes(x = DOC_inf, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x) #+ xlim(c(0))
ggplot(surfdata) + geom_point(aes(x = DOC_inf, y = Chla_ugL), col = "coral4")  +
  geom_point(aes(x = DOC_mgL, y = Chla_ugL), col = "blue") + xlim(c(0,10))

ggplot(surfdata, aes(x = TN_TP_inf, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = TN_TP, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)

ggplot(surfdata, aes(x = NH4NO3_SRP_inf, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)
ggplot(surfdata, aes(x = NH4NO3_SRP, y = Chla_ugL)) + geom_point() +  geom_smooth(method='lm',formula=y~x)


ggplot(data, aes(x = Date, y = Kd)) + geom_point() 



## subset 2013 data only
data_13 <- data[data$Date < "2014-01-01" & data$Depth < 0.5,]
ggplot(data_13, aes(x = Date, y = Chla_ugL)) + geom_point()

## subset 2014 data only
data_14 <- data[data$Date < "2015-01-01" & data$Depth < 0.5,]
ggplot(data_14, aes(x = Date, y = Chla_ugL)) + geom_point()

## subset 2015 data only
data_15 <- data[data$Date < "2016-01-01" & data$Date > "2014-12-31" & data$Depth < 0.5,]
ggplot(data_15, aes(x = Date, y = Chla_ugL)) + geom_point()

## subset 2016 data only
data_16 <- data[data$Date < "2017-01-01" & data$Date > "2015-12-31" & data$Depth < 0.5,]
ggplot(data_16, aes(x = Date, y = Chla_ugL)) + geom_point()



#put into long form
data_long <- data %>% gather(Metric, Value, Depth:Kd) 


ggplot(data_long)

ggplot(surfdata, aes(x = Total_chlorophyll, y = Chla_ugL)) + geom_point(aes(x = Green_algae, y = Chla_ugL, col = "green")) + 
  geom_point(aes(x = Cyanobacteria, y = Chla_ugL, col = "blue"))
#+  geom_smooth(method='lm',formula=y~x)

