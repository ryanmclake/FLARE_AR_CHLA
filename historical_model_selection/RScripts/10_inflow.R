# script to calculate residence time and nutrient loads for FCR based on inflow data from inflow weir
library(tidyverse)

inf <- read.csv("./historical_model_selection/Data/Inflow/inflow_interpolation_2013_2017.csv")

##############################################################################################################################################
 ##### # calculate residence time: volume in m3? / (flow in m3/s * 60s *60min * 24 hours ), unit: # of days
wrt <- inf %>%
  mutate(wtr_res_time = 3.1E5/(Flow_cms*60*60*24))
plot(wrt$wtr_res_time)
#


hist_wrt <- ggplot(data = wrt, aes(x = wtr_res_time))+
  geom_histogram(aes(y=..density..))+
  geom_density(alpha = 0.2, fill = "blue")+
  theme_bw()+ xlim(0, 2000)
hist_wrt

#calculate mean wrt/day
mean_wrt <- wrt %>%
  mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(mean_wrt = mean(wtr_res_time))

###########################################################################################################################################
############## calculate mean flow/day in order to calculate mean daily nutrient loads
mean_flow <- wrt %>%
  mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(mean_flow = mean(Flow_cms))

########## calculate other summary statistics about inflow data

#calculate mean daily temperature at inflow
#calculate mean wrt/day
temp_inf <- wrt %>%
  mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(Temp_inf_mean = mean(Temp_C), Temp_inf_max = max(Temp_C), Temp_inf_min = min(Temp_C))

# calculate min, max, and median for flow for each day
flow <- inf %>%
  mutate(Date = as.Date(DateTime, format="%Y-%m-%d")) %>%
  group_by(Date) %>%
  summarise(flow_max = max(Flow_cms), flow_min = min(Flow_cms), flow_median = median(Flow_cms))

# put all inflow data into one dataframe and write to a csv
infdata <- left_join(mean_wrt, mean_flow, by = "Date")
infdata <- left_join(infdata, temp_inf, by = "Date")
infdata <- left_join(infdata, flow, by = "Date")
write.csv(infdata, "./historical_model_selection/Data/Inflow/inflowcalcs_FCR.csv", row.names = FALSE)

# read in dataset which includes interpolated nutrient chemistry at inflow 
# to calculate nutrient loads
chem <- read.csv("./historical_model_selection/ARIMA_data/data_interpolated_MayOct13_16.csv")
chem$Date <- as.Date(chem$Date)

# add the residence times to the same dataframe as the nutrient data
calc <- left_join(chem, mean_flow)
calc$Date <- as.Date((calc$Date))

# nutrient loads = inflow * nutrient concentration at inflow
load <- calc %>%
  group_by(Date) %>%
  mutate(TN_load = TN_inf*mean_flow)%>%
  mutate(TP_load = TP_inf*mean_flow)%>%
  mutate(NH4_load = NH4_inf*mean_flow) %>%
  mutate(NO3NO2_load = NO3NO2_inf*mean_flow) %>%
  mutate(SRP_load = SRP_inf*mean_flow) %>%
  mutate(DOC_load = DOC_inf*mean_flow)

#subset just loads for some quick plots
just_load <- load %>%
  select(Date, mean_flow:DOC_load)

par(mfrow = c(1,1))
plot(just_load$Date, just_load$mean_flow)
plot(mean_wrt$Date, mean_wrt$mean_wrt)

par(mfrow=c(2,3))
plot(just_load$Date, just_load$TN_load)
plot(just_load$Date, just_load$TP_load)
plot(just_load$Date, just_load$NH4_load)
plot(just_load$Date, just_load$NO3NO2_load)
plot(just_load$Date, just_load$SRP_load)
plot(just_load$Date, just_load$DOC_load)

# merge together mean residence team, mean daily nutrient load calculations
data <- left_join(load, mean_wrt)

###############################################################################################################################################


data2 <- left_join(data, temp_inf)
data3 <- left_join(data2, flow)
write.csv(data3, "./historical_model_selection/ARIMA_data/data_interpolated_plusinflowcalcs_MayOct13_16.csv", row.names= FALSE)
