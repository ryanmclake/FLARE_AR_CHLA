# script to interpolate days where chla, our response variable, does not have data in order to reach a weekly timestep

# install.packages("zoo")
library(zoo)
library(tidyverse)

#master dataset of data collected by carey lab (i.e., no met data)
data <- read.csv("./historical_model_selection/ARIMA_data/FCR_VT_data_2013_2017.csv")
data$Date <- as.Date(data$Date)
data <- data %>% select(Date, everything())
#truncate the dataset to the end of the 2016 stratified period
#data <- data[data$Date < "2016-10-31" & data$Date >"2013-05-01",]

#	Create a new dataframe that consists of all the days that need to be interpolated that contains the same column headings
#  THESE ARE THE DATES I WANT TO INTERPOLATE (18 dates)
newdates <- as.Date(c( "2013-08-18", "2013-08-23", "2013-09-06", "2013-09-13", "2013-10-04", 
            "2013-10-11", "2014-08-30", "2014-10-10", "2014-10-30", "2015-03-01", "2015-08-15", "2015-08-22", "2015-10-12", 
            "2015-10-30", "2016-09-22", "2016-10-20", "2016-10-27", "2016-10-31"))
add <- data.frame(matrix(nrow = length(newdates) , ncol = ncol(data)))
colnames(add) <- colnames(data)
add[1] <- newdates

# add in every depth for each date
depths <- rep(c(0.1, 1.0, 1.6, 2.8, 3.8, 5.0, 5.2, 5.5, 5.8, 6.0, 6.2, 8.0, 9.0, 9.3), each = nrow(add))
add <- cbind(depths, add)
add$Depth <- depths
add <- add%>% select(-depths)

interp <- rbind(add, data)
#arrange by date so the NA's are not at the top of the dataframe
interp <- interp[order(interp$Date),]

# manually select 0.1 to check if interpolation works for chl
interp_0.1 <- interp[interp$Depth==0.1,]
interp_0.1$Chla_interp <- na.approx(interp_0.1$Chla_ugL,  na.rm=FALSE, rule = 2, maxgap = 15)
interp_0.1 <- mutate(interp_0.1, diff = Chla_interp - Chla_ugL)

# check temp at 3.8
interp_3.8 <- interp[interp$Depth==3.8,]
interp_3.8$Temp_C_interp <- na.approx(interp_3.8$Temp_C,  na.rm=FALSE, rule = 2, maxgap = 15)


## first interpolate the ctd variables 
ctd <- interp %>% 
  group_by(Depth) %>%
  mutate(Temp_interp = na.approx(Temp_C, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(DO_mgL_interp = na.approx(DO_mgL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(Chla_interp = na.approx(Chla_ugL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(Turb_NTU_interp = na.approx(Turb_NTU, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(SpCond_uScm_interp = na.approx(SpCond_calc, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  select(Date:Depth, Temp_interp:SpCond_uScm_interp) #keep only interpolated columns
write.csv(ctd, "./historical_model_selection/ARIMA_data/Interpolations/ctd_interp.csv", row.names = FALSE)


# interpolate TP, subset to 2013-04-04 to get rid of NA at beginning of dataset
TP <- interp[interp$Date>"2013-03-07" & interp$Date < "2017-12-20",]
TP <- data %>%
  group_by(Depth) %>%
  mutate(TP_interp = na.approx(TP_ugL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  select(Date:Depth, TP_interp) #keep only interpolated columns
write.csv(TP, "./historical_model_selection/ARIMA_data/Interpolations/TP_interp.csv", row.names = FALSE)

# interpolate TN, subset to dates where there is data
TN <- interp[interp$Date>"2013-12-31" & interp$Date < "2017-12-20",]
TN <- TN %>%
  group_by(Depth) %>%
  mutate(TN_interp = na.approx(TN_ugL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  select(Date:Depth, TN_interp) #keep only interpolated columns
write.csv(TN, "./historical_model_selection/ARIMA_data/Interpolations/TN_interp.csv", row.names = FALSE)


# interpolate TP_inf
TP_inf <- interp[interp$Date > "2013-05-15" & interp$Date < "2017-12-10",]
TP_inf <- TP_inf %>%
  group_by(Depth) %>%
  mutate(TPinf_interp = na.approx(TP_inf, na.rm = FALSE, rule = 2, maxgap = 15))%>%
  select(Date:Depth, TPinf_interp) #keep only interpolated columns
write.csv(TP_inf, "./historical_model_selection/ARIMA_data/Interpolations/TPinf_interp.csv", row.names = FALSE)


# interpolate TN_inf, subset first
TN_inf <- interp[interp$Date >"2015-03-01" & interp$Date < "2017-12-10", ]
TN_inf <- TN_inf %>%
  group_by(Depth) %>%
  mutate(TNinf_interp = na.approx(TN_inf, na.rm = FALSE, rule = 2, maxgap = 15))%>%
  select(Date:Depth, TNinf_interp) #keep only interpolated columns
write.csv(TN_inf, "./historical_model_selection/ARIMA_data/Interpolations/TNinf_interp.csv", row.names = FALSE)


# interpolate NH4, NO3NO2, and SRP, subset
nuts_other <- interp[interp$Date < "2017-12-20" & interp$Date > "2013-06-25",]
nuts_other <- nuts_other %>%
  group_by(Depth) %>%
  mutate(NH4_interp = na.approx(NH4_ugL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(NO3NO2_interp = na.approx(NO3NO2_ugL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(SRP_interp = na.approx(SRP_ugL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(NH4inf_interp = na.approx(NH4_inf, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(NO3NO2inf_interp = na.approx(NO3NO2_inf, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(SRPinf_interp = na.approx(SRP_inf, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  select(Date:Depth, NH4_interp, NO3NO2_interp, SRP_interp, NO3NO2inf_interp, NH4inf_interp, SRPinf_interp)
write.csv(nuts_other, "./historical_model_selection/ARIMA_data/Interpolations/nutsother_interp.csv", row.names = FALSE)

# interpolate DOC, subset first
DOC <- interp[interp$Date > "2013-12-10"  & interp$Date < "2017-12-10",]
DOC <- DOC %>%
  group_by(Depth) %>%
  mutate(DOC_interp = na.approx(DOC_mgL, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  mutate(DOCinf_interp = na.approx(DOC_inf, na.rm = FALSE, rule = 2, maxgap = 15))%>%
  select(Date:Depth, DOC_interp, DOCinf_interp)
write.csv(DOC, "./historical_model_selection/ARIMA_data/Interpolations/doc_interp.csv", row.names = FALSE)


#interpolate kd
kd <- interp[interp$Date > "2014-07-01" & interp$Date < "2017-12-10",]
kd <- kd %>%
  group_by(Date) %>%
  mutate(kd_interp = na.approx(Kd, na.rm = FALSE, rule = 2, maxgap = 15)) %>%
  select(Date:Depth, kd_interp)
write.csv(kd, "./Interpolations/kd_interp.csv", row.names = FALSE)
# put each of the subsetted interpolated dataframes back together
# did this in excel...couldn't figure out how to make R join the dataframes together properly
# now read in new csv file
interp_all <- read.csv("./historical_model_selection/ARIMA_data/Interpolations/collated_interpolated.csv")


# and calculate nutrient ratios (TN:TP and NH4+NO3NO2:SRP)
interp_all <- mutate(interp_all, TN_TP = TN_interp/TP_interp)
interp_all <- mutate(interp_all, NH4NO3NO2_SRP = (NH4_interp + NO3NO2_interp)/SRP_interp)
# also for inflow
interp_all <- mutate(interp_all, TN_TPinf = TNinf_interp/TPinf_interp)
interp_all <- mutate(interp_all, NH4NO3NO2_SRPinf = (NH4inf_interp + NO3NO2inf_interp)/SRPinf_interp)

neww <- interp_all
neww$Date <- as.Date(neww$Date)

# subset out each year for the May-Oct stratified period and then put them together to have a dataset that includes only
# the dates I want to model
data13 <- neww[neww$Date < "2013-11-01" & neww$Date >"2013-05-01",]
data14 <- neww[neww$Date < "2014-11-01" & neww$Date >"2014-05-01",]
data15 <- neww[neww$Date < "2015-11-01" & neww$Date >"2015-05-01",]
data16 <- neww[neww$Date < "2016-11-01" & neww$Date >"2016-05-01",]
data_all <- rbind(data13, data14, data15, data16)
# data_all is our dataset that includes all of the CTD variables interpolated within the MAY-OCT 2013-2016 timeframe

# change column names to get rid of 'interp'
colnames(data_all) <- c("Date" , "Depth", "Temp_C",  "DO_mgL", "Chla_ugL", "Turb_NTU",  "SpCond_uScm", "TP_ugL",
                        "TN_ugL", "NH4_ugL", "NO3NO2_ugL", "SRP_ugL",  "DOC_mgL", "TP_inf", "TN_inf", "NH4_inf",   "NO3NO2_inf",  
                        "DOC_inf", "SRP_inf","Kd", "TN_TP", "NH4NO3NO2_SRP", "TN_TP_inf" ,"NH4NO3_SRP_inf" )

write.csv(data_all, "./historical_model_selection/ARIMA_data/data_interpolated_MayOct13_16.csv", row.names = FALSE)


