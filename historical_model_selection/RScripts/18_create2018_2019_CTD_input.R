# prepare CTD data from 2019 and combine it with 2018 to go into chl-a empirical forecast
library(tidyverse)

ctd_2019 <- read_csv('./historical_model_selection/Data/CTD/CTD_notmatlab_ready_2019_fcr50.csv')


layer = data.frame(ctd_2019)

layer1 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 0.1)))
layer2 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.0)))
layer3 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 1.6)))
layer4 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 2.8)))
layer5 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 3.8)))
layer6 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.0)))
layer7 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.2)))
layer8 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.5)))
layer9 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 5.8)))
layer10 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.0)))
layer11 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 6.2)))
layer12 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 8.0)))
layer13 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.0)))
layer14 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth_m) - 9.3)))

## replace name of depth with the selected layer depths for ease of merging later
layer1$Depth_m <- 0.1 
layer2$Depth_m <- 1.0
layer3$Depth_m <- 1.6 
layer4$Depth_m <- 2.8 
layer5$Depth_m <- 3.8 
layer6$Depth_m <- 5.0 
layer7$Depth_m <- 5.2 
layer8$Depth_m <- 5.5 
layer9$Depth_m <- 5.8 
layer10$Depth_m <- 6.0 
layer11$Depth_m <- 6.2 
layer12$Depth_m <- 8.0 
layer13$Depth_m <- 9.0 
layer14$Depth_m <- 9.3 

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,
                 layer11,layer12,layer13,layer14)

#as.Date(df.final$Date)
fcr50_layers_binned <- df.final[order(df.final$Date, df.final$Depth_m), ]
# arrange(df.final, Date)

write.csv(fcr50_layers_binned, "./historical_model_selection/Data/CTD/FCR_CTD_2019_50_binned.csv", row.names=FALSE)
plot(layer3$Chla_ugL, layer2$Chla_ugL)


ctd_2018 <- read.csv('./historical_model_selection/Data/CTD/FCR_CTD_50_1m_2018.csv')
ctd_2018 <- ctd_2018 %>% select(Date, Chla_ugL)
ctd_2018$Date <- as.Date(ctd_2018$Date)

layer3 <- layer3 %>% select(Date, Chla_ugL)
layer3$Date <- as.Date(layer3$Date)
ctd <- dplyr::bind_rows(ctd_2018, layer3)
ctd <- ctd[!duplicated(ctd$Date),]
ctd <- ctd %>% mutate(chla_lag = lag(Chla_ugL, n = 1L))
# remove some of the dates that are too far away from each other
ctd <- ctd[ctd$Date>'2018-04-21',]
ctd <- ctd[!ctd$Date==as.Date('2018-12-06') & !ctd$Date==(as.Date('2018-12-17')) & !ctd$Date==as.Date('2019-02-08'),]

write.csv(ctd, './historical_model_selection/Data/CTD/FCR_CTD_2018_2019.csv', row.names = FALSE)
