#script to merge 2014-2017 fluoroprobe data and clean it up 
library(tidyverse)

#load in fluoro all data for 2014-2017
fluoro14 <- read.csv("./historical_model_selection/Data/Fluoroprobe/FCR2014_FluoroMeta.csv")
fluoro15 <- read.csv("./historical_model_selection/Data/Fluoroprobe/FCR2015_FluoroMeta.csv")
fluoro16 <- read.csv("./historical_model_selection/Data/Fluoroprobe/FCR2016_FluoroMeta50.csv")
fluoro17 <- read.delim("./historical_model_selection/Data/Fluoroprobe/FP_recal_2017.txt")  #fluoroprobe

#### clean up the 2014-2016 data
## remove unecessary columns
fluoro14 <- fluoro14 %>% select(-Year, -Reservoir)
fluoro15 <- fluoro15 %>% select(-Year, -Reservoir)
fluoro16 <- fluoro16 %>% select(-Year, -Reservoir)

# add in 'Site' column and rearrange
fluoro14$Site <-  rep(50,nrow(fluoro14))
fluoro14 <- fluoro14[,c("Date", "Site", "Depth", "Total_chlorophyll", "Green_algae", "Cyanobacteria", "Diatoms","Cryptophyta")]
fluoro15$Site <-  rep(50,nrow(fluoro15))
fluoro15 <- fluoro15[,c("Date", "Site","Depth", "Total_chlorophyll", "Green_algae","Cyanobacteria","Diatoms","Cryptophytes")]
colnames(fluoro15) <- colnames(fluoro14)
fluoro16$Site <-  rep(50,nrow(fluoro16))
fluoro16 <- fluoro16[,c("Date", "Site", "Depth", "Total_chlorophyll", "Green_algae", "Cyanobacteria", "Diatoms","Cryptophyta")]

#format date column to get rid of time
fluoro14$Date <- as.Date(fluoro14$Date, format = "%Y-%m-%d")
fluoro15$Date <- as.Date(fluoro15$Date, format = "%Y-%m-%d")
fluoro16$Date <- as.Date(fluoro16$Date, format = "%Y-%m-%d")


##clean up the 2017 data
#keep only FCR data
fluoro17 <- fluoro17[fluoro17$Reservoir=="FCR",]
#get rid of unnecessary columns
fluoro17 <- fluoro17 %>% select(-transmission_.,-(temp_sample_C:Reservoir), -yellow_sub_ugL)
fluoro17 <- select(fluoro17, datetime, Site, depth_m, total_conc_ugL, green_ugL, cyano_ugL, 
                   diatom_ugL, crypto_ugL)
#change column names to match 2014-2016 data
colnames(fluoro17)<- colnames(fluoro14)

#put together 2014-2017 data
fluoro <- rbind(fluoro14, fluoro15, fluoro16, fluoro17)

#select fluoro data which most closely matches water chem depths (0.1, 0.8, 1, etc.)
layer = data.frame(fluoro)

layer1 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 0.1)))
layer2 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 0.8)))
layer3 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 1.6)))
layer4 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 2.8)))
layer5 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 3.8)))
layer6 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 5.0)))
layer7 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 5.2)))
layer8 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 5.5)))
layer9 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 5.8)))
layer10 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 6.0)))
layer11 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 6.2)))
layer12 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 8.0)))
layer13 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 9.0)))
layer14 <- layer %>% group_by(Date) %>% slice(which.min(abs(as.numeric(Depth) - 9.3)))

## replace name of depth with the selected layer depths for ease of merging later
layer1$Depth <- 0.1 
layer2$Depth <- 0.8
layer3$Depth <- 1.6 
layer4$Depth <- 2.8 
layer5$Depth <- 3.8 
layer6$Depth <- 5.0 
layer7$Depth <- 5.2 
layer8$Depth <- 5.5 
layer9$Depth <- 5.8 
layer10$Depth <- 6.0 
layer11$Depth <- 6.2 
layer12$Depth <- 8.0 
layer13$Depth <- 9.0 
layer14$Depth <- 9.3 

df.final = rbind(layer1,layer2,layer3,layer4,layer5,layer6,layer7,layer8,layer9,layer10,
                 layer11,layer12,layer13,layer14)

write.csv(df.final, "./historical_model_selection/Data/Fluoro_FCR50_2014_2017.csv")



