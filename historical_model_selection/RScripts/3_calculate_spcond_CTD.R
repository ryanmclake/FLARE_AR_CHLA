library(tidyverse)
library(ggplot2)
library(dplyr)

#setwd("C:/Users/wwoel/Dropbox/FCR_TimeSeries")

ctd <- read.csv("./historical_model_selection/Data/CTD/FCR_CTD_50_binned.csv") #ctd data for FCR at site 50, processed for layers

# calculate specific conductivity within CTD data
ctd <- mutate(ctd, SpCond_calc = Cond_uScm/(1+(0.0191*(Temp_C - 25))))

ggplot(ctd) + geom_point(aes(x =Date, y = SpCond_calc), color = "slateblue3") +
  ylim(c(0, 60)) + ggtitle("Calculated Sp Conductivity")
# check calculated sp cond with CTD provided sp cond
ggplot(ctd) + geom_point(aes(x = Spec_Cond_uScm, y = SpCond_calc)) + ylim(c(0,50)) +
  geom_abline(slope = 1, intercept = 0)

# good match, so let's remove the CTD-generated data to clean things up
ctd2 <- ctd %>% select(-(Spec_Cond_uScm))

#create a csv with the new calculated specific conductance
write.csv(ctd2, "./historical_model_selection/Data/CTD/FCR_CTD_50_binned.csv", row.names = FALSE)
