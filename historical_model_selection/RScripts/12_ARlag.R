# examine data within astsa package to determine what time lag is appropriate for an auto-regressive time series model
# add in a new column in the dataset with the AR-lag response variable (chlorophyll t-?)

#install.packages("astsa")
library(astsa) # See note 1 below


data <- read.csv("./historical_model_selection/ARIMA_data/variables_all_2013_2016.csv")

plot(data$Chla_ugL, type="b") #time series plot of x with points marked as “o”
lag1.plot(data$Chla_ugL,30) # Plots x versus lag 1 of x.
acf(data$Chla_ugL, xlim=c(1,30)) # Plots the ACF of x for lags 1 to 19
xlag1=lag(data$Chla_interp,-15) # Creates a lag 1 of x variable. See note 2
y=cbind(data$Chla_ugL,xlag1) # See note 3 below
ar1fit=lm(y[,1]~y[,2])#Does regression, stores results object named ar1fit
summary(ar1fit) # This lists the regression results
plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18


# subset to just surface chlorophyll data
data2 <- data[data$Depth==1.0,]
plot(data2$Chla_ugL, type="l") #time series plot of x with points marked as “o”
lag1.plot(data2$Chla_ugL,30) # Plots x versus lag 1 of x.

png("./Figures/arima/ACF_SI_fig.png")
acf(data2$Chla_ugL, xlim=c(1,25), main = 'ACF of 2013-2016 Weekly Surface Chl-a') # Plots the ACF of x for lags 1 to 19
dev.off()

xlag1=lag(data2$Chla_ugL,1) # Creates a lag 1 of x variable. See note 2
y=as.data.frame(cbind(data2$Chla_ugL,xlag1)) # See note 3 below
ar1fit=lm(y[,1]~y[,2])#Does regression of t ~ t-1, stores results object named ar1fit
summary(ar1fit) # This lists the regression results
plot(ar1fit$fit,ar1fit$residuals) #plot of residuals versus fits
acf(ar1fit$residuals, xlim=c(1,18)) # ACF of the residuals for lags 1 to 18

# add AR lag of 1 timestep to dataframe (object: y)
colnames(y) <- c("Chla_ugL", "Chla_ARlag1")
data_surf <- left_join(data2, y)
data_surf <- data_surf %>% 
  select(Date:Depth, Chla_ugL, Chla_ARlag1, everything())

write.csv(data_surf, row.names = FALSE, "./historical_model_selection/ARIMA_data/variables_all_pluslag_2013_2016.csv")
