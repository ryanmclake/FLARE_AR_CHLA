###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 13 ----

# determine summary stats (median, standard deviation, max values) for observed chl-a during training period and during forecast period
setwd(folder)
timestep <- '1day'
sim_name <- sim_name


# training dataset May-Oct 2013-2016
train <- read.csv('./training_datasets/data_arima_updated.csv')
train$Date <- as.Date(train$Date)
train <- train %>% mutate(Chla = Chla_sqrt^2) %>% 
  mutate(Chla_EXO = ((Chla - 0.255)/0.605))

median(train$Chla_EXO)
median(train$Chla)
sd(train$Chla_EXO)
plot(train$Date, train$Chla_EXO, xlab = 'Date', ylab = 'Chl-a (ug/L)')


# chl over the forecast period
stuff <- read.csv('./obs_chl_15Aug18_29Aug20.csv')
stuff$Date <- as.Date(stuff$Date, "%Y-%m-%d")
stuff <- stuff[order(stuff$Date),]

colnames(stuff) <- c('Date', 'Chla_EXO')

# for summary stats, remove spin up dates, so anything before Dec 31, 2018
stuff <- stuff[stuff$Date>'2018-12-31',]

median(stuff$Chla_EXO)
sd(stuff$Chla_EXO)
max(stuff$Chla_EXO)

plot(stuff$Date, stuff$Chla_EXO, xlim = c(as.Date('2020-01-01'), as.Date('2020-05-01')), type = 'l')
abline(h = 17.1)
abline(v = as.Date('2020-03-16'))
abline(v = as.Date('2020-04-23'))

all <- rbind(train, stuff)
par(mar= c(5.1, 5.1, 4.1, 2.1))
plot(all$Date, all$Chla_EXO, xlab = 'Date', ylab = 'Chl-a (μg/L)', cex.axis = 2, cex.main = 2, cex.lab = 2, cex = 1.5)


## and BVR stats
bvr <- read.csv('./training_datasets/data_arima_7day_BVR.csv')

bvr$Date <- as.Date(bvr$Date)
bvr <- bvr %>% mutate(Chla = Chla_sqrt^2) %>% 
  mutate(Chla_EXO = ((Chla - 0.255)/0.605))

median(bvr$Chla)
sd(bvr$Chla_EXO)
plot(train$Date, train$Chla_EXO, xlab = 'Date', ylab = 'Chl-a (ug/L)')

