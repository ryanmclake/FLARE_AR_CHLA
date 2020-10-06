# assess convergence and effective sample size

# run statistics on the first day of the forecasting period (aka after training and spinup)
forecast_start_day <-"2019-01-01 00:00:00"
data_location <-  "C:/Users/wwoel/Desktop/FLARE_AR_CHLA/SCCData"
folder <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA"

###########################################################################################################################################################################
# first for daily
timestep <- '1day' # character definition of the timestep
timestep_numeric <- 1 # maybe timestep_numeric and timestep_interval are actually the same thing and not both needed -_-
timestep_interval <- 1 # the interval in between timesteps, e.g. 4day would be 4; daily would be 1; weekly would be 7
max_timestep <- 14 #maximum number of timesteps that can be propagated to the max time horizon (e.g., daily is 14, weekly is 2)
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 14)

data <- read.csv(paste0(folder, '/data_arima_', timestep, '_through_2020.csv'))
data$Date <- as.Date(data$Date)
data <- data[data$Date<forecast_start_day,]

library(rjags)
library(PerformanceAnalytics)

setwd(folder)

N <- nrow(data)

sink("jags_model.bug")
cat('model {
  for (i in 1:N) {
    chla[i] ~ dnorm(chla.hat[i], tau)
    chla.hat[i] <- beta[1] + beta[2]*chla_lag[i] + beta[3]*discharge[i] + beta[4]*sw[i]
  }
  
  #Vague priors on the beta
  for(j in 1:4){
    beta[j] ~ dnorm(0,1/100000)
  }

  # Prior for the inverse variance
  sigma ~ dunif(0, 100) # standard deviation
	tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
}'
)
sink()

jags <- jags.model('jags_model.bug',
                   data = list('chla' = data$Chla_sqrt,
                               'chla_lag' = data$Chla_ARlag_timestep_sqrt,
                               'discharge' = data$mean_flow,
                               'sw' = data$ShortWave_mean,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 1000)

#sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
samples_daily = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 10000)

###########################################################################################################################################################################
# for weekly
timestep <- '7day' # character definition of the timestep
data <- read.csv(paste0(folder, '/data_arima_', timestep, '_through_2020.csv'))
data$Date <- as.Date(data$Date)
data <- data[data$Date<forecast_start_day,]

setwd(folder)

N <- nrow(data)

sink("jags_model.bug")
cat('model {
  for (i in 1:N) {
    chla[i] ~ dnorm(chla.hat[i], tau)
    chla.hat[i] <- beta[1] + beta[2]*chla_lag[i] + beta[3]*discharge[i] + beta[4]*sw[i]
  }
  
  #Vague priors on the beta
  for(j in 1:4){
    beta[j] ~ dnorm(0,1/100000)
  }

  # Prior for the inverse variance
  sigma ~ dunif(0, 100) # standard deviation
	tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
}'
)
sink()

jags <- jags.model('jags_model.bug',
                   data = list('chla' = data$Chla_sqrt,
                               'chla_lag' = data$Chla_ARlag_timestep_sqrt,
                               'discharge' = data$mean_flow,
                               'sw' = data$ShortWave_mean,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 1000)

#sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
samples_weekly = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 10000)

###########################################################################################################################################################################
# for fortnightly
timestep <- '14day' # character definition of the timestep
data <- read.csv(paste0(folder, '/data_arima_', timestep, '_through_2020.csv'))
data$Date <- as.Date(data$Date)
data <- data[data$Date<forecast_start_day,]

setwd(folder)

N <- nrow(data)

sink("jags_model.bug")
cat('model {
  for (i in 1:N) {
    chla[i] ~ dnorm(chla.hat[i], tau)
    chla.hat[i] <- beta[1] + beta[2]*chla_lag[i] + beta[3]*discharge[i] + beta[4]*sw[i]
  }
  
  #Vague priors on the beta
  for(j in 1:4){
    beta[j] ~ dnorm(0,1/100000)
  }

  # Prior for the inverse variance
  sigma ~ dunif(0, 100) # standard deviation
	tau <- 1 / (sigma * sigma) # sigma^2 doesnt work in JAGS
}'
)
sink()

jags <- jags.model('jags_model.bug',
                   data = list('chla' = data$Chla_sqrt,
                               'chla_lag' = data$Chla_ARlag_timestep_sqrt,
                               'discharge' = data$mean_flow,
                               'sw' = data$ShortWave_mean,
                               'N' = N),
                   n.chains = 4,
                   n.adapt = 100)

#burn in, this updates the jags$state()
update(jags,n.iter = 1000)

#sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
samples_fortnightly = coda.samples(model = jags,
                       variable.names = c('beta','sigma'),
                       n.iter = 10000)

################################################################################################################################################################################
gelman.diag(samples_daily)
gelman.diag(samples_weekly)
gelman.diag(samples_fortnightly)

ess_daily <- ESS(samples_daily)
ess_daily <- as.data.frame(ess_daily)
ess_weekly <- ESS(samples_weekly)
ess_weekly <- as.data.frame(ess_weekly)
ess_fortnightly <- ESS(samples_fortnightly)
ess_fortnightly <- as.data.frame(ess_fortnightly)

si_table <- matrix(NA, nrow = 3, ncol = 5)
rownames(si_table) <- c('daily', 'weekly', 'fortnightly')
colnames(si_table) <- c('Intercept', 'Chlorophyll-a', 'Discharge', 'Shortwave', 'Error')

si_table[1,] <- ess_daily[,1]
si_table[2,] <- ess_weekly[,1]
si_table[3,] <- ess_fortnightly[,1]

write.csv(si_table, paste0(folder, './FCR_forecasts/effective_sample_size_SI_table.csv'))
