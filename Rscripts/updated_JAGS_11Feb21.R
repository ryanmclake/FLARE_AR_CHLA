# written 11 Feb 21
# updated JAGS code for FCR chl forecasts
# problems in former jags code:
  # AR term was not linked to latent state
  # y (observation vector) not linked past the first observation

# example:
  # https://github.com/eco4cast/neon4cast-aquatics/blob/master/03_generate_null_forecast_aquatics.R
  # https://github.com/melofton/Bayes_forecast_WG/blob/master/RCode/Jags_Models/Seasonal_for_loop/Seasonal_AR_Airtemp.R
  # https://nwfsc-timeseries.github.io/atsa-labs/sec-jags-uss.html
#########################################################################
if(data_assimilation){
  # read in file with all data and subset to the forecast day
  data <- read.csv(paste0(folder, '/training_datasets/data_arima_', timestep, '_through_2020.csv'))
  data$Date <- as.Date(data$Date)
  data <- data[data$Date<=forecast_start_day,]
  print('data subsetted to forecast start day')
  
}else{
  data <- read.csv(paste0(folder, '/data_arima_', timestep, '_through_2020.csv'))
  data$Date <- as.Date(data$Date)
  data <- data[data$Date<as.Date('2018-01-01'),]
  
  print('training data only')
}
library(rjags)
library(PerformanceAnalytics)

setwd(folder)

#Full time series with gaps
y <- c(data$Chla_sqrt)
time <- c(data$Date)


obs <- na.omit(data)

# define time vector
N <- nrow(obs)


AR = '
model {

#Vague priors on the beta
for(j in 1:4){
  beta[j] ~ dnorm(0,1/100000)
}

sigma ~ dunif(0.001, 100) # prior for standard deviation of sigma, process error
tau_process <- 1 / (sigma * sigma) # bayes uses precision, 1/sd^2
tau_obs <- 1/(sd_obs*sd_obs) # precision for obs error, 1/sd of observation error ^2
tau_ic <- 1/(sd_obs*sd_obs) # precision for obs error, 1/sd of observation error ^2

# initialize latent state
latent.chl[1] <- dnorm(x_ic, tau_ic)


  for(i in 2:N){
  # data model
    y[i] ~ dnorm(latent.chl[i], tau_obs)
    
  # process model
    chla[i] = beta[1] + beta[2]*latent.chl[i-1] + beta[3]*discharge[i] + beta[4]*sw[i]
    latent.chl[i] ~ dnorm(chl[i], tau_process) 
  }

#### save the last latent.chl value for IC to run the forecast  
  IC_forecast <- latent.chl[N]

}'

j.model <- jags.model(file = textConnection(AR),
                      data = list(
                        'sd_obs' = 0.5,
                        'x_ic' = obs$Chla_sqrt[1],
                        'y' = obs$Chla_sqrt,
                        'discharge' = obs$mean_flow,
                        'sw' = obs$ShortWave_mean,
                        'N' = N),
                      n.chains = 4,
                      n.adapt = 100)  

#burn in, this updates the jags$state()
update(j.model,n.iter = 1000)

#sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
samples = coda.samples(model = j.model,
                       variable.names = c('IC_forecast', 'beta','sigma'),
                       n.iter = 10000)

par_matrix <- as.matrix(samples[1])
t <- nrow(data) # this is the the last observation in the dataset, therefore the distribution of latent chl to index as IC for the model
