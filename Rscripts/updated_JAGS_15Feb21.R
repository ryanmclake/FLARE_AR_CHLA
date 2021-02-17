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

sd_beta <- 1000

#Vague priors on the beta

  beta1 ~ dnorm(0,1/(sd_beta*sd_beta))
  beta2 ~ dnorm(0,1/(sd_beta*sd_beta))
  beta3 ~ dnorm(0,1/(sd_beta*sd_beta))
  beta4 ~ dnorm(0,1/(sd_beta*sd_beta))


sd_process ~ dunif(0.001, 100) # prior for standard deviation of process error distribution
tau_process <- 1 / (sd_process * sd_process) # jags uses precision, 1/sd^2
tau_obs <- 1/(sd_obs*sd_obs) # precision for obs error, 1/sd of observation error ^2



# initialize latent state
latent.chl[1] ~ dnorm(x_ic, tau_process)
y[1] ~ dnorm(latent.chl[1], tau_obs)


  for(i in 2:N){
  # data model
    y[i] ~ dnorm(latent.chl[i], tau_obs)
    
  # process model
    chl[i] <- beta1 + beta2*latent.chl[i-1] + beta3*discharge[i] + beta4*sw[i]
    latent.chl[i] ~ dnorm(chl[i], tau_process) 
  }

  IC_forecast <- latent.chl[N]


}'

#Initialize parameters 
nchain = 4
chain_seeds <- c(200,800,1400, 600)
#param <- c(-5, 0, 5, 10)
init <- list()
for(i in 1:nchain){
  init[[i]] <- list(sd_process = 0.5,
                    beta1 = rnorm(1, 1.65, 0.26), # initial conditions for beta parameters taken from
                    beta2 = rnorm(1, 0.46, 0.08), # weekly training dataset model fitting
                    beta3 = rnorm(1, -3.05, 1.39), 
                    beta4 = rnorm(1, -0.0025, 0.00064),
                    .RNG.name = "base::Wichmann-Hill",
                    .RNG.seed = chain_seeds[i])
}

j.model <- jags.model(file = textConnection(AR),
                      data = list(
                        'sd_obs' = 0.21,
                        'x_ic' = obs$Chla_sqrt[1],
                        'y' = obs$Chla_sqrt,
                        'discharge' = obs$mean_flow,
                        'sw' = obs$ShortWave_mean,
                        'N' = N),
                      inits = init,
                      n.chains = 4,
                      n.adapt = 1000)  

#burn in, this updates the jags$state()
update(j.model,n.iter = 1000)

#sample from posterier starting from the end of the burn in.  The coda.samples track samples for a trace plot
samples = coda.samples(model = j.model,
                       variable.names = c('IC_forecast', 'beta1', 'beta2', 'beta3', 'beta4', 'sd_process'),
                       n.iter = 10000)

#### diagnostics
gelman.diag(samples)
traceplot(samples)

# library(LaplacesDemon)
ESS(samples)

par_matrix <- as.matrix(samples[1])
t <- nrow(data) # this is the the last observation in the dataset, therefore the distribution of latent chl to index as IC for the model


# output dataframe
null_out <- array(NA, dim = c(N, 7))
null_out <- as.data.frame(null_out)
colnames(null_out) <- c('mean', 'upper_CI', 'lower_CI', 'date', 'forecast_run_day', 'day_in_future', 'timestep')


for (i in 1:length(N) ){
  p <- sample(seq(1,length(samples[[1]][,i])), 420, replace = TRUE) # pick 420 random numbers from the first chain, i'th column
  temp <- samples[[1]][p,i]
  null_out[i,1] <-  mean(temp)^2
  null_out[i,2] <-  qnorm(0.975, mean = mean(temp), sd =sd(temp) )^2 #upper CI
  null_out[i,3] <- qnorm(0.025, mean = mean(temp), sd =sd(temp) )^2 #lower CI
}

null_out$date <- full_time 
null_out <- left_join(null_out, obs_all)
null_out <- null_out %>% mutate(residual = chla_ugL-mean)

# save only the forecasted data at end of time series
null_save <- null_out[null_out$date %in% forecast_time,]

# check plot
plot(null_out$date, null_out$mean, type = 'l')
points(null_save$date, null_save$upper_CI)
points(null_save$date, null_save$lower_CI)

