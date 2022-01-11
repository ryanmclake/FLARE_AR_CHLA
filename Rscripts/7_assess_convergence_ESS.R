###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 08 ----
# assess convergence and effective sample size

# run statistics on the first day of the forecasting period (aka after training and spinup)
forecast_start_day <- forecast_start_day

###########################################################################################################################################################################
# first for daily
timestep <- '1day' # character definition of the timestep
timestep_numeric <- 1 # maybe timestep_numeric and timestep_interval are actually the same thing and not both needed -_-
timestep_interval <- 1 # the interval in between timesteps, e.g. 4day would be 4; daily would be 1; weekly would be 7
max_timestep <- 14 #maximum number of timesteps that can be propagated to the max time horizon (e.g., daily is 14, weekly is 2)
max_horizon <- 14 # maximum number of days that are propagated in this forecast (e.g. daily timestep has max_horizon = 14)

data <- read.csv(paste0(folder, '/training_datasets/data_arima_', timestep, '.csv'))
data$Date <- as.Date(data$Date)
data <- data[data$Date<forecast_start_day,]

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
samples_daily = coda.samples(model = j.model,
                       variable.names = c('IC_forecast', 'beta1', 'beta2', 'beta3', 'beta4', 'sd_process'),
                       n.iter = 10000)
###########################################################################################################################################################################
# for weekly
timestep <- '7day' # character definition of the timestep
data <- read.csv(paste0(folder, '/training_datasets/data_arima_', timestep, '.csv'))
data$Date <- as.Date(data$Date)
data <- data[data$Date<forecast_start_day,]

#Full time series with gaps
y <- c(data$Chla_sqrt)
time <- c(data$Date)


obs <- na.omit(data)

# define time vector
N <- nrow(obs)

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
samples_weekly = coda.samples(model = j.model,
                             variable.names = c('IC_forecast', 'beta1', 'beta2', 'beta3', 'beta4', 'sd_process'),
                             n.iter = 10000)
###########################################################################################################################################################################
# for fortnightly
timestep <- '14day' # character definition of the timestep
data <- read.csv(paste0(folder, '/training_datasets/data_arima_', timestep, '.csv'))
data$Date <- as.Date(data$Date)
data <- data[data$Date<forecast_start_day,]

setwd(folder)

N <- nrow(data)
setwd(folder)

#Full time series with gaps
y <- c(data$Chla_sqrt)
time <- c(data$Date)


obs <- na.omit(data)

# define time vector
N <- nrow(obs)

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
samples_fortnightly = coda.samples(model = j.model,
                              variable.names = c('IC_forecast', 'beta1', 'beta2', 'beta3', 'beta4', 'sd_process'),
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

si_table <- matrix(NA, nrow = 3, ncol = 6)
rownames(si_table) <- c('daily', 'weekly', 'fortnightly')
colnames(si_table) <- c('IC_forecast', 'Intercept', 'Chlorophyll-a', 'Discharge', 'Shortwave', 'Error')

si_table[1,] <- ess_daily[,1]
si_table[2,] <- ess_weekly[,1]
si_table[3,] <- ess_fortnightly[,1]

write.csv(si_table, paste0(folder, '/FCR_forecasts/effective_sample_size_SI_table_', Sys.Date(), '.csv'))
