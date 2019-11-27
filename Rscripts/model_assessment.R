# function to calculate a list of model assessment metrics

model_metrics <- function(sim, obs){
  
  library(Metrics)
  library(hydroGOF)
  
  residual <- sim - obs
  data_minusglobamean <- obs - mean(obs, na.rm = TRUE)
  
  RMSE <- rmse(sim, obs)
  NSE <- NSE(sim, obs)
  KGE <- KGE(sim, obs)
  bias <- mean(residual)  #mean residual
  bias_sd <- sd(sim)/sd(obs)  # bias calculations for mean and median of both weeks (1 is non-biased, <1 means model is smoothing, >1 means model is predicting more fluctuations than are observed)
  R2_1_1 <- 1 - sum(residual^2)/sum(data_minusglobamean^2)
  
  x <- summary(lm(sim ~ obs))
  coeff_determination <- x$r.squared #r2 from linear model output
  
  cor_p <- cor(sim, obs, method = 'pearson') #pearson's correlation coefficient
 
   return(c(RMSE = RMSE, NSE = NSE, KGE = KGE, bias = bias, bias_sd = bias_sd, R2_1_1 = R2_1_1, coeff_determination = coeff_determination, cor_p = cor_p))
}

test <- model_metrics(week1$forecast_mean_chl, week1$obs_chl_EXO)
