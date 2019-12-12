# function for making model plots and assessment metrics of different forecast horizons

assess_forecast <- function(folder, forecast_file, forecast_horizon){
  
  data <- read.csv(forecast_file)
  data$forecast_date <- as.Date(data$forecast_date)
  
  hist(data$residual)
  title(paste0('Residual at Day ', forecast_horizon))
  
  plot(data$forecast_date, data$obs_chl_EXO)
  points(data$forecast_date, data$forecast_mean_chl, col = 'red', type = 'l')
  title(paste0('Observed and Forecast at Day ', forecast_horizon))
  
  png(paste0('C:/Users/wwoel/Dropbox/Thesis/Figures/arima/daily_forecasts/ConfIntForecasts2019_daily_', forecast_horizon, '.png'), width = 1100, height = 800)
  p <- ggplot(data, aes(forecast_date, forecast_mean_chl)) +
    geom_line(size = 2) +
    geom_point(aes(forecast_date, obs_chl_EXO), size = 4, stroke = 0, shape = 19, color = 'green4') +
    geom_ribbon(aes(ymin = forecast_CI95_lower, ymax = forecast_CI95_upper), fill = 'forestgreen', linetype = 2, alpha = 0.2) +
    xlab('Date') +
    ylab('Chlorophyll a (μg/L)') +
    #geom_vline(xintercept = as.numeric(as.Date("2019-02-28", "%Y-%m-%d")), color = 'blue', size = 1.5) +
    #geom_vline(xintercept = as.numeric(as.Date("2019-03-20", "%Y-%m-%d")), color = 'blue', size = 1.5) +
    theme(axis.text.x = element_text(size = 35),
          axis.text.y = element_text(size = 50),
          axis.title.x = element_text(size =45),
          axis.title.y = element_text(size = 45),
          legend.title = element_text(size = 35),
          legend.text = element_text(size = 30),
          panel.grid.major = element_blank(),
          legend.position = 'right',
          panel.grid.minor = element_blank())
  # panel.background = element_rect(fill = '#9DC3E6', color = '#9DC3E6'),
  #  plot.background = element_rect(fill = '#9DC3E6', color = '#9DC3E6'))
  print(p)
  dev.off()
  
  # read in the null model data
  null <- read.csv(paste0(folder, '/daily_null.csv'))
  
  #source(paste0(folder,"/","Rscripts/model_assessment.R")) # sim, obs
  null_metrics <- model_metrics(null[,forecast_horizon+3], null$exo_chl_ugL) # index the forecast_horizon + 3 because there are three columns before the null models come in
  forecast_metrics <- model_metrics(data$forecast_mean_chl, data$obs_chl_EXO)
  print('null metrics')
  print(null_metrics)
  print('forecast_metrics')
  print(forecast_metrics)
  
  }


assess_forecast(folder = folder, forecast_file = paste0(forecast_folder, '/day_7.csv'), forecast_horizon = 7)



