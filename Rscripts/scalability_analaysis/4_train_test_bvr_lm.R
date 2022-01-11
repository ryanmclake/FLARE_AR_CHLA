# train and validate AR model on BVR data

data <- read.csv('./training_datasets/data_arima_7day_BVR.csv')
data$Date <- as.Date(data$Date)
train <- data[data$Date < as.Date('2019-01-01'),]
test <- data[data$Date > as.Date('2019-01-01'),]

mod <- glm(Chla_sqrt~Chla_ARlag_timestep_sqrt + flow_cms +ShortWave_mean, 
                 data = train, family = gaussian, na.action = 'na.fail')

hist(mod$residuals)
confint(mod)
sd(mod$residuals)
summary(mod)


pred <- predict(mod, newdata = test)
rmse(test$Chla_sqrt^2, pred^2)
r2 <- function (x, y) cor(x, y) ^ 2
r2(test$Chla_sqrt, pred)


plot(data$Date, data$Chla_sqrt^2, xlab = 'Date', ylab = 'Chl-a (ug/L)')
points(test$Date, pred^2, type = 'l')
legend('topleft', c('observed', 'modeled'), lty = c(0, 1), pch = c(1, NA))
