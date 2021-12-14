###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 06 ---- 
# Build EXO and CTD regression
folder <- getwd()

ctd <- read.csv(paste0(folder, '/sim_files/CTD_final_2013_2020.csv'))
ctd <- ctd %>% dplyr::select(Reservoir, Site, Date, Depth_m, Chla_ugL) %>%
  mutate(Year = year(Date),
         Day = date(Date),
         Hour = hour(Date)) %>%
  filter(Reservoir == "FCR", Site == 50) %>%
  mutate(Depth_m = ifelse(Depth_m <= 1.2 & Depth_m >= 0.8, 1, Depth_m)) 

ctd1 <- ctd %>% filter(Depth_m == 1)

ctd2 <- aggregate(Chla_ugL ~ Day + Hour + Depth_m, data = ctd1, mean) 


# download EXO data from EDI
exo <- read.csv(paste0(folder, '/sim_files/Catwalk_EDI_2020.csv'))
exo <- exo %>% dplyr::select(DateTime, EXOChla_ugL_1) %>% 
  filter(EXOChla_ugL_1 != "NAN")
exo <- exo %>%
  mutate(Day = date(DateTime),
         Hour = hour(DateTime)) %>%
  dplyr::select(-DateTime)
exo$EXOChla_ugL_1 <- as.numeric(exo$EXOChla_ugL_1)


exo <- exo %>% group_by(Day) %>% 
  mutate(EXOChla_daily_mean = mean(EXOChla_ugL_1))

exo_daily <- exo %>% distinct(Day, .keep_all = TRUE)

overlap_WW <- left_join(ctd2, exo_daily, by = c("Day")) %>%
  filter(!is.na(EXOChla_daily_mean))
overlap_WW <- overlap_WW %>% dplyr::select(Day, Chla_ugL, EXOChla_daily_mean)
colnames(overlap_WW) <- c('Day', 'CTDChla_ugL', 'EXOChla_ugL')
overlap_WW <- overlap_WW[overlap_WW$Day<'2019-01-01',]


## build linear model with EXO on x axis
mod_WW <- lm(CTDChla_ugL ~ EXOChla_ugL, data = overlap_WW)
res_WW <- resid(mod_WW)
summary(mod_WW)
rout <- list(paste('Model: ', round(coef(mod_WW)[1], 3), ' + ',
                   round(coef(mod_WW)[2], 3), 'x,', ' R^2 = ', round(summary(mod_WW)[['r.squared']], 3), sep = ''))

#regression plot
plot1 <- ggplot(data = overlap_WW, aes(x = EXOChla_ugL, y = CTDChla_ugL))+
  geom_point(size = 3)+
  #stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 6)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "springgreen3")+
  geom_text(aes(x = 8.5, y = 2, label = rout[[1]]), vjust = 0, colour = "black") +
  xlab("Chl-a (EXO sonde) - ug/L")+
  ylab("Chl-a (CTD) - ug/L")
plot1
#residual plots
plot(overlap_WW$EXOChla_ugL, res_WW)
abline(0,0)

####### and a linear model in sqrt units
overlap_WW <- overlap_WW %>% mutate(EXOChla_ugL_1_sqrt = sqrt(EXOChla_ugL)) %>% 
  mutate(CTDChla_ugL_1_sqrt = sqrt(CTDChla_ugL))

# run the linear model on sqrt transformed data
# with EXO on the x-axis, so converting CTD into EXO
# dependent or y variable first
mod_sqrt <- lm(CTDChla_ugL_1_sqrt ~ EXOChla_ugL_1_sqrt, data = overlap_WW)

res_sqrt <- resid(mod_sqrt)
sd(res_sqrt)
summary(mod_sqrt)
rout_sqrt <- list(paste('Model: ', round(coef(mod_sqrt)[1], 3), ' + ',
                   round(coef(mod_sqrt)[2], 3), 'x,', ' R^2 = ', round(summary(mod_sqrt)[['r.squared']], 3), sep = ''))


plot2 <- ggplot(data = overlap_WW, aes(x = EXOChla_ugL_1_sqrt, y = CTDChla_ugL_1_sqrt))+
  geom_point(size = 3)+
  #stat_smooth_func(geom="text",method="lm",hjust=0,parse=TRUE, size = 6)+
  geom_abline(slope = 1, intercept = 0, size = 1)+
  geom_smooth(method='lm',formula=y~x, se = FALSE, size = 1, colour = "springgreen3")+
  geom_text(aes(x = 3, y = 1.5, label = rout_sqrt[[1]]), vjust = 0, colour = "black") +
  xlab("Chl-a (EXO sonde) - ug/L")+
  ylab("Chl-a (CTD) - ug/L")
plot2
#residual plots
plot(overlap_WW$EXOChla_ugL_1_sqrt, res_sqrt)
abline(0,0)

summary(mod_sqrt)
summary(mod_WW)