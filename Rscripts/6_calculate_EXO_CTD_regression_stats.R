###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### CHLA Forecasting in Falling Creek Reservoir
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### STEP 06 ---- 
# Build EXO and CTD regression

# download the CTD data from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/200/11/d771f5e9956304424c3bc0a39298a5ce" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

ctd <- read.csv(infile1,header=F 
                 ,skip=1
                 ,sep=","  
                 , col.names=c(
                   "Reservoir",     
                   "Site",     
                   "Date",     
                   "Depth_m",     
                   "Temp_C",     
                   "DO_mgL",     
                   "DO_pSat",     
                   "Cond_uScm",     
                   "Spec_Cond_uScm",     
                   "Chla_ugL",     
                   "Turb_NTU",     
                   "pH",     
                   "ORP_mV",     
                   "PAR_umolm2s",     
                   "Desc_rate",     
                   "Flag_Temp",     
                   "Flag_DO",     
                   "Flag_Cond",     
                   "Flag_SpecCond",     
                   "Flag_Chla",     
                   "Flag_Turb",     
                   "Flag_pH",     
                   "Flag_ORP",     
                   "Flag_PAR",     
                   "Flag_DescRate"    ), check.names=TRUE)
unlink(infile1)
ctd <- ctd %>% select(Reservoir, Site, Date, Depth_m, Chla_ugL) %>%
  mutate(Year = year(Date),
         Day = date(Date),
         Hour = hour(Date)) %>%
  filter(Reservoir == "FCR", Site == 50) %>%
  mutate(Depth_m = ifelse(Depth_m <= 1.2 & Depth_m >= 0.8, 1, Depth_m)) 

ctd1 <- ctd %>% filter(Depth_m == 1)

ctd2 <- aggregate(Chla_ugL ~ Day + Hour + Depth_m, data = ctd1, mean) 


# download EXO data from EDI
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/edi/271/5/c1b1f16b8e3edbbff15444824b65fe8f" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")


exo <-read.csv(infile1,header=F 
               ,skip=1
               ,sep=","  
               , col.names=c(
                 "Reservoir",     
                 "Site",     
                 "DateTime",     
                 "ThermistorTemp_C_surface",     
                 "ThermistorTemp_C_1",     
                 "ThermistorTemp_C_2",     
                 "ThermistorTemp_C_3",     
                 "ThermistorTemp_C_4",     
                 "ThermistorTemp_C_5",     
                 "ThermistorTemp_C_6",     
                 "ThermistorTemp_C_7",     
                 "ThermistorTemp_C_8",     
                 "ThermistorTemp_C_9",     
                 "RDO_mgL_5",     
                 "RDOsat_percent_5",     
                 "RDO_mgL_5_adjusted",     
                 "RDOsat_percent_5_adjusted",     
                 "RDOTemp_C_5",     
                 "RDO_mgL_9",     
                 "RDOsat_percent_9",     
                 "RDO_mgL_9_adjusted",     
                 "RDOsat_percent_9_adjusted",     
                 "RDOTemp_C_9",     
                 "EXOTemp_C_1",     
                 "EXOCond_uScm_1",     
                 "EXOSpCond_uScm_1",     
                 "EXOTDS_mgL_1",     
                 "EXODOsat_percent_1",     
                 "EXODO_mgL_1",     
                 "EXOChla_RFU_1",     
                 "EXOChla_ugL_1",     
                 "EXOBGAPC_RFU_1",     
                 "EXOBGAPC_ugL_1",     
                 "EXOfDOM_RFU_1",     
                 "EXOfDOM_QSU_1",     
                 "EXO_pressure",     
                 "EXO_depth",     
                 "EXO_battery",     
                 "EXO_cablepower",     
                 "EXO_wiper",     
                 "Lvl_psi_9",     
                 "LvlTemp_C_9",     
                 "RECORD",     
                 "CR6_Batt_V",     
                 "CR6Panel_Temp_C",     
                 "Flag_All",     
                 "Flag_DO_1",     
                 "Flag_DO_5",     
                 "Flag_DO_9",     
                 "Flag_Chla",     
                 "Flag_Phyco",     
                 "Flag_TDS",     
                 "Flag_fDOM",     
                 "Flag_Temp_Surf",     
                 "Flag_Temp_1",     
                 "Flag_Temp_2",     
                 "Flag_Temp_3",     
                 "Flag_Temp_4",     
                 "Flag_Temp_5",     
                 "Flag_Temp_6",     
                 "Flag_Temp_7",     
                 "Flag_Temp_8",     
                 "Flag_Temp_9"    ), check.names=TRUE)

unlink(infile1)
exo <- exo %>% select(DateTime, EXOChla_ugL_1) %>% 
  filter(EXOChla_ugL_1 != "NAN")
exo <- exo %>%
  mutate(Day = date(DateTime),
         Hour = hour(DateTime)) %>%
  select(-DateTime)
exo$EXOChla_ugL_1 <- as.numeric(exo$EXOChla_ugL_1)


exo <- exo %>% group_by(Day) %>% 
  mutate(EXOChla_daily_mean = mean(EXOChla_ugL_1))

exo_daily <- exo %>% distinct(Day, .keep_all = TRUE)

overlap_WW <- left_join(ctd2, exo_daily, by = c("Day")) %>%
  filter(!is.na(EXOChla_daily_mean))
overlap_WW <- overlap_WW %>% select(Day, Chla_ugL, EXOChla_daily_mean)
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