library(tidyverse)
library(dplyr)
library(lubridate)
library(imputeTS) 
library(modelr)
library(RcppRoll)

inflow_qaqc_file <- "C:/Users/wwoel/Desktop/FLARE_AR_CHLA/SCCData/manual-data/inflow_for_EDI_2013_06Mar2020.csv"


flow <- read_csv(inflow_qaqc_file, guess_max = 1000000) %>% 
  rename("timestamp" = DateTime) %>% 
  select(timestamp, WVWA_Flow_cms, WVWA_Temp_C, VT_Flow_cms, VT_Temp_C) %>% 
  mutate(day = day(timestamp),
         year = year(timestamp),
         month = month(timestamp)) %>%
  group_by(day, year, month) %>% 
  summarize(WVWA_Flow_cms = mean(WVWA_Flow_cms, na.rm = TRUE),
            WVWA_Temp_C = mean(WVWA_Temp_C, na.rm = TRUE),
            VT_Flow_cms = mean(VT_Flow_cms, na.rm = TRUE),
            VT_Temp_C = mean(VT_Temp_C, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(day = as.numeric(day)) %>% 
  mutate(day = ifelse(as.numeric(day) < 10, paste0("0",day),day)) %>% 
  mutate(time = as_date(paste0(year,"-",month,"-",day))) %>% 
  select(time,WVWA_Flow_cms,WVWA_Temp_C,VT_Flow_cms,VT_Temp_C) %>% 
  mutate(VT_Flow_cms = ifelse(is.nan(VT_Flow_cms), NA, VT_Flow_cms),
         VT_Temp_C = ifelse(is.nan(VT_Temp_C), NA, VT_Temp_C),
         WVWA_Flow_cms = ifelse(is.nan(WVWA_Flow_cms), NA, WVWA_Flow_cms),
         WVWA_Temp_C = ifelse(is.nan(WVWA_Temp_C), NA, WVWA_Temp_C)) %>% 
  arrange(time) 

inflow <- tibble(time = seq(first(flow$time), last(flow$time), by = "1 day")) %>% 
  left_join(flow) %>% 
  mutate(TEMP = ifelse(is.na(VT_Temp_C), WVWA_Temp_C, VT_Temp_C),
         FLOW = ifelse(time > as_date("2019-06-07"), VT_Flow_cms, WVWA_Flow_cms),
         SALT = 0) %>% 
  select(time, FLOW, TEMP) %>% 
  rename(WaterTemp = TEMP)

met_qaqc_file <- read_csv("C:/Users/wwoel/Desktop/FLARE_AR_CHLA/SCCData/manual-data/Met_final_2015_2018.csv")

met <- met_qaqc_file %>% 
  rename("timestamp" = DateTime) %>% 
  select(timestamp, AirTemp_Average_C, Rain_Total_mm) %>% 
  mutate(day = day(timestamp),
         year = year(timestamp),
         month = month(timestamp)) %>%
  group_by(day, year, month) %>% 
  summarize(AirTemp = mean(AirTemp_Average_C, na.rm = TRUE),
            Rain = sum(Rain_Total_mm)) %>% 
  mutate(Rain = Rain *  0.001) %>% 
  ungroup() %>% 
  mutate(day = as.numeric(day)) %>% 
  mutate(day = ifelse(as.numeric(day) < 10, paste0("0",day),day)) %>% 
  mutate(time = as_date(paste0(year,"-",month,"-",day))) %>% 
  mutate(AirTemp = ifelse(is.nan(AirTemp), NA, AirTemp),
         Rain = ifelse(is.nan(Rain), NA, Rain)) %>% 
  select(time, AirTemp, Rain) %>% 
  arrange(time) %>% 
  mutate(AirTempMean = roll_mean(AirTemp, n = 7, align = "right",fill=NA),
         RainMean = roll_mean(Rain, n = 7, align = "right",fill=NA))

inflow_met <- left_join(inflow, met, by = "time") %>% 
  mutate(WaterTemp_lag1 = lag(WaterTemp, 1),
         Rain_lag1 = lag(Rain, 1),
         AirTemp_lag1 = lag(AirTemp, 1),
         Flow_lag1 = lag(FLOW, 1))

inflow_met %>% 
  select(time, AirTemp, WaterTemp, AirTempMean) %>% 
  pivot_longer(col = -time, names_to = "var", values_to = "value") %>% 
  ggplot(aes(x = time, y = value, color = var)) + 
  geom_point()


inflow_met %>% 
  ggplot(aes(x = WaterTemp_lag1, y = WaterTemp)) + 
  geom_point()

inflow_met_pre <- inflow_met %>% 
  filter(time > as_date("2017-01-01"),# &
    time < as_date("2018-07-12"),
         !is.na(WaterTemp_lag1),
         !is.na(AirTempMean),
         !is.na(Flow_lag1),
         !is.na(RainMean))


inflow_met_pre <- inflow_met %>% 
  filter(time < as_date("2018-07-12"),
         !is.na(WaterTemp_lag1),
         !is.na(AirTempMean),
         !is.na(Flow_lag1),
         !is.na(RainMean))

######

fit <- lm(WaterTemp ~ WaterTemp_lag1 + AirTemp_lag1, inflow_met_pre)

inflow_met %>% 
  add_predictions(fit) %>% 
  select(time, WaterTemp, pred) %>% 
  pivot_longer(-time, names_to = "var", values_to = "value") %>% 
  ggplot(aes(x = time, y = value, color = var)) + 
  geom_line()

inflow_met %>% 
  add_residuals(fit) %>% 
  ggplot(aes(x = time, y = resid)) + 
  geom_line()

inflow_met_pre %>% 
  add_residuals(fit) %>% 
  summarise(sd = sd(resid, na.rm = TRUE))

####

fit <- lm(FLOW ~ Flow_lag1 + Rain_lag1, inflow_met_pre)

inflow_met %>% 
  add_predictions(fit) %>% 
  select(time, WaterTemp, pred) %>% 
  pivot_longer(-time, names_to = "var", values_to = "value") %>% 
  ggplot(aes(x = time, y = value, color = var)) + 
  geom_line()

inflow_met %>% 
  add_residuals(fit) %>% 
  ggplot(aes(x = time, y = resid)) + 
  geom_line()

inflow_met_pre %>% 
  add_residuals(fit) %>% 
  summarise(sd = sd(resid, na.rm = TRUE))

######

