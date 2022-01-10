


qaqc <- function(data_file, #data2_file, 
                 maintenance_file,  output_file, output2_file)
{

  BVRDATA_COL_NAMES = c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_1",
                        "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4", "ThermistorTemp_C_5",
                        "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8", "ThermistorTemp_C_9",
                        "ThermistorTemp_C_10","ThermistorTemp_C_11","ThermistorTemp_C_12","ThermistorTemp_C_13",
                        "RDO_mgL_6", "RDOsat_percent_6", "RDOTemp_C_6", "RDO_mgL_13",
                        "RDOsat_percent_13", "RDOTemp_C_13", "EXO_Date", "EXO_Time", "EXOTemp_C_1_5", "EXOCond_uScm_1_5",
                        "EXOSpCond_uScm_1_5", "EXOTDS_mgL_1_5", "EXODOsat_percent_1_5", "EXODO_mgL_1_5", "EXOChla_RFU_1_5",
                        "EXOChla_ugL_1_5", "EXOBGAPC_RFU_1_5", "EXOBGAPC_ugL_1_5", "EXOfDOM_RFU_1_5", "EXOfDOM_QSU_1_5",
                        "EXOTurbidity_FNU_1_5", "EXOTSS_mg_1_5","EXO_pressure_1_5", "EXO_depth", "EXO_battery",
                         "EXO_cablepower", "EXO_wiper", "Lvl_psi_13", "LvlTemp_C_13")
  
  # after maintenance, DO values will continue to be replaced by NA until DO_mgL returns within this threshold (in mg/L)
  # of the pre-maintenance value
  DO_RECOVERY_THRESHOLD <- 1
  
  # columns where certain values are stored
  DO_MGL_COLS <- c(18, 21, 31)
  DO_SAT_COLS <- c(19, 22, 30)
  DO_FLAG_COLS <- c(46, 47, 48)
  
  # depths at which DO is measured
  #what do I say for DO depths
  DO_DEPTHS <- c(1.5, 7.5, 0.5)
  
  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4
  
  #read in data from obs1 above
  
  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  bvrdata <- read_csv(data_file, col_names = BVRDATA_COL_NAMES,
                      col_types = cols(.default = col_double(), DateTime = col_datetime()))
 

#read in maintenance log
  log <- read_csv(maintenance_file,
    col_types = cols(
    .default = col_character(),
    TIMESTAMP_start = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = col_integer()
  ))
 
  # add flag columns
  bvrdata$Flag_All <- 0
  bvrdata$Flag_DO_1_5 <- 0
  bvrdata$Flag_DO_6 <- 0
  bvrdata$Flag_DO_13 <- 0
  bvrdata$Flag_Chla <- 0
  bvrdata$Flag_Phyco <- 0
  bvrdata$Flag_TDS <- 0
  bvrdata$Flag_fDOM <- 0
  bvrdata$Flag_Cond <-0
  bvrdata$Flag_Lvl_13 <-0
  bvrdata$Flag_Temp_1 <-0
  bvrdata$Flag_Temp_2 <-0
  bvrdata$Flag_Temp_3 <-0
  bvrdata$Flag_Temp_4 <-0
  bvrdata$Flag_Temp_5 <-0
  bvrdata$Flag_Temp_6 <-0
  bvrdata$Flag_Temp_7 <-0
  bvrdata$Flag_Temp_8 <-0
  bvrdata$Flag_Temp_9 <-0
  bvrdata$Flag_Temp_10 <-0
  bvrdata$Flag_Temp_11 <-0
  bvrdata$Flag_Temp_12 <-0
  bvrdata$Flag_Temp_13 <-0
  
  
  # replace negative DO values with 0
  bvrdata <- bvrdata %>%
    mutate(Flag_DO_1_5 = ifelse((! is.na(EXODO_mgL_1_5) & EXODO_mgL_1_5 < 0)
                            | (! is.na(EXODOsat_percent_1_5) & EXODOsat_percent_1_5 < 0), 3, Flag_DO_1_5)) %>%
    mutate(EXODO_mgL_1_5 = ifelse(EXODO_mgL_1_5 < 0, 0, EXODO_mgL_1_5)) %>%
    mutate(EXODOsat_percent_1_5 = ifelse(EXODOsat_percent_1_5 <0, 0, EXODOsat_percent_1_5))
  
  bvrdata <- bvrdata %>%
    mutate(Flag_DO_6 = ifelse((! is.na(RDO_mgL_6) & RDO_mgL_6 < 0)
                            | (! is.na(RDOsat_percent_6) & RDOsat_percent_6 < 0), 3, Flag_DO_6)) %>%
    mutate(RDO_mgL_6 = ifelse(RDO_mgL_6 < 0, 0, RDO_mgL_6)) %>%
    mutate(RDOsat_percent_6 = ifelse(RDOsat_percent_6 < 0, 0, RDOsat_percent_6))

  bvrdata <- bvrdata %>%
    mutate(Flag_DO_13 = ifelse((! is.na(RDO_mgL_13) & RDO_mgL_13 < 0)
                            | (! is.na(RDOsat_percent_13) & RDOsat_percent_13 < 0), 3, Flag_DO_13)) %>%
    mutate(RDO_mgL_13 = ifelse(RDO_mgL_13 < 0, 0, RDO_mgL_13)) %>%
    mutate(RDOsat_percent_13 = ifelse(RDOsat_percent_13 < 0, 0, RDOsat_percent_13))
  

  
   # modify bvrdata based on the information in the log
  for(i in 4:nrow(log)){
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]
    
    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:44), as.integer(log$colnumber[i]))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:44), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:44), c(bounds[1]:bounds[2]))
    }
    else
    {
      warning(paste("Could not parse column colnumber in row", i, "of the maintenance log. Skipping maintenance for",
        "that row. The value of colnumber should be in one of three formats: a single number (\"47\"), a",
        "semicolon-separated list of numbers in c() (\"c(47;48;49)\"), or a range of numbers in c() (\"c(47:74)\").",
        "Other values (even valid calls to c()) will not be parsed properly."))
      next
    }
    
    # remove EXO_Date and EXO_Time columns from the list of maintenance columns, because they will be deleted later
    maintenance_cols <- setdiff(maintenance_cols, c(24, 25))
    
    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
        "indices 2 through 39, excluding 21 and 22, which are deleted by this script. Skipping maintenance for that row."))
      next
    }
    
    # replace relevant data with NAs and set "all" flag while maintenance was in effect
    bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, maintenance_cols] <- "NA"
    bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, "Flag_All"] <- 1
    
    # if DO data was affected by maintenance, set the appropriate DO flags, and replace DO data with NAs after maintenance
    # was in effect until value returns to within a threshold of the value when maintenance began, because the sensors take
    # time to re-adjust to ambient conditions
    last_row_before_maintenance <- tail(bvrdata %>% filter(DateTime < start), 1)
    for(j in 1:3)
    {
      # if maintenance was not in effect on DO data, then skip
      if(! (DO_MGL_COLS[j] %in% maintenance_cols | DO_SAT_COLS[j] %in% maintenance_cols))
      {
        next
      }
      
      # set the appropriate DO flag while maintenance was in effect
      bvrdata[bvrdata$DateTime >= start & bvrdata$DateTime <= end, DO_FLAG_COLS[j]] <- 1
      
      last_DO_before_maintenance <- last_row_before_maintenance[[DO_MGL_COLS[j]]][1]
      if(is.na(last_DO_before_maintenance))
      {
        warning(paste("For row", i, "of the maintenance log, the pre-maintenance DO value at depth", DO_DEPTHS[j],
          "could not be found. Not replacing DO values after the end of maintenance. This could occur because the start",
          "date-time for maintenance is at or before the first date-time in the data, or simply because the value was",
          "missing or replaced in prior maintenance."))
      }
      else
      {
        DO_recovery_time <- (bvrdata %>%
                               filter(DateTime > end &
                                      abs(bvrdata[[DO_MGL_COLS[j]]] - last_DO_before_maintenance) <= DO_RECOVERY_THRESHOLD)
                            )$DateTime[1]
        
        # if the recovery time cannot be found, then raise a warning and replace post-maintenance DO values until the end of
        # the file
        if(is.na(DO_recovery_time))
        {
          warning(paste("For row", i, "of the maintenance log, post-maintenance DO levels at depth", DO_DEPTHS[j],
            "never returned within the given threshold of the pre-maintenance DO value. All post-maintenance DO values",
            "have been replaced with NA. This could occur because the end date-time for maintenance is at or after the",
            "last date-time in the data, or simply because post-maintenance levels never returned within the threshold."))
          bvrdata[bvrdata$DateTime > end, intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          bvrdata[bvrdata$DateTime > end, DO_FLAG_COLS[j]] <- 1
        }
        else
        {
          bvrdata[bvrdata$DateTime > end & bvrdata$DateTime < DO_recovery_time,
                  intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          bvrdata[bvrdata$DateTime > end & bvrdata$DateTime < DO_recovery_time, DO_FLAG_COLS[j]] <- 1
        }
      }
    }
  }
  
  # find EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
  
  # chl and phyco qaqc ----
  # perform qaqc on the entire dataset for chl and phyco
  
  # assign standard deviation thresholds
  sd_4 <- 4*sd(bvrdata$EXOChla_ugL_1_5, na.rm = TRUE)
  threshold <- sd_4
  sd_4_phyco <- 4*sd(bvrdata$EXOBGAPC_ugL_1_5, na.rm = TRUE)
  threshold_phyco <- sd_4_phyco 
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  bvrdata <- bvrdata %>% 
    mutate(Chla = lag(EXOChla_ugL_1_5, 0),
           Chla_lag1_5 = lag(EXOChla_ugL_1_5, 1),
           Chla_lead1_5 = lead(EXOChla_ugL_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_Chla = ifelse(Chla < 0 & !is.na(Chla), 3, Flag_Chla)) %>% 
    mutate(Flag_Chla = ifelse(Chla < 0 & !is.na(Chla), 3, Flag_Chla)) %>% 
    mutate(EXOChla_ugL_1_5 = ifelse(Chla < 0 & !is.na(Chla), 0, EXOChla_ugL_1_5)) %>% 
    mutate(EXOChla_RFU_1_5 = ifelse(Chla < 0 & !is.na(Chla), 0, EXOChla_RFU_1_5)) %>% 
    mutate(EXOChla_ugL_1_5 = ifelse((abs(Chla_lag1_5 - Chla) > (threshold))  & (abs(Chla_lead1_5 - Chla) > (threshold) & !is.na(Chla)), 
                                    NA, EXOChla_ugL_1_5)) %>%   
    mutate(EXOChla_RFU_1_5 = ifelse((abs(Chla_lag1_5 - Chla) > (threshold))  & (abs(Chla_lead1_5 - Chla) > (threshold) & !is.na(Chla)), 
                                    NA, EXOChla_RFU_1_5)) %>% 
    mutate(Flag_Chla = ifelse((abs(Chla_lag1_5 - Chla) > (threshold))  & (abs(Chla_lead1_5 - Chla) > (threshold)) & !is.na(Chla), 
                              2, Flag_Chla)) %>% 
    select(-Chla, -Chla_lag1_5, -Chla_lead1_5)
  
  # QAQC on major chl outliers using DWH's method: datapoint set to NA if data is greater than 4*sd different from both previous and following datapoint
  bvrdata <- bvrdata %>% 
    mutate(phyco = lag(EXOBGAPC_ugL_1_5, 0),
           phyco_lag1_5 = lag(EXOBGAPC_ugL_1_5, 1),
           phyco_lead1_5 = lead(EXOBGAPC_ugL_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_Phyco = ifelse(phyco < 0 & !is.na(phyco), 3, Flag_Phyco)) %>% 
    mutate(Flag_Phyco = ifelse(phyco < 0 & !is.na(phyco), 3, Flag_Phyco)) %>% 
    mutate(EXOBGAPC_RFU_1_5 = ifelse(phyco < 0 & !is.na(phyco), 0, EXOBGAPC_RFU_1_5)) %>% 
    mutate(EXOBGAPC_ugL_1_5 = ifelse(phyco < 0 & !is.na(phyco), 0, EXOBGAPC_ugL_1_5)) %>% 
    mutate(EXOBGAPC_ugL_1_5 = ifelse((abs(phyco_lag1_5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1_5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                                     NA, EXOBGAPC_ugL_1_5)) %>%   
    mutate(EXOBGAPC_RFU_1_5 = ifelse((abs(phyco_lag1_5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1_5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                                     NA, EXOBGAPC_RFU_1_5)) %>% 
    mutate(Flag_Phyco = ifelse((abs(phyco_lag1_5 - phyco) > (threshold_phyco))  & (abs(phyco_lead1_5 - phyco) > (threshold_phyco) & !is.na(phyco)), 
                               2, Flag_Phyco)) %>%
    select(-phyco, -phyco_lag1_5, -phyco_lead1_5)
  
  #deteriming the standard deviation of fDOM data 
  sd_fDOM <- sd(bvrdata$EXOfDOM_QSU_1_5, na.rm = TRUE) 
  bvrdata <- bvrdata %>% 
    mutate(fDOM = lag(EXOfDOM_QSU_1_5, 0),
           fDOM_lag1_5 = lag(EXOfDOM_QSU_1_5, 1),
           fDOM_lead1_5 = lead(EXOfDOM_QSU_1_5, 1)) %>%  #These mutates create columns for current fDOM, fDOM before and fDOM after. These are used to run ifelse QAQC loops
    mutate(Flag_fDOM = ifelse(fDOM < 0 & !is.na(fDOM), 3, Flag_fDOM),
           EXOfDOM_QSU_1_5 = ifelse(fDOM < 0 & !is.na(fDOM), NA, EXOfDOM_QSU_1_5),
           EXOfDOM_RFU_1_5 = ifelse(fDOM < 0 & !is.na(fDOM), NA, EXOfDOM_RFU_1_5),
           Flag_fDOM = ifelse(fDOM < 0, 2, Flag_fDOM)   ) %>% #These mutates are QAQCing for negative fDOM QSU values and setting these to NA and making a flag for these. This was done outside of the 2 sd deviation rule because there were two negative points in a row and one was not removed with the follwoing if else statements. 
    mutate(EXOfDOM_QSU_1_5 = ifelse(
      ( abs(fDOM_lag1_5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1_5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM) ), NA, EXOfDOM_QSU_1_5
    )) %>%  #QAQC to remove outliers for QSU fDOM data 
    mutate(EXOfDOM_RFU_1_5 = ifelse(
      ( abs(fDOM_lag1_5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1_5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM)  ), NA, EXOfDOM_RFU_1_5
    )) %>% #QAQC to remove outliers for RFU fDOM data
    mutate(Flag_fDOM = ifelse(
      ( abs(fDOM_lag1_5 - fDOM) > (2*sd_fDOM)   )  & ( abs(fDOM_lead1_5 - fDOM) > (2*sd_fDOM)  & !is.na(fDOM)  ), 2, Flag_fDOM
    ))  %>%  #QAQC to set flags for data that was set to NA after applying 2 S.D. QAQC 
    select(-fDOM, -fDOM_lag1_5, -fDOM_lead1_5)  #This removes the columns used to run ifelse statements since they are no longer needed. 
  
  #create depth column
  bvrdata=bvrdata%>%mutate(Depth_m_13=Lvl_psi_13*0.70455)#1psi=2.31ft, 1ft=0.305m
  
  # delete EXO_Date and EXO_Time columns
  bvrdata <- bvrdata %>% select(-EXO_Date, -EXO_Time)
  
  # add Reservoir and Site columns
  bvrdata$Reservoir="BVR"
  bvrdata$Site=50
    
 
  # reorder columns
  bvrdata <- bvrdata %>% select(Reservoir, Site, -RECORD, -CR6_Batt_V, -CR6Panel_Temp_C, -Flag_All, -Flag_DO_1_5, -Flag_DO_6,
                                -Flag_DO_13, -Flag_Chla, -Flag_Phyco, -Flag_TDS, everything())
  
  # replace NaNs with NAs
  bvrdata[is.na(bvrdata)] <- NA
  
  # convert datetimes to characters so that they are properly formatted in the output file
  bvrdata$DateTime <- as.character(bvrdata$DateTime)
 
 
  # write to output file
  write.csv(bvrdata, output_file, row.names = FALSE, quote=FALSE)
  
 
  
  ###Prep RemoveMet for final file version
  
    names(log)=c("Station", "DateTime_start","DateTime_end", "Parameter", "ColumnNumber", "Flag", "Notes")
    
    log=log%>%
    mutate(Reservoir="BVR")%>%
    mutate(Site=50)%>%
    mutate(Station="BVR_sensor_string")%>%
    select(Reservoir, Site, Station, DateTime_start, DateTime_end, Parameter, ColumnNumber, Flag, Notes)
  
  
  write.csv(log, output2_file, row.names = FALSE, quote=FALSE)
}

# example usage


#qaqc('https://github.com/FLARE-forecast/BVRE-data/raw/bvre-platform-data/BVRplatform.csv',
#      'https://raw.githubusercontent.com/CareyLabVT/ManualDownloadsSCCData/master/BVRplatform_manual_2020.csv',
#      "https://raw.githubusercontent.com/FLARE-forecast/BVRE-data/bvre-platform-data/BVR_maintenance_log.txt",
#       "BVRplatform_clean.csv", 
#     "BVR_Maintenance_2020.csv")

