
temp_oxy_chla_qaqc_bvre <- function(data_file,
                               maintenance_file,
                               output_file){

  CATDATA_COL_NAMES <- c("DateTime", "RECORD", "CR6_Batt_V", "CR6Panel_Temp_C", "ThermistorTemp_C_1",
                         "ThermistorTemp_C_2", "ThermistorTemp_C_3", "ThermistorTemp_C_4", "ThermistorTemp_C_5",
                         "ThermistorTemp_C_6", "ThermistorTemp_C_7", "ThermistorTemp_C_8", "ThermistorTemp_C_9",
                         "ThermistorTemp_C_10","ThermistorTemp_C_11","ThermistorTemp_C_12","ThermistorTemp_C_13",
                         "RDO_mgL_6", "RDOsat_percent_6", "RDOTemp_C_6", "RDO_mgL_13",
                         "RDOsat_percent_13", "RDOTemp_C_13", "EXO_Date", "EXO_Time", "EXOTemp_C_1_5", "EXOCond_uScm_1_5",
                         "EXOSpCond_uScm_1_5", "EXOTDS_mgL_1_5", "EXODOsat_percent_1_5", "EXODO_mgL_1_5", "EXOChla_RFU_1_5",
                         "EXOChla_ugL_1_5", "EXOBGAPC_RFU_1_5", "EXOBGAPC_ugL_1_5", "EXOfDOM_RFU_1_5", "EXOfDOM_QSU_1_5",
                         "EXO_pressure_1_5", "EXO_depth", "EXO_battery", "EXO_cablepower", "EXO_wiper", "Lvl_psi_13", "LvlTemp_C_13")

  # after maintenance, DO values will continue to be replaced by NA until DO_mgL returns within this threshold (in mg/L)
  # of the pre-maintenance value
  DO_RECOVERY_THRESHOLD <- 1

  # columns where certain values are stored
  DO_MGL_COLS <- c(31, 18, 21)
  DO_SAT_COLS <- c(30, 19, 22)
  DO_FLAG_COLS <- c(46,47,48) 
  
  # depths at which DO is measured
  DO_DEPTHS <- c(1.5, 6, 13) #these are essentially meaningless because of the drastic water level changes...

  # EXO sonde sensor data that differs from the mean by more than the standard deviation multiplied by this factor will
  # either be replaced with NA and flagged (if between 2018-10-01 and 2019-03-01) or just flagged (otherwise)
  EXO_FOULING_FACTOR <- 4

  # read catwalk data and maintenance log
  # NOTE: date-times throughout this script are processed as UTC
  catdata <- readr::read_csv(data_file, skip = 4, col_names = CATDATA_COL_NAMES, 
                      col_types = readr::cols(.default = readr::col_double(), DateTime = readr::col_datetime()))
  catdata <- catdata[!is.na(catdata$RECORD),]
  
  log <- readr::read_csv(maintenance_file, col_types = readr::cols(
    .default = readr::col_character(),
    TIMESTAMP_start = readr::col_datetime("%Y-%m-%d %H:%M:%S%*"),
    TIMESTAMP_end = readr::col_datetime("%Y-%m-%d %H:%M:%S%*"),
    flag = readr::col_integer()
  ))

  # remove NaN data at beginning
  #catdata <- catdata %>% dplyr::filter(DateTime > ymd_hms("2020-08-13 11:30:00"))

  # add flag columns
  catdata$Flag_All <- 0
  catdata$Flag_DO_1_5 <- 0
  catdata$Flag_DO_6 <- 0
  catdata$Flag_DO_13 <- 0
  catdata$Flag_Chla <- 0
  catdata$Flag_Phyco <- 0
  catdata$Flag_TDS <- 0
  catdata$Flag_fDOM <- 0
  catdata$Flag_Cond <-0
  catdata$Flag_Lvl_13 <-0
  catdata$Flag_Temp_1 <-0
  catdata$Flag_Temp_2 <-0
  catdata$Flag_Temp_3 <-0
  catdata$Flag_Temp_4 <-0
  catdata$Flag_Temp_5 <-0
  catdata$Flag_Temp_6 <-0
  catdata$Flag_Temp_7 <-0
  catdata$Flag_Temp_8 <-0
  catdata$Flag_Temp_9 <-0
  catdata$Flag_Temp_10 <-0
  catdata$Flag_Temp_11 <-0
  catdata$Flag_Temp_12 <-0
  catdata$Flag_Temp_13 <-0
  
  # replace negative DO values with 0
  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_1_5 = ifelse((! is.na(EXODO_mgL_1_5) & EXODO_mgL_1_5 < 0)
                              | (! is.na(EXODOsat_percent_1_5) & EXODOsat_percent_1_5 < 0), 3, Flag_DO_1_5)) %>%
    dplyr::mutate(EXODO_mgL_1_5 = ifelse(EXODO_mgL_1_5 < 0, 0, EXODO_mgL_1_5)) %>%
    dplyr::mutate(EXODOsat_percent_1_5 = ifelse(EXODOsat_percent_1_5 <0, 0, EXODOsat_percent_1_5))

  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_6 = ifelse((! is.na(RDO_mgL_6) & RDO_mgL_6 < 0)
                              | (! is.na(RDOsat_percent_6) & RDOsat_percent_6 < 0), 3, Flag_DO_6)) %>%
    dplyr::mutate(RDO_mgL_6 = ifelse(RDO_mgL_6 < 0, 0, RDO_mgL_6)) %>%
    dplyr::mutate(RDOsat_percent_6 = ifelse(RDOsat_percent_6 < 0, 0, RDOsat_percent_6))

  catdata <- catdata %>%
    dplyr::mutate(Flag_DO_13 = ifelse((! is.na(RDO_mgL_13) & RDO_mgL_13 < 0)
                              | (! is.na(RDOsat_percent_13) & RDOsat_percent_13 < 0), 3, Flag_DO_13)) %>%
    dplyr::mutate(RDO_mgL_13 = ifelse(RDO_mgL_13 < 0, 0, RDO_mgL_13)) %>%
    dplyr::mutate(RDOsat_percent_13 = ifelse(RDOsat_percent_13 < 0, 0, RDOsat_percent_13))

  # modify catdata based on the information in the log
  for(i in 1:nrow(log)){
    # get start and end time of one maintenance event
    start <- log$TIMESTAMP_start[i]
    end <- log$TIMESTAMP_end[i]

    # get indices of columns affected by maintenance
    if(grepl("^\\d+$", log$colnumber[i])) # single num
    {
      maintenance_cols <- intersect(c(2:39), as.integer(log$colnumber[i]))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*(;\\s*\\d+\\s*)*\\)$", log$colnumber[i])) # c(x;y;...)
    {
      maintenance_cols <- intersect(c(2:39), as.integer(unlist(regmatches(log$colnumber[i],
                                                                          gregexpr("\\d+", log$colnumber[i])))))
    }
    else if(grepl("^c\\(\\s*\\d+\\s*:\\s*\\d+\\s*\\)$", log$colnumber[i])) # c(x:y)
    {
      bounds <- as.integer(unlist(regmatches(log$colnumber[i], gregexpr("\\d+", log$colnumber[i]))))
      maintenance_cols <- intersect(c(2:39), c(bounds[1]:bounds[2]))
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
    maintenance_cols <- setdiff(maintenance_cols, c(21, 22))

    if(length(maintenance_cols) == 0)
    {
      warning(paste("Did not parse any valid data columns in row", i, "of the maintenance log. Valid columns have",
                    "indices 2 through 39, excluding 21 and 22, which are deleted by this script. Skipping maintenance for that row."))
      next
    }

    # replace relevant data with NAs and set "all" flag while maintenance was in effect
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, maintenance_cols] <- NA
    catdata[catdata$DateTime >= start & catdata$DateTime <= end, "Flag_All"] <- 1

    # if DO data was affected by maintenance, set the appropriate DO flags, and replace DO data with NAs after maintenance
    # was in effect until value returns to within a threshold of the value when maintenance began, because the sensors take
    # time to re-adjust to ambient conditions
    last_row_before_maintenance <- tail(catdata %>% filter(DateTime < start), 1)
    for(j in 1:3)
    {
      # if maintenance was not in effect on DO data, then skip
      if(! (DO_MGL_COLS[j] %in% maintenance_cols | DO_SAT_COLS[j] %in% maintenance_cols))
      {
        next
      }

      # set the appropriate DO flag while maintenance was in effect
      catdata[catdata$DateTime >= start & catdata$DateTime <= end, DO_FLAG_COLS[j]] <- 1

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
        DO_recovery_time <- (catdata %>%
                               dplyr::filter(DateTime > end &
                                        abs(catdata[[DO_MGL_COLS[j]]] - last_DO_before_maintenance) <= DO_RECOVERY_THRESHOLD)
        )$DateTime[1]

        # if the recovery time cannot be found, then raise a warning and replace post-maintenance DO values until the end of
        # the file
        if(is.na(DO_recovery_time))
        {
          warning(paste("For row", i, "of the maintenance log, post-maintenance DO levels at depth", DO_DEPTHS[j],
                        "never returned within the given threshold of the pre-maintenance DO value. All post-maintenance DO values",
                        "have been replaced with NA. This could occur because the end date-time for maintenance is at or after the",
                        "last date-time in the data, or simply because post-maintenance levels never returned within the threshold."))
          catdata[catdata$DateTime > end, intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          catdata[catdata$DateTime > end, DO_FLAG_COLS[j]] <- 1
        }
        else
        {
          catdata[catdata$DateTime > end & catdata$DateTime < DO_recovery_time,
                  intersect(maintenance_cols, c(DO_MGL_COLS[j], DO_SAT_COLS[j]))] <- NA
          catdata[catdata$DateTime > end & catdata$DateTime < DO_recovery_time, DO_FLAG_COLS[j]] <- 1
        }
      }
    }
  }

  # find EXO sonde sensor data that differs from the mean by more than the standard deviation times a given factor, and
  # replace with NAs between October and March, due to sensor fouling
  Chla_RFU_1_5_mean <- mean(catdata$EXOChla_RFU_1_5, na.rm = TRUE)
  Chla_ugL_1_5_mean <- mean(catdata$EXOChla_ugL_1_5, na.rm = TRUE)
  BGAPC_RFU_1_5_mean <- mean(catdata$EXOBGAPC_RFU_1_5, na.rm = TRUE)
  BGAPC_ugL_1_5_mean <- mean(catdata$EXOBGAPC_ugL_1_5, na.rm = TRUE)
  Chla_RFU_1_5_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOChla_RFU_1_5, na.rm = TRUE)
  Chla_ugL_1_5_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOChla_ugL_1_5, na.rm = TRUE)
  BGAPC_RFU_1_5_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOBGAPC_RFU_1_5, na.rm = TRUE)
  BGAPC_ugL_1_5_threshold <- EXO_FOULING_FACTOR * sd(catdata$EXOBGAPC_ugL_1_5, na.rm = TRUE)

  catdata <- catdata %>%
    dplyr::mutate(Flag_Chla = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                (! is.na(EXOChla_RFU_1_5) & abs(EXOChla_RFU_1_5 - Chla_RFU_1_5_mean) > Chla_RFU_1_5_threshold |
                                   ! is.na(EXOChla_ugL_1_5) & abs(EXOChla_ugL_1_5 - Chla_ugL_1_5_mean) > Chla_ugL_1_5_threshold),
                              4, Flag_Chla)) %>%
    dplyr::mutate(Flag_Phyco = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                 (! is.na(EXOBGAPC_RFU_1_5) & abs(EXOBGAPC_RFU_1_5 - BGAPC_RFU_1_5_mean) > BGAPC_RFU_1_5_threshold |
                                    ! is.na(EXOBGAPC_ugL_1_5) & abs(EXOBGAPC_ugL_1_5 - BGAPC_ugL_1_5_mean) > BGAPC_ugL_1_5_threshold),
                               4, Flag_Phyco)) %>%
    dplyr::mutate(EXOChla_RFU_1_5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                    abs(EXOChla_RFU_1_5 - Chla_RFU_1_5_mean) > Chla_RFU_1_5_threshold, NA, EXOChla_RFU_1_5)) %>%
    dplyr::mutate(EXOChla_ugL_1_5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                    abs(EXOChla_ugL_1_5 - Chla_ugL_1_5_mean) > Chla_ugL_1_5_threshold, NA, EXOChla_ugL_1_5)) %>%
    dplyr::mutate(EXOBGAPC_RFU_1_5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                     abs(EXOBGAPC_RFU_1_5 - BGAPC_RFU_1_5_mean) > BGAPC_RFU_1_5_threshold, NA, EXOBGAPC_RFU_1_5)) %>%
    dplyr::mutate(EXOBGAPC_ugL_1_5 = ifelse(DateTime >= ymd("2020-10-01") & DateTime < ymd("2021-03-01") &
                                     abs(EXOBGAPC_ugL_1_5 - BGAPC_ugL_1_5_mean) > BGAPC_ugL_1_5_threshold, NA, EXOBGAPC_ugL_1_5))

  # flag EXO sonde sensor data of value above 4 * standard deviation at other times
  catdata <- catdata %>%
    dplyr::mutate(Flag_Phyco = ifelse(! is.na(EXOBGAPC_RFU_1_5) & abs(EXOBGAPC_RFU_1_5 - BGAPC_RFU_1_5_mean) > BGAPC_RFU_1_5_threshold |
                                 ! is.na(EXOBGAPC_ugL_1_5) & abs(EXOBGAPC_ugL_1_5 - BGAPC_ugL_1_5_mean) > BGAPC_ugL_1_5_threshold,
                               5, Flag_Phyco))

  #create depth column
  catdata=catdata%>%mutate(Depth_m_13=Lvl_psi_13*0.70455)#1psi=2.31ft, 1ft=0.305m
  #note that there are a decent amount of negative depths at 13m so unless BVR drained, this isn't the most robust method w/o additional QAQC
  
  # temp qaqc ----
  
  #Setting the temperature to NA when the thermistors are out of the water
  #add a depth column which we set NAs for when the thermistor is out of the water. Flag 2
  catdata=catdata%>%
    mutate(depth_1=Depth_m_13-11.82)%>%
    mutate(depth_2=Depth_m_13-11.478)
  
  
  #change the temp to NA when the thermistor is clearly out of the water which we used to determine the depth of the temp string
  #negative depths are changed to NA
  #the date ifelse statement is when the pressure transducer was unplugged
  
  #for thermistor at position 1 when it was out of the water 
  catdata=catdata%>%
    mutate(Flag_Temp_1= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",2,Flag_Temp_1))%>%
    mutate(ThermistorTemp_C_1= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",NA,ThermistorTemp_C_1))%>%
    mutate(Flag_Temp_1= ifelse(!is.na(depth_1) & depth_1<0 ,2,Flag_Temp_1))%>%
    mutate(ThermistorTemp_C_1=ifelse(!is.na(depth_1) & depth_1<0,NA,ThermistorTemp_C_1))
  
  
  #for thermistor at position 2 when it was out of the water 
  catdata=catdata%>%
    mutate(Flag_Temp_2= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",2,Flag_Temp_2))%>%#this is when the pressure sensor was unplugged
    mutate(ThermistorTemp_C_2= ifelse(DateTime>="2020-10-26 12:10:00 tz=Etc/GMT+5 "&DateTime<="2020-10-30 09:40:00 tz=Etc/GMT+5",NA,ThermistorTemp_C_2))%>%
    mutate(Flag_Temp_2= ifelse(!is.na(depth_2) & depth_2<0 ,2,Flag_Temp_2))%>%
    mutate(ThermistorTemp_C_2=ifelse(!is.na(depth_2) & depth_2<0,NA,ThermistorTemp_C_2))
  
  
  #take out the depth columns for thermisotrs 1 and 2 after you set the values to NA
  catdata=catdata%>%
    dplyr::select(-depth_1,-depth_2)
  
  catdata=catdata%>%
    mutate(Flag_Temp_11=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_11))%>%
    mutate(Flag_Temp_12=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_12))%>%
    mutate(Flag_Temp_13=ifelse(DateTime<"2020-10-05 12:05:00 tz=Etc/GMT+5", 7, Flag_Temp_13))
  
  #add flags and set to NA 
  catdata=catdata%>%
    mutate(Flag_Phyco= ifelse(DateTime=="2020-08-01 12:50:00 tz=Etc/GMT+5", 5, Flag_Phyco))%>%
    mutate(Flag_Phyco= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", 2, Flag_Phyco))%>%
    mutate(Flag_Phyco= ifelse(DateTime=="2020-12-25 06:20:00 tz=Etc/GMT+5", 2, Flag_Phyco))%>%
    mutate(EXOBGAPC_RFU_1_5= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", NA, EXOBGAPC_RFU_1_5))%>%
    mutate(EXOBGAPC_ugL_1_5= ifelse(DateTime>="2020-10-29 10:10:00 tz=Etc/GMT+5" &DateTime<="2020-10-29 12:00:00 tz=Etc/GMT+5", NA, EXOBGAPC_ugL_1_5))%>%
    mutate(EXOBGAPC_RFU_1_5= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5",NA, EXOBGAPC_RFU_1_5))%>%
    mutate(EXOBGAPC_ugL_1_5= ifelse(DateTime=="2020-12-28 03:20:00 tz=Etc/GMT+5", NA, EXOBGAPC_ugL_1_5))
  
  #Add flags when the wire was not connected
  catdata=catdata%>%
    mutate(Flag_Lvl_13=ifelse(is.na(Lvl_psi_13)& DateTime>="2020-10-26 12:00:00" & DateTime<="2020-10-30 09:40:00",
                              7, Flag_Lvl_13))
  
  # reorder columns
  catdata <- catdata %>% dplyr::select(-RECORD, -CR6_Batt_V, -CR6Panel_Temp_C, -Flag_All, -Flag_DO_1_5, -Flag_DO_6,
                                       -Flag_DO_13, -Flag_Chla, -Flag_Phyco, -Flag_TDS, -EXO_wiper, -EXO_cablepower,
                                       -EXO_battery,-EXO_pressure_1_5)
  
  # replace NaNs with NAs
  catdata[is.na(catdata)] <- NA
  
  # convert datetimes to characters so that they are properly formatted in the output file
  catdata$DateTime <- as.Date(catdata$DateTime)
  
  # subset to just what is needed for fcr forecasts
  catdata <- catdata %>% dplyr::select(DateTime, EXOChla_ugL_1_5)
  colnames(catdata) <- c('DateTime', 'EXOChla_ugL_1') # rename so it matches the FCR column for taking daily average
  
  # write to output file
  write_csv(catdata, output_file)
}


