# Script to rename forecast output files to denote forecast model timestep and uncertainty mode and move to a combined folder
# in prep for EDI publishing
# create the following folders:

#  forecast_output_EDI
#    YYYY_MM_DD_chla_Xday_uncertY.csv
#    Where X = the numeric timestep (e.g., 1, 7, or 14)
#    Where Y = the uncertainty mode (e.g., 1-6)
#  parameter_output_EDI
#    YYYY_MM_DD_ensemble_parameters_Xday_uncertY.csv
#    Where X = the numeric timestep (e.g., 1, 7, or 14)
#    Where Y = the uncertainty mode (e.g., 1-6)
#  null_model_EDI
#    YYYY_MM_DD_null_summary_Xday.csv
#    Where X = the numeric timestep (e.g., 1, 7, 14)


#####################################################################################################
# forecast output files
new_folder <- './FCR_forecasts/forecast_output_EDI'
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020'
dir.create(new_folder)
names <- list.files(path = old_folder, pattern = '*chla_1day.csv')
newNames <- sub("chla_1day.csv", "chla_1day_uncert1.csv", names)

file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 1day files in each of the uncertainty folders, uncertainty_2_process
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020/uncertainty_2_process'
names <- list.files(path = old_folder, pattern = '*chla_1day.csv')
newNames <- sub("chla_1day.csv", "chla_1day_uncert2.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 1day files in each of the uncertainty folders, uncertainty_3_weather
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020/uncertainty_3_weather'
names <- list.files(path = old_folder, pattern = '*chla_1day.csv')
newNames <- sub("chla_1day.csv", "chla_1day_uncert3.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 1day files in each of the uncertainty folders, uncertainty_4_initial_condition
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020/uncertainty_4_initial_condition'
names <- list.files(path = old_folder, pattern = '*chla_1day.csv')
newNames <- sub("chla_1day.csv", "chla_1day_uncert4.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 1day files in each of the uncertainty folders, uncertainty_5_parameter
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020/uncertainty_5_parameter'
names <- list.files(path = old_folder, pattern = '*chla_1day.csv')
newNames <- sub("chla_1day.csv", "chla_1day_uncert5.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 1day files in each of the uncertainty folders, uncertainty_6_discharge
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020/uncertainty_6_discharge'
names <- list.files(path = old_folder, pattern = '*chla_1day.csv')
newNames <- sub("chla_1day.csv", "chla_1day_uncert6.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

##############################
# do again for 7day forecasts
old_folder <- './FCR_forecasts/7day/update_bayes_method_Oct_2020'
names <- list.files(path = old_folder, pattern = '*chla_7day.csv')
newNames <- sub("chla_7day.csv", "chla_7day_uncert1.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 7day files in each of the uncertainty folders, uncertainty_2_process
old_folder <- './FCR_forecasts/7day/update_bayes_method_Oct_2020/uncertainty_2_process'
names <- list.files(path = old_folder, pattern = '*chla_7day.csv')
newNames <- sub("chla_7day.csv", "chla_7day_uncert2.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 7day files in each of the uncertainty folders, uncertainty_3_weather
old_folder <- './FCR_forecasts/7day/update_bayes_method_Oct_2020/uncertainty_3_weather'
names <- list.files(path = old_folder, pattern = '*chla_7day.csv')
newNames <- sub("chla_7day.csv", "chla_7day_uncert3.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 7day files in each of the uncertainty folders, uncertainty_4_initial_condition
old_folder <- './FCR_forecasts/7day/update_bayes_method_Oct_2020/uncertainty_4_initial_condition'
names <- list.files(path = old_folder, pattern = '*chla_7day.csv')
newNames <- sub("chla_7day.csv", "chla_7day_uncert4.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 7day files in each of the uncertainty folders, uncertainty_5_parameter
old_folder <- './FCR_forecasts/7day/update_bayes_method_Oct_2020/uncertainty_5_parameter'
names <- list.files(path = old_folder, pattern = '*chla_7day.csv')
newNames <- sub("chla_7day.csv", "chla_7day_uncert5.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 7day files in each of the uncertainty folders, uncertainty_6_discharge
old_folder <- './FCR_forecasts/7day/update_bayes_method_Oct_2020/uncertainty_6_discharge'
names <- list.files(path = old_folder, pattern = '*chla_7day.csv')
newNames <- sub("chla_7day.csv", "chla_7day_uncert6.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for the 14day forecasts
old_folder <- './FCR_forecasts/14day/update_bayes_method_Oct_2020'
names <- list.files(path = old_folder, pattern = '*chla_14day.csv')
newNames <- sub("chla_14day.csv", "chla_14day_uncert1.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 14day files in each of the uncertainty folders, uncertainty_2_process
old_folder <- './FCR_forecasts/14day/update_bayes_method_Oct_2020/uncertainty_2_process'
names <- list.files(path = old_folder, pattern = '*chla_14day.csv')
newNames <- sub("chla_14day.csv", "chla_14day_uncert2.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 14day files in each of the uncertainty folders, uncertainty_3_weather
old_folder <- './FCR_forecasts/14day/update_bayes_method_Oct_2020/uncertainty_3_weather'
names <- list.files(path = old_folder, pattern = '*chla_14day.csv')
newNames <- sub("chla_14day.csv", "chla_14day_uncert3.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 14day files in each of the uncertainty folders, uncertainty_4_initial_condition
old_folder <- './FCR_forecasts/14day/update_bayes_method_Oct_2020/uncertainty_4_initial_condition'
names <- list.files(path = old_folder, pattern = '*chla_14day.csv')
newNames <- sub("chla_14day.csv", "chla_14day_uncert4.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 14day files in each of the uncertainty folders, uncertainty_5_parameter
old_folder <- './FCR_forecasts/14day/update_bayes_method_Oct_2020/uncertainty_5_parameter'
names <- list.files(path = old_folder, pattern = '*chla_14day.csv')
newNames <- sub("chla_14day.csv", "chla_14day_uncert5.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 14day files in each of the uncertainty folders, uncertainty_6_discharge
old_folder <- './FCR_forecasts/14day/update_bayes_method_Oct_2020/uncertainty_6_discharge'
names <- list.files(path = old_folder, pattern = '*chla_14day.csv')
newNames <- sub("chla_14day.csv", "chla_14day_uncert6.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

#####################################################################################################
# parameter output files

new_folder <- './FCR_forecasts/parameter_output_EDI'
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020'
dir.create(new_folder)
names <- list.files(path = old_folder, pattern = '*parameters.csv')
newNames <- sub("parameters.csv", "parameters_1day_uncert1.csv", names)

file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 7day parameter output
old_folder <- './FCR_forecasts/7day/update_bayes_method_Oct_2020'
names <- list.files(path = old_folder, pattern = '*parameters.csv')
newNames <- sub("parameters.csv", "parameters_7day_uncert1.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 14day parameter output
old_folder <- './FCR_forecasts/14day/update_bayes_method_Oct_2020'
names <- list.files(path = old_folder, pattern = '*parameters.csv')
newNames <- sub("parameters.csv", "parameters_14day_uncert1.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# DO NOT NEED TO DO SAVE PARAMETER OUTPUT FOR UNCERTAINTY MODES 2-6

###########################################################################################################
# null model files
new_folder <- './FCR_forecasts/null_model_EDI'
old_folder <- './FCR_forecasts/1day/null_daily'
dir.create(new_folder)
names <- list.files(path = old_folder, pattern = '*null_summary.csv')
newNames <- sub("null_summary.csv", "null_summary_1day.csv", names)

file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 7day null
old_folder <- './FCR_forecasts/7day/null_weekly'
names <- list.files(path = old_folder, pattern = '*null_summary.csv')
newNames <- sub("null_summary.csv", "null_summary_7day.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 14day null
old_folder <- './FCR_forecasts/14day/null_fortnightly'
names <- list.files(path = old_folder, pattern = '*null_summary.csv')
newNames <- sub("null_summary.csv", "null_summary_14day.csv", names)
file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))
