# Script to rename forecast output files to denote forecast model timestep and uncertainty mode and move to a combined folder
# in prep for EDI publishing

#####################################################################################################
# forecast output files
new_folder <- './FCR_forecasts/forecast_output_folder_EDI'
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020'
dir.create(new_folder)
names <- list.files(path = old_folder, pattern = '*chla_1day.csv')
newNames <- sub("chla_1day.csv", "chla_1day_uncert1.csv", names)

file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))

# do again for 1day files in each of the uncertainty folders, 2:6

# do again for 7day forecasts

# do again for 7day forecasts in each of the uncertainty folder, 2:6

# do again for the 14day forecasts

# do again for the 14day forecasts in each of the uncertainty folders, 2:6

#####################################################################################################
# parameter out files

new_folder <- './FCR_forecasts/parameter_output_folder_EDI'
old_folder <- './FCR_forecasts/1day/update_bayes_method_Oct_2020'
dir.create(new_folder)
names <- list.files(path = old_folder, pattern = '*parameters.csv')
newNames <- sub("parameters.csv", "parameters_1day_uncert1.csv", names)


file.copy(from = paste0(old_folder, "/", names), to = paste0(new_folder, "/", newNames))


