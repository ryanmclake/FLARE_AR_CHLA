##MakeEML FCR Chl Forecasts
##Author: Whitney Woelmer, modified from script by Mary Lofton
##Date: 17Apr21

#good site for step-by-step instructions
#https://ediorg.github.io/EMLassemblyline/articles/overview.html
#and links therein

#load packages
library(tidyverse)

# (install and) Load EMLassemblyline #####
# install.packages('devtools')

#remotes::install_github("EDIorg/EMLassemblyline", ref = "development") # use this for fix on having a col with the same entry for all rows
#devtools::install_github("EDIorg/EMLassemblyline")
#note that EMLassemblyline has an absurd number of dependencies and you
#may exceed your API rate limit; if this happens, you will have to wait an
#hour and try again or get a personal authentification token (?? I think)
#for github which allows you to submit more than 60 API requests in an hour
library(EMLassemblyline)


#Step 1: Create a directory for your dataset
eml_folder <- './FCR_forecasts/MakeEML'
dir.create(eml_folder)

#Step 2: Move your dataset to the directory
# in this case we have three example files and three zipped folders
# '2019_01_02_chla_1day_uncert1.csv' and zipped folder 'forecast_output_EDI'
# '2019_01_02_ensemble_parameters_1day_uncert1.csv' and zipped folder 'parameter_output_EDI'
# '2019_01_02_null_summary_1day.csv' and zipped folder 'null_model_EDI'

# read in files to check their structure
fc <- read.csv(paste0(eml_folder, '/2019_01_02_chla_1day_uncert1.csv'))
str(fc)
# because the EML functions do not like when columns have the same value for every row, we will delete those rows
# and make sure to note in the metadata that they were removed
#fc <- fc %>% select(-forecast_run_day, -(par1:par5))
#write.csv(fc, paste0(eml_folder, '/2019_01_02_chla_1day_uncert1.csv'), row.names = FALSE)

par <- read.csv(paste0(eml_folder, '/2019_01_02_ensemble_parameters_1day_uncert1.csv'))
str(par)

null <- read.csv(paste0(eml_folder, '/2019_01_02_null_summary_1day.csv'))
str(null)

#Step 3: Identify an intellectual rights license
#ours is CCBY

#Step 4: Identify the types of data in your dataset
#we have three tables and three zip files

#Step 5: Import the core metadata templates

#for our application, we will need to generate all types of metadata
#files except for taxonomic coverage, as we have both continuous and
#categorical variables and want to report our geographic location

# View documentation for these functions
?template_core_metadata
?template_table_attributes
?template_geographic_coverage
?template_taxonomic_coverage

# Import templates for our dataset licensed under CCBY, with 3 tables.
template_core_metadata(path = eml_folder,
                 license = "CCBY",
                 file.type = ".txt",
                 write.file = TRUE)

template_table_attributes(path = eml_folder,
                       data.path = eml_folder,
                       data.table = "2019_01_02_ensemble_parameters_1day_uncert1.csv",
                       write.file = TRUE)

template_table_attributes(path = eml_folder,
                          data.path = eml_folder,
                          data.table = "2019_01_02_null_summary_1day.csv",
                          write.file = TRUE)

template_table_attributes(path = eml_folder,
                          data.path = eml_folder,
                          data.table = "2019_01_02_chla_1day_uncert1.csv",
                          write.file = TRUE)



#we want empty to be true for this because we don't include lat/long
#as columns within our dataset but would like to provide them
#only have to do this once even if have multiple data tables
template_geographic_coverage(path = eml_folder,
                          data.path = eml_folder,
                          data.table = "2019_01_02_chla_1day_uncert1.csv",
                          empty = TRUE,
                          write.file = TRUE)



#Step 6: Script your workflow
#that's what this is, silly!

#Step 7: Abstract
#edit abstract.txt directly in Rstudio 
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 8: Methods
#copy-paste the methods from your Microsoft Word document into methods.txt
#if you want to check your abstract for non-allowed characters, go to:
#https://pteo.paranoiaworks.mobi/diacriticsremover/
#paste text and click remove diacritics

#Step 9: Additional information

#Step 10: Keywords
#DO NOT EDIT KEYWORDS FILE USING A TEXT EDITOR!! USE EXCEL!!
#good sources for keyword thesauri:
#lter controlled vocabulary
#cuahsi controlled vocabulary

#Step 11: Personnel
#copy-paste this information in from your metadata document
#Typically, the lead PI needs to be listed several times; that person has to be listed separately for his/her roles as
#PI, creator, and contact, and also separately for each separate funding source (!!)

#Step 12: Attributes
#you will need to edit the attributes files for each data table you are publishing
#grab attribute names and definitions from your metadata Google doc
#for units....
# View and search the standard units dictionary
view_unit_dictionary()

#Step 13: Custom Units
#if units are not in the unit dictionary, you need to fill them out in the
#custom_units.txt file


#Step 14: Close files
#if all your files aren't closed, sometimes functions don't work

#Step 15: Categorical variables
#we don't have any - hooray! :-)

#Step 16: Geographic coverage
#fill in the text file with the appropriate bounding box for your site
#we added both a bounding box for Lake Sunapee as well as
#point coordinates for the South Herrick Cove sampling site

#Step 17: Taxonomic coverage
#again, got this file from Bethel so no need to edit here - thx, Bethel! :-)

## Step 18: Obtain a package.id. ####
# Go to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using your username and password.

# Select Tools --> Data Package Identifier Reservations and click
# "Reserve Next Available Identifier"
# A new value will appear in the "Current data package identifier reservations"
# table (e.g., edi.123)
# Make note of this value, as it will be your package.id below

#Step 19: Make EML
# View documentation for this function
?make_eml

# Run this function
make_eml(
  path = eml_folder,
  data.path = eml_folder,
  eml.path = eml_folder,
  dataset.title = "Forecasts of Chlorophyll-a (ug/L) at Falling Creek Reservoir over 2019 and 2020 using three models with different timesteps and a null persistence model, including parameter values for all forecasts",
  temporal.coverage = c("2019-01-02", "2020-08-15"), #needs edited!!
  maintenance.description = 'completed', #completed if we don't ever plan on updating this
  data.table = c("2019_01_02_chla_1day_uncert1.csv","2019_01_02_ensemble_parameters_1day_uncert1.csv", "2019_01_02_null_summary_1day.csv"),
  data.table.name = c("Example forecast output","Example parameter output", "Example null forecast output"),
  data.table.description = c("Example forecast output","Example parameter output", "Example null forecast output"),
  other.entity = c("forecast_output_EDI.zip", "parameter_output_EDI.zip", "null_model_EDI.zip"),
  other.entity.name = c("FCR forecast files", "FCR parameter output files", "FCR null model files"),
  other.entity.description = c("FCR forecast files", "FCR parameter output files", "FCR null model files"),
  user.id = 'ccarey', 
  user.domain = 'EDI',
  package.id = 'edi.199.2') #will need to change this

## Step 20: Check your data product! ####
# Return to the EDI staging environment (https://portal-s.edirepository.org/nis/home.jsp),
# then login using your username and password

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File",
# choose your metadata (.xml) file (e.g., edi.270.1.xml), check "I want to
# manually upload the data by selecting files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder),
# then click Upload. Files will upload and your EML metadata will be checked
# for errors. If there are no errors, your data product is now published!
# If there were errors, click the link to see what they were, then fix errors
# in the xml file.
# Note that each revision results in the xml file increasing one value
# (e.g., edi.270.1, edi.270.2, etc). Re-upload your fixed files to complete the
# evaluation check again, until you receive a message with no errors.

## Step 21: PUBLISH YOUR DATA! ####
# Reserve a package.id for your error-free data package.
# NEVER ASSIGN this identifier to a staging environment package.
# Go to the EDI Production environment (https://portal.edirepository.org/nis/home.jsp)
# and login using your permanent credentials.

# Select Tools --> Data Package Identifier Reservations and click "Reserve Next
# Available Identifier". A new value will appear in the "Current data package
# identifier reservations" table (e.g., edi.518)
# This will be your PUBLISHED package.id

# In the make_eml command below, change the package.id to match your
# PUBLISHED package id. This id should end in .1 (e.g., edi.518.1)

# ALL OTHER entries in the make_eml() command should match what you ran above,
# in step 19

make_eml(
  path = "C:/Users/Mary Lofton/Documents/RProjects/IGC_Stroubles/MakeEMLBridge1",
  data.path = "C:/Users/Mary Lofton/Documents/RProjects/IGC_Stroubles/MakeEMLBridge1",
  eml.path = "C:/Users/Mary Lofton/Documents/RProjects/IGC_Stroubles/MakeEMLBridge1",
  dataset.title = "High-frequency time series of stage height, stream discharge, and water quality (specific conductivity, dissolved oxygen, pH, temperature, turbidity) for Stroubles Creek in Blacksburg, Virginia, USA 2013-2018",
  temporal.coverage = c("2013-01-01", "2018-12-31"), #needs edited!!
  maintenance.description = 'ongoing', #or completed if we don't ever plan on updating this
  data.table = c("stage.csv","WQ.csv"),
  data.table.name = c("Stage height and stream discharge data","Water quality data"),
  data.table.description = c("Stroubles Creek StREAM Lab Bridge 1 Stage Height and Stream Discharge Dataset 2013-2018","Stroubles Creek StREAM Lab Bridge 1 Water Quality Dataset 2013-2018"),
  user.id = 'melofton', #you need credentials for this part - not cool to use someone else's w/o permission! ask Mary Lofton what to do or email EDI
  user.domain = 'EDI',
  package.id = 'edi.510.1') #will need to change this

# Once your xml file with your PUBLISHED package.id is Done, return to the
# EDI Production environment (https://portal.edirepository.org/nis/home.jsp)

# Select Tools --> Preview Your Metadata, then upload your metadata (.xml) file
# associated with your PUBLISHED package.id. Look through the rendered
# metadata one more time to check for mistakes (author order, bounding box, etc.)

# Select Tools --> Evaluate/Upload Data Packages, then under "EML Metadata File",
# choose your metadata (.xml) file associated with your PUBLISHED package.id
# (e.g., edi.518.1.xml), check "I want to manually upload the data by selecting
# files on my local system", then click Upload.

# Now, Choose File for each file within the data package (e.g., each zip folder),
# then click Upload. Files will upload and your EML metadata will be checked for
# errors. Since you checked for and fixed errors in the staging environment, this
# should run without errors, and your data product is now published!

# Click the package.id hyperlink to view your final product! HOORAY!
# Have a cookie or a beer, whichever you feel is more appropriate. :-)
