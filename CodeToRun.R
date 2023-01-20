# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
renv::activate()
renv::restore() 

# Load packages ------
library(SqlRender)
library(DatabaseConnector)
library(FeatureExtraction) 
library(here)
library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(dbplyr)
library(dplyr)
library(tidyr)
library(kableExtra)
library(RSQLite) 
library(rmarkdown)
library(tableone) 
library(scales)
library(forcats) 
library(RPostgres)
library(cmprsk) 
library(mstate)
library(broom) 
library(rms)
library(glue) 
library(remotes)
library(readr)
library(log4r) 
library(survival)
library(flexsurv)
library(tictoc)
library(purrr)
library(CirceR)
library(CohortGenerator)
library(survminer)
library(openxlsx)
library(bshazard)
library(tibble)
library(CDMConnector)

# Set the name/ acronym for your database (to be used in the titles of reports, etc) -----
db.name<-"..."

# Set output folder locations -----
# the path to a folder where the results from this analysis will be saved
# to set the location within the project with folder called "CPRD", we can use: here("CPRD")
# but this file path could be set to somewhere else
output.folder<-here("Results",db.name)

# database connection details
server     <- "..."
server_dbi <- "..."
user       <- "..."
password   <- "..."
port       <- "..."
host       <- "..."


# schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"..."

# schema where a results table will be created 
results_database_schema<-"..."

# stem for tables to be created in your results schema for this analysis
# You can keep the above names or change them
# Note, any existing tables in your results schema with the same name will be overwritten
cohortTableStem<-"..." # needs to be in lower case

#put in the start date from which you have usable data must be in YYYY-MM-DD
startdate <- "..."

# number of years of extrapolation
timeinyrs <- "..." # amount of followup in your database plus 10 years

# run gender stratification (there will be a if statement in the analysis code which will call the analysis for this)
RunGenderStrat <- TRUE

# run age stratification
RunAgeStrat <- TRUE

# connect to the database
db <- DBI::dbConnect("...", dbname = server_dbi, port = port, host = host, user = user,
                     password = password)


# to check the DBI worked, uncomment and run the below line
tbl(db, sql(paste0("SELECT * FROM ",cdm_database_schema, ".person"))) %>% tally()
# you should have a count of people in the database printed back in the console

# Run the study ------
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your results folder to share

