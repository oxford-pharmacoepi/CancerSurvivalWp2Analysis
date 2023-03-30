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
db.name<-"CPRD_Aurum"
#db.name<-"CPRD_GOLD"

# Set output folder locations -----
# the path to a folder where the results from this analysis will be saved
output.folder<-here("Results",db.name)

# database connection details
server     <- Sys.getenv("DB_SERVER_cdm_aurum_202106") # AURUM
server_dbi <- Sys.getenv("DB_SERVER_cdm_aurum_202106_dbi") #AURUM
# server     <- Sys.getenv("DB_SERVER_cdmgold202007") # GOLD
# server_dbi <- Sys.getenv("DB_SERVER_cdmgold202007_dbi") #GOLD
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT") 
host       <- Sys.getenv("DB_HOST") 

# schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"public"

# schema where a results table will be created 
results_database_schema<-"results"

# stem for tables to be created in your results schema for this analysis
# You can keep the above names or change them
# Note, any existing tables in your results schema with the same name will be overwritten
cohortTableStem<-"ehdenwp2cancerextrap" # needs to be in lower case

#put in the start date from which you have usable data for this study must be in format YYYY-MM-DD
startdate <- '2000-01-01'

# number of years of extrapolation
timeinyrs <- 30 # amount of followup in your database plus 10 years so if your databses has 20 years of followup you would put 30 here

# run gender analysis
RunGenderAnalysis <- TRUE

# run age analysis (10 year age bands)
RunAgeAnalysis <- TRUE

# connect to the database
db <- DBI::dbConnect(RPostgres::Postgres(), dbname = server_dbi, port = port, host = host, user = user,
                     password = password)


# to check the DBI worked, uncomment and run the below line
tbl(db, sql(paste0("SELECT * FROM ",cdm_database_schema, ".person"))) %>% tally()
# you should have a count of people in the database printed back in the console

# Run the study ------
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your results folder to share