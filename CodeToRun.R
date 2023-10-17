# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
renv::activate()
renv::restore()

# install.packages("devtools")
# devtools::install_github("darwin-eu-dev/CDMConnector")

# Load packages ------
library(CirceR)
library(here)
library(DBI)
library(dbplyr)
library(dplyr)
library(readr)
library(log4r)
library(tidyr)
library(stringr)
library(CDMConnector)
library(ggplot2)
library(broom)
library(survival)
library(bshazard)
library(flexsurv)
library(tictoc)
library(tibble)
library(RPostgres)
library(purrr)
library(PatientProfiles)
library(CodelistGenerator)

# Set the short name/acronym for your database (to be used in the titles of reports, etc) -----
db.name <-"CPRD_GOLD"

# Set output folder locations -----
# the path to a folder where the results from this analysis will be saved
output.folder <- here("Results",db.name)

# database connection details
server     <- Sys.getenv("DB_SERVER_cdm_gold_202207") # GOLD
server_dbi <- Sys.getenv("DB_SERVER_cdm_gold_202207_dbi") #GOLD
user       <- Sys.getenv("DB_USER")
password   <- Sys.getenv("DB_PASSWORD")
port       <- Sys.getenv("DB_PORT") 
host       <- Sys.getenv("DB_HOST") 

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "public"
#cdm_database_schema <- "public_100k"

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- cdm_database_schema

# The name of the schema where results tables will be created 
results_database_schema <- "results"

# stem table description use something short and informative such as ehdenwp2 or your initials
# Note, if there is an existing table in your results schema with the same names it will be overwritten 
# needs to be in lower case and NOT more than 10 characters
table_stem <-"ehdenwp2"

# create cdm reference ----
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = c("schema" = results_database_schema, 
                                                   "prefix" = table_stem),
                                  cdm_name = db.name)

# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  tally() %>% 
  computeQuery()

# Set study details -----
# put in the start date from which you have usable data for this study
# must be in format YYYY-MM-DD
startdate <- "2000-01-01" 

# calculating the number of years of extrapolation for your database ----
# amount of followup in your database plus 10 years
# so if your databases has 20 years of followup you would be 30 here
timeinyrs <- as.numeric(floor(((as.Date("2019-12-31") - as.Date(startdate)) / 365))) + 10

# Run the study ------
source(here("RunStudy.R"))
# after the study is run you should have a zip folder in your output folder to share

# drop the permanent tables from the study
#CDMConnector::dropTable(cdm, dplyr::starts_with(table_stem))

# Disconnect from database
#dbDisconnect(db)
