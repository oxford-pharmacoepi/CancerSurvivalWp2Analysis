# Manage project dependencies ------
# the following will prompt you to install the various packages used in the study 
# install.packages("renv")
renv::activate()
renv::restore()

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
library(SqlRender)
library(DrugUtilisation)

# Set the short name/acronym for your database (to be used in the titles of reports, etc) -----
# Please do not use omop, cdm for db.name
db.name <-"..."

# Set output folder locations -----
# the path to a folder where the results from this analysis will be saved
output.folder <- here::here("Results", db.name)

# database connection details
server     <- "..."
server_dbi <- "..."
user       <- "..."
password   <- "..."
port       <- "..." 
host       <- "..." 
dbms       <- "..."

# Specify cdm_reference via DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)
db <- DBI::dbConnect(dbms,
                dbname = server_dbi,
                port = port,
                host = host, 
                user = user, 
                password = password)

# Set database details -----
# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema <- "..."

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema <- "..."

# The name of the schema where results tables will be created 
results_database_schema <- "..."

# stem table description use something short and informative such as ehdenwp2 or your initials
# Note, if there is an existing table in your results schema with the same names it will be overwritten 
# needs to be in lower case and NOT more than 10 characters
table_stem <- "..."

# create cdm reference ---- DO NOT REMOVE "PREFIX" ARGUMENT IN THIS CODE
cdm <- CDMConnector::cdm_from_con(con = db, 
                                  cdm_schema = cdm_database_schema,
                                  write_schema = c("schema" = results_database_schema, 
                                                   "prefix" = table_stem),
                                  cdm_name = db.name)

# to check whether the DBI connection is correct, 
# running the next line should give you a count of your person table
cdm$person %>% 
  dplyr::tally() %>% 
  CDMConnector::computeQuery()

# Set study details -----
# if you do not have suitable data from 2000-01-01 
# please put year of useable data starting from 1st jan 
# must be in format YYYY-MM-DD ie. 20XX-01-01
startdate <- "2000-01-01" 

# Prior history -----
# if you have a database where the observation period start date for each patient is the date of cancer diagnosis (ie. some cancer registries)
# set this value to FALSE. If your database has linkage or data where you can look in prior history before cancer diagnosis (e.g. primary care)
# set as TRUE.
priorhistory <- TRUE

# Truncated time analysis ------
# By setting this to TRUE this will perform an additional analysis where extrapolation methods will extrapolate on the observed data truncated at 2 years NOT on the full observed data. 
# If FALSE this additional analysis will not be run.
PerformTruncatedAnalysis <- TRUE

# Run the study ------
source(here::here("RunStudy.R"))
# after the study is run you should have a zip folder in your output folder to share

# drop the permanent tables from the study 
# YOU MUST HAVE TABLE STEM SET ABOVE WITH A NAME ABOVE OTHERWISE IT WILL DELETE EVERYTHING!
#CDMConnector::dropTable(cdm, dplyr::everything())

# Disconnect from database
#dbDisconnect(db)
