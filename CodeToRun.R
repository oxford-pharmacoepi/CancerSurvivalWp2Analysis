# Manage project dependencies ------
# install.packages("renv") # if not already installed, install renv from CRAN
# the following will prompt you to install the various packages used in the study 
renv::activate()
renv::restore() 

# Load packages ------
# load r packages
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

# Set the name/ acronym for your database (to be used in the titles of reports, etc) -----
db.name<-"..."

# Set output folder locations -----
# the path to a folder where the results from this analysis will be saved
# to set the location within the project with folder called "CPRD", we can use: here("CPRD")
# but this file path could be set to somewhere else
output.folder<-here("Results",db.name)
plots.folder <-here(output.folder, "Plots")
example.plots.folder <- here("3_ExamplePlots")


# database connection details
server     <- "..."
server_dbi <- "..."
user       <- "..."
password   <- "..."
port       <- "..."
host       <- "..." 

# Specify OHDSI DatabaseConnector connection details  ------
# set up the createConnectionDetails to connect to the database
# see http://ohdsi.github.io/DatabaseConnector for more details

downloadJdbcDrivers("...", here()) # if you already have this you can omit and change pathToDriver below
connectionDetails <- createConnectionDetails(dbms = "...",
                                             server =server,
                                             user = user,
                                             password = password,
                                             port = port ,
                                             pathToDriver = here())



# Specify DBI connection details -----
# In this study we also use the DBI package to connect to the database
# set up the dbConnect details below (see https://dbi.r-dbi.org/articles/dbi for more details)
# you may need to install another package for this (although RPostgres is included with renv in case you are using postgres)

db <- dbConnect(RPostgres::Postgres(), dbname = server_dbi, port = port, host = host, user = user,
                password = password)


# Set database details -----

# your sql dialect used with the OHDSI SqlRender package
# eg postgresql, redshift etc
# see https://ohdsi.github.io/SqlRender/articles/UsingSqlRender.html for more details
targetDialect <-"..." 

# The name of the schema that contains the OMOP CDM with patient-level data
cdm_database_schema<-"..."

# The name of the schema that contains the vocabularies 
# (often this will be the same as cdm_database_schema)
vocabulary_database_schema<-"..."

# The name of the schema where results tables will be created 
results_database_schema<-"..."

# Tables to be created in your results schema for this analysis will be named using this as the stem 
# Note, any existing tables in your results schema with the same names will be overwritten
cohortTableStem<-"..."

# Check database connections -----
# to check whether the OHDSI DatabaseConnector worked, uncomment and run the below three lines
# conn <- connect(connectionDetails)
# querySql(conn,paste0("SELECT COUNT(*) FROM ", cdm_database_schema, ".person"))
# disconnect(conn)

# to check the DBI worked, uncomment and run the below line
#tbl(db, sql(paste0("SELECT * FROM ",cdm_database_schema, ".person"))) %>% tally()

# in both cases, you should have a count of people in the database printed back in the console

# Run the study ------
source(here("RunStudy.R"))

# after the study is run you should have a zip folder in your output folder to share



