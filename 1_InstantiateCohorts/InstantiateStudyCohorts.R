

#### USING COHORT GENERATOR ######
# Get cohort details -----
cohortJsonFiles <- list.files(here("1_InstantiateCohorts", "ExposureCohorts", "json"))
cohortJsonFiles <- cohortJsonFiles[str_detect(cohortJsonFiles,".json")]

cohortDefinitionSet <- list()
for(i in 1:length(cohortJsonFiles)){
  working.json<-here("1_InstantiateCohorts", "ExposureCohorts", "json",
                     cohortJsonFiles[i])
  cohortJson <- readChar(working.json, file.info(working.json)$size)
  cohortExpression <- cohortExpressionFromJson(cohortJson) # generates the sql
  sql <- buildCohortQuery(cohortExpression, 
                          options = CirceR::createGenerateOptions(generateStats = TRUE))
  
  cohortDefinitionSet[[i]]<-tibble(atlasId = i,
                                   cohortId = i,
                                   cohortName = str_replace(cohortJsonFiles[i],".json",""),
                                   json=cohortJson,
                                   sql=sql,
                                   logicDescription = NA,
                                   generateStats=TRUE)
}
cohortDefinitionSet<-bind_rows(cohortDefinitionSet)

# Names of tables to be created during study run ----- 
cohortTableNames <- CohortGenerator::getCohortTableNames(cohortTable = cohortTableStem)

# Create the tables in the database -----
CohortGenerator::createCohortTables(connectionDetails = connectionDetails,
                                    cohortTableNames = cohortTableNames,
                                    cohortDatabaseSchema = results_database_schema)

# Generate the cohort set -----
CohortGenerator::generateCohortSet(connectionDetails= connectionDetails,
                                   cdmDatabaseSchema = cdm_database_schema,
                                   cohortDatabaseSchema = results_database_schema,
                                   cohortTableNames = cohortTableNames,
                                   cohortDefinitionSet = cohortDefinitionSet)

# get stats  -----
CohortGenerator::exportCohortStatsTables(
  connectionDetails = connectionDetails,
  connection = NULL,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  cohortStatisticsFolder = here("Results"),
  incremental = FALSE)

# get the number of people in the cohort
getCohortCounts(connectionDetails = connectionDetails,
                cohortDatabaseSchema = results_database_schema,
                cohortTable = cohortTableNames$cohortTable)


# drop cohort stats table
CohortGenerator::dropCohortStatsTables(
  connectionDetails = connectionDetails,
  cohortDatabaseSchema = results_database_schema,
  cohortTableNames = cohortTableNames,
  connection = NULL)


exposure.cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        results_database_schema,".",
                                        cohortTableExposures)))%>% 
  mutate(cohort_definition_id=as.integer(cohort_definition_id)) 

# dbListObjects(db) #shows results 
# dbListObjects(db, Id(schema = "results")) # states the results tables


# drop any exposure cohorts with less than 5 people
exposure.cohorts_db %>%
  group_by(cohort_definition_id) %>% tally()

exposure.cohorts<-exposure.cohorts_db %>% 
               group_by(cohort_definition_id) %>% 
               tally() %>% 
               collect() %>% 
               filter(n>5) 










##### USING OLD WAY ############
# 
# # connect -----
# conn <- connect(connectionDetails)
# 
# # instantiate exposure cohorts -----
# CohortsToCreate <- suppressMessages(read_csv("1_InstantiateCohorts/ExposureCohorts/CohortsToCreate.csv"))
# exposure.cohorts<-tibble(id=CohortsToCreate$cohortId,
#                         file=paste0(CohortsToCreate$name, ".sql"),
#                         name=CohortsToCreate$name)
# if(run.as.test==TRUE){
#   exposure.cohorts<-exposure.cohorts %>% filter(name=="colorectal_cancer")
# }
# 
# if(create.exposure.cohorts==TRUE){
# print(paste0("- Getting exposure cohorts"))
# info(logger, "- Getting exposure cohorts")
# 
# # create empty cohorts table
# sql<-readSql(here("1_InstantiateCohorts","ExposureCohorts","sql","CreateCohortTable.sql"))
# sql<-SqlRender::translate(sql, targetDialect = targetDialect)
# renderTranslateExecuteSql(conn=conn, 
#                             sql,
#                             cohort_database_schema =  results_database_schema,
#                             cohort_table = cohortTableExposures)
# rm(sql)
#   
# for(cohort.i in 1:length(exposure.cohorts$id)){
# working.id<-exposure.cohorts$id[cohort.i]
# print(paste0("-- Getting: ",  exposure.cohorts$name[cohort.i],
#                  " (", cohort.i, " of ", length(exposure.cohorts$name), ")"))
# info(logger, paste0("-- Getting: ",  exposure.cohorts$name[cohort.i],
#                  " (", cohort.i, " of ", length(exposure.cohorts$name), ")"))
# 
# 
# sql<-readSql(here("1_InstantiateCohorts","ExposureCohorts","sql",exposure.cohorts$file[cohort.i])) 
# sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
# 
# sql<-SqlRender::translate(sql, targetDialect = targetDialect)
# renderTranslateExecuteSql(conn=conn, 
#                               sql, 
#                               cdm_database_schema = cdm_database_schema,
#                               vocabulary_database_schema = vocabulary_database_schema,
#                               target_database_schema = results_database_schema,
#                               # results_database_schema = results_database_schema,
#                               target_cohort_table = cohortTableExposures,
#                               target_cohort_id = working.id)  
#   }
# 
# } else {
#   print(paste0("Skipping creating exposure cohorts")) 
#   info(logger, "Skipping creating exposure cohorts")
# 
# }
# 
# # link to table
# exposure.cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
#                                         results_database_schema,".",
#                                         cohortTableExposures))) %>% 
#   mutate(cohort_definition_id=as.integer(cohort_definition_id)) 
# 
# 
# # drop any exposure cohorts with less than 5 people
# exposure.cohorts_db %>%
#   group_by(cohort_definition_id) %>% tally()
# 
# exposure.cohorts<-exposure.cohorts %>% 
#   inner_join(exposure.cohorts_db %>% 
#                group_by(cohort_definition_id) %>% 
#                tally() %>% 
#                collect() %>% 
#                filter(n>5) %>% 
#                select(cohort_definition_id),
#              by=c("id"="cohort_definition_id"))
# # exposure.cohorts
# 

# disconnect ----
#disconnect(db)

# # instantiate outcome cohorts -----
# CohortsToCreate <- suppressMessages(read_csv("1_InstantiateCohorts/OutcomeCohorts/CohortsToCreate.csv"))
# outcome.cohorts<-tibble(id=CohortsToCreate$cohortId,
#                         file=paste0(CohortsToCreate$name, ".sql"),
#                         name=CohortsToCreate$name)
# 
# if(run.as.test==TRUE){
#   outcome.cohorts<-outcome.cohorts %>% filter(name=="DVT narrow")
# }
# 
# if(create.outcome.cohorts==TRUE){
# print(paste0("- Getting outcome cohorts"))
# info(logger, "- Getting outcome cohorts")
# 
# # create empty cohorts table
# sql<-readSql(here("1_InstantiateCohorts","OutcomeCohorts","sql","CreateCohortTable.sql"))
# sql<-SqlRender::translate(sql, targetDialect = targetDialect)
# renderTranslateExecuteSql(conn=conn, 
#                           sql,
#                           cohort_database_schema =  results_database_schema,
#                           cohort_table = cohortTableOutcomes)
# rm(sql)
# 
# for(cohort.i in 1:length(outcome.cohorts$id)){
#   working.id<-outcome.cohorts$id[cohort.i]
#   print(paste0("-- Getting: ",  outcome.cohorts$name[cohort.i],
#                " (", cohort.i, " of ", length(outcome.cohorts$name), ")"))
#   info(logger, paste0("-- Getting: ",  outcome.cohorts$name[cohort.i],
#                " (", cohort.i, " of ", length(outcome.cohorts$name), ")"))
#   
#   sql<-readSql(here("1_InstantiateCohorts","OutcomeCohorts", "sql",outcome.cohorts$file[cohort.i])) 
#   sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
#   sql<-SqlRender::translate(sql, targetDialect = targetDialect)
#   renderTranslateExecuteSql(conn=conn, 
#                             sql, 
#                             cdm_database_schema = cdm_database_schema,
#                             vocabulary_database_schema = vocabulary_database_schema,
#                             target_database_schema = results_database_schema,
#                             # results_database_schema = results_database_schema,
#                             target_cohort_table = cohortTableOutcomes,
#                             target_cohort_id = working.id)  
# }
# } else {
#   print(paste0("Skipping creating exposure cohorts"))
#   info(logger, "Skipping creating exposure cohorts")
# 
# }



# # link to table
# outcome.cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
#                                         results_database_schema,".",
#                                         cohortTableOutcomes)))%>% 
#   mutate(cohort_definition_id=as.integer(cohort_definition_id)) 
# 
# # drop any outcome cohorts with fewer than 5 people in database
# outcome.cohorts<-outcome.cohorts %>% 
#   inner_join(outcome.cohorts_db %>% 
#                group_by(cohort_definition_id) %>% 
#                tally() %>% 
#                collect() %>% 
#                filter(n>5) %>% 
#                select(cohort_definition_id),
#              by=c("id"="cohort_definition_id"))  

# outcome.cohorts_db %>% 
#                group_by(cohort_definition_id) %>% 
#                tally()
# instantiate comorbidity cohorts ----
# all condition occurrences for people in exposure cohorts
# cond.codes<-c("434621",
#               "4098292",
#               "4125650",
#               "317009",
#               "313217",
#               "443392",
#               "201820",
#               "433736",
#               "321588",
#               "316866",
#               "4030518",
#               "255573",
#               "4182210")
# cond.names<-c("autoimmune_disease",
#               "antiphospholipid_syndrome",
#               "thrombophilia",
#               "asthma",
#               "atrial_fibrillation",
#               "malignant_neoplastic_disease",
#               "diabetes_mellitus",
#               "obesity",
#               "heart_disease",
#               "hypertensive_disorder",
#               "renal_impairment",
#               "copd",
#               "dementia")

# 
# if(create.profile.cohorts==TRUE){
# # create empty table
# sql<-readSql(here("1_InstantiateCohorts","ExposureCohorts","sql","CreateCohortTable.sql"))
# sql<-SqlRender::translate(sql, targetDialect = targetDialect)
# renderTranslateExecuteSql(conn=conn, 
#                           sql,
#                           cohort_database_schema = results_database_schema,
#                           cohort_table = cohortTableComorbidities,
#                           progressBar = FALSE)
# 
# 
# 
# for(i in 1:length(cond.codes)){
# 
# working.cond.db<-condition_occurrence_db  %>% 
#   select(person_id, condition_concept_id,condition_start_date )%>% 
#                rename("subject_id"="person_id") %>% 
#   distinct() %>% 
#   inner_join(exposure.cohorts_db %>% 
#                select(subject_id) %>% 
#                distinct()) %>% 
#   inner_join(concept_ancestor_db %>%
#                # all descendents
#   filter(ancestor_concept_id %in% !!cond.codes[i]) %>% 
#   select(descendant_concept_id) %>% 
#   rename("condition_concept_id"="descendant_concept_id")) %>% 
#   distinct() %>% 
#   rename("cohort_start_date"="condition_start_date") %>% 
#   mutate("cohort_definition_id"=as.integer(!!cond.codes[i])) %>% 
#   mutate("cohort_end_date"=cohort_start_date) %>% 
#   select(cohort_definition_id, subject_id, cohort_start_date,cohort_end_date) 
#  
# # first in history (we´re using all history time window prior to index) 
# working.cond.db<-working.cond.db %>% 
#   group_by(subject_id) %>% 
#   slice_min(cohort_start_date, n = 1) 
# 
# 
# 
# # insert into  table
# print(paste0("-- Getting ", cond.names[i]))
# info(logger, paste0("-- Getting ", cond.names[i]))
# 
# start.insert<-Sys.time()
# # insert into tmp table
# 
# sql_query <- glue::glue("IF OBJECT_ID('{results_database_schema}.{cohortTableComorbiditiestmp}', 'U') IS NOT NULL\n",
# 	"DROP TABLE {results_database_schema}.{cohortTableComorbiditiestmp};")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, as.character(sql_query))
# 
# sql_query <- glue::glue("SELECT * \n",
#                           "INTO {results_database_schema}.{cohortTableComorbiditiestmp}\n",
#                           " FROM (\n",
#                           dbplyr::sql_render(working.cond.db),
#                           "\n) AS from_table")
# DBI::dbExecute(db, as.character(sql_query))
# # tbl(db, sql(paste0("SELECT * FROM ",  results_database_schema,
# #                     ".", cohortTableComorbiditiestmp)))
# 
# sql_query <- glue::glue("INSERT ",
#                           "INTO {results_database_schema}.{cohortTableComorbidities} ",
#                           "SELECT * FROM {results_database_schema}.{cohortTableComorbiditiestmp}")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# duration <- Sys.time()-start.insert
# print(paste("--- Getting", cond.names[i], "took",  round(duration[[1]], 2),  units(duration)))
# info(logger, paste("--- Getting", cond.names[i], "took",  round(duration[[1]], 2),  units(duration)))
# 
# }
# } else {
#    print(paste0("Skipping creating comorbidity cohorts")) 
#   info(logger, "Skipping creating comorbidity cohorts")
# 
#  }
# 
# cohortTableComorbidities_db<- tbl(db, sql(paste0("SELECT * FROM ",  results_database_schema,
#                     ".", cohortTableComorbidities))) 
# 
# # cohortTableComorbidities_db%>%
# #   group_by(cohort_definition_id) %>%
# #    tally()
# 
# 
# 
# # instantiate medication cohorts ----
# # all meds for people in exposure cohorts
# drug.codes<-c("21603933",
#               "21603991",
#               "21602722",
#               "21600961",
#               "21601853",
#               "21601386",
#               "21602472",
#               "21603831",
#               "21602471",
#               "21601254")
# drug.names<-c("antiinflamatory_and_antirheumatic",
#               "coxibs",
#               "corticosteroids",
#               "antithrombotic",
#               "lipid_modifying",
#               "antineoplastic_immunomodulating",
#               "hormonal_contraceptives",
#               "tamoxifen",
#               "sex_hormones_modulators",
#               "immunoglobulins")
# 
# if(create.profile.cohorts==TRUE){
# # create empty table
# sql<-readSql(here("1_InstantiateCohorts","ExposureCohorts","sql","CreateCohortTable.sql"))
# sql<-SqlRender::translate(sql, targetDialect = targetDialect)
# renderTranslateExecuteSql(conn=conn, 
#                           sql,
#                           cohort_database_schema = results_database_schema,
#                           cohort_table = cohortTableMedications,
#                           progressBar = FALSE)
# 
# 
# 
# for(i in 1:length(drug.codes)){
# 
# working.drug.db<-drug_era_db  %>% 
#   filter(drug_era_end_date>=!!as.Date("01/05/2020", "%d/%m/%y")) %>% # we´re going to look back 180 days max
#   select(person_id, drug_concept_id,drug_era_start_date )%>% 
#                rename("subject_id"="person_id") %>% 
#   distinct() %>% 
#   inner_join(exposure.cohorts_db %>% 
#                select(subject_id) %>% 
#                distinct()) %>% 
#   inner_join(concept_ancestor_db %>%
#                # all descendents
#   filter(ancestor_concept_id %in% !!drug.codes[i]) %>% 
#   select(descendant_concept_id) %>% 
#   rename("drug_concept_id"="descendant_concept_id")) %>% 
#   distinct() %>% 
#   rename("cohort_start_date"="drug_era_start_date") %>% 
#   mutate("cohort_definition_id"=as.integer(!!drug.codes[i])) %>% 
#   mutate("cohort_end_date"=cohort_start_date) %>% 
#   select(cohort_definition_id, subject_id, cohort_start_date,cohort_end_date)
#   
#   
#   
# # insert into  table
# print(paste0("-- Getting ", drug.names[i]))
# info(logger, paste0("-- Getting ", drug.names[i]))
# 
# start.insert<-Sys.time()
# # insert into tmp table
# 
# sql_query <- glue::glue("IF OBJECT_ID('{results_database_schema}.{cohortTableMedicationstmp}', 'U') IS NOT NULL\n",
# 	"DROP TABLE {results_database_schema}.{cohortTableMedicationstmp};")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, as.character(sql_query))
# 
# sql_query <- glue::glue("SELECT * \n",
#                           "INTO {results_database_schema}.{cohortTableMedicationstmp}\n",
#                           " FROM (\n",
#                           dbplyr::sql_render(working.drug.db),
#                           "\n) AS from_table")
# DBI::dbExecute(db, as.character(sql_query))
# # tbl(db, sql(paste0("SELECT * FROM ",  results_database_schema,
# #                     ".", cohortTableMedicationstmp)))
# 
# sql_query <- glue::glue("INSERT ",
#                           "INTO {results_database_schema}.{cohortTableMedications} ",
#                           "SELECT * FROM {results_database_schema}.{cohortTableMedicationstmp}")
# sql_query<-SqlRender::translate(sql_query, targetDialect = targetDialect)
# DBI::dbExecute(db, sql_query)
# 
# duration <- Sys.time()-start.insert
# print(paste("--- Getting", drug.names[i], "took",  round(duration[[1]], 2),  units(duration)))
# info(logger, paste("--- Getting", drug.names[i], "took",  round(duration[[1]], 2),  units(duration)))
# 
# }
# } else {
#    print(paste0("Skipping creating medication cohorts")) 
#   info(logger, paste0("Skipping creating medication cohorts"))
# 
#  }
# 
# cohortTableMedications_db<- tbl(db, sql(paste0("SELECT * FROM ",  results_database_schema,
#                     ".", cohortTableMedications))) 
# # cohortTableMedications_db%>%
# #   group_by(cohort_definition_id) %>%
# #    tally()
# 
# # instantiate covid cohorts -----
# cohort.sql<-list.files(here("1_InstantiateCohorts","CovidCohorts", "sql"))
# cohort.sql<-cohort.sql[cohort.sql!="CreateCohortTable.sql"]
# covid.cohorts<-tibble(id=as.integer(1:length(cohort.sql)),
#                         file=cohort.sql,
#                         name=str_replace(cohort.sql, ".sql", ""))
# 
# if(create.profile.cohorts==TRUE){
# print(paste0("- Getting covid cohorts"))
# info(logger,paste0("- Getting covid cohorts"))
# 
# 
# # create empty cohorts table
# sql<-readSql(here("1_InstantiateCohorts","CovidCohorts","sql","CreateCohortTable.sql"))
# sql<-SqlRender::translate(sql, targetDialect = targetDialect)
# renderTranslateExecuteSql(conn=conn, 
#                           sql,
#                           cohort_database_schema =  results_database_schema,
#                           cohort_table = cohortTableCovid)
# rm(sql)
# 
# for(cohort.i in 1:length(covid.cohorts$id)){
#   working.id<-outcome.cohorts$id[cohort.i]
#   print(paste0("-- Getting: ",  covid.cohorts$name[cohort.i],
#                " (", cohort.i, " of ", length(covid.cohorts$name), ")"))
#   info(logger,paste0("-- Getting: ",  covid.cohorts$name[cohort.i],
#                " (", cohort.i, " of ", length(covid.cohorts$name), ")"))
# 
#   
#   sql<-readSql(here("1_InstantiateCohorts","CovidCohorts", "sql",covid.cohorts$file[cohort.i])) 
#   sql <- sub("BEGIN: Inclusion Impact Analysis - event.*END: Inclusion Impact Analysis - person", "", sql)
#   sql<-SqlRender::translate(sql, targetDialect = targetDialect)
#   renderTranslateExecuteSql(conn=conn, 
#                             sql, 
#                             cdm_database_schema = cdm_database_schema,
#                             vocabulary_database_schema = vocabulary_database_schema,
#                             target_database_schema = results_database_schema,
#                             # results_database_schema = results_database_schema,
#                             target_cohort_table = cohortTableCovid,
#                             target_cohort_id = working.id)  
# }
# } else {
#   print(paste0("Skipping creating covid cohorts"))
#   info(logger,paste0("Skipping creating covid cohorts"))
# 
# }
# 
# 
# 
# # link to table
# covid.cohorts_db<-tbl(db, sql(paste0("SELECT * FROM ",
#                                         results_database_schema,".",
#                                         cohortTableCovid)))%>% 
#   mutate(cohort_definition_id=as.integer(cohort_definition_id)) 

# # disconnect ----
# disconnect(conn)
