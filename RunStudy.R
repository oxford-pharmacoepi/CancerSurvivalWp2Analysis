if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

start<-Sys.time()
# extra options for running -----
# if you have already created the cohorts, you can set this to FALSE to skip instantiating these cohorts again
create.exposure.cohorts<-TRUE
# create.outcome.cohorts<-TRUE
# create.profile.cohorts<-TRUE

# to run for just one exposure/ outcome pair
run.as.test<-FALSE

# run main exposure/ outcome pairs only
run.main.analyses.only<-FALSE

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# functions ----
# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=0, scientific=FALSE))}


# link to db tables -----
person_db<-tbl(db, sql(paste0("SELECT * FROM ",
                              cdm_database_schema,
                              ".person")))
observation_period_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                          cdm_database_schema,
                                          ".observation_period")))
visit_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        cdm_database_schema,
                                        ".visit_occurrence")))
condition_occurrence_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                            cdm_database_schema,
                                            ".condition_occurrence")))
drug_era_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                cdm_database_schema,
                                ".drug_era")))
concept_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept")))
concept_ancestor_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                        vocabulary_database_schema,
                                        ".concept_ancestor")))

death_db<-tbl(db, sql(paste0("SELECT * FROM ",
                                cdm_database_schema,
                                ".death")))


# result table names ----
#cohortTableExposures<-paste0(cohortTableStem, "Exposures") # using this doesnt work
cohortTableExposures<-paste0(cohortTableStem)

# cohortTableOutcomes<-paste0(cohortTableStem, "Outcomes")
# cohortTableComorbiditiestmp<-paste0(cohortTableStem, "Comorbiditiestmp")
# cohortTableComorbidities<-paste0(cohortTableStem, "Comorbidities")
# cohortTableCovid<-paste0(cohortTableStem, "Covid")
# cohortTableMedicationstmp<-paste0(cohortTableStem, "Medicationstmp")
# cohortTableMedications<-paste0(cohortTableStem, "Medications")
# cohortTableLargeScaleFeatures<-paste0(cohortTableStem, "LSF")

# instantiate study cohorts ----
info(logger, 'INSTANTIATING STUDY COHORTS')
source(here("1_InstantiateCohorts","InstantiateStudyCohorts.R"))
info(logger, 'GOT STUDY COHORTS')

# study cohorts ----
# The study cohorts are various cancer cohorts that have been instantiated (10 aug just people with colorectal and a dummy set to make sure it works for > 1)

# get variables for analysis ---
# get the variables needed for demographics for cohorts (age, gender, death date)
# this pulls out information from person table and attaches information to the exposure cohorts (all of them)
Pop<-person_db %>% 
  inner_join(exposure.cohorts_db,
             by = c("person_id" = "subject_id" )) %>%
  select(person_id,gender_concept_id, 
         year_of_birth, month_of_birth, day_of_birth,
         cohort_start_date,
         cohort_definition_id)  %>% 
  left_join(observation_period_db %>% 
              select("person_id",  "observation_period_start_date", "observation_period_end_date") %>% 
              distinct(),
            by = "person_id") %>% 
  left_join(death_db %>% 
              select("person_id",  "death_date") %>% 
              distinct(),
            by = "person_id") %>% 
  
  collect()


# only include people with a diagnosis that starts at or after 1st jan 2005 ---
Pop<-Pop %>% 
  filter(cohort_start_date >= '2005-01-01') 

# Only include people with a diagnosis at or before 1st jan 2019 to remove pandemic effects ---
Pop<-Pop %>% 
  filter(cohort_start_date <= '2019-01-01') 


# format data -----
# add age -----
Pop$age<- NA
if(sum(is.na(Pop$day_of_birth))==0 & sum(is.na(Pop$month_of_birth))==0){
  # if we have day and month 
  Pop<-Pop %>%
    mutate(age=floor(as.numeric((ymd(cohort_start_date)-
                                   ymd(paste(year_of_birth,
                                             month_of_birth,
                                             day_of_birth, sep="-"))))/365.25))
} else { 
  Pop<-Pop %>% 
    mutate(age= year(cohort_start_date)-year_of_birth)
}


# age age groups ----
Pop<-Pop %>% 
  mutate(age_gr=ifelse(age<20,  "<20",
                       ifelse(age>=20 &  age<=44,  "20-44",
                              ifelse(age>=45 & age<=54,  "45-54",
                                     ifelse(age>=55 & age<=64,  "55-64",
                                            ifelse(age>=65 & age<=74, "65-74", 
                                                   ifelse(age>=75 & age<=84, "75-84",      
                                                          ifelse(age>=85, ">=85",
                                                                 NA)))))))) %>% 
  mutate(age_gr= factor(age_gr, 
                        levels = c("<20","20-44","45-54", "55-64",
                                   "65-74", "75-84",">=85"))) 
table(Pop$age_gr, useNA = "always")

# wider age groups
Pop<-Pop %>% 
  mutate(age_gr2=ifelse(age<=50,  "<=50",
                               ifelse(age>50, ">50",
                                      NA))) %>% 
  mutate(age_gr2= factor(age_gr2, 
                         levels = c("<=50", ">50")))
table(Pop$age_gr2, useNA = "always")


# reformat gender
# add gender -----
#8507 male
#8532 female
Pop<-Pop %>% 
  mutate(gender= ifelse(gender_concept_id==8507, "Male",
                        ifelse(gender_concept_id==8532, "Female", NA ))) %>% 
  mutate(gender= factor(gender, 
                        levels = c("Male", "Female")))
table(Pop$gender, useNA = "always")

# if missing (or unreasonable) age or gender, drop ----
Pop<-Pop %>% 
  filter(!is.na(age)) %>% 
  filter(age>=18) %>% 
  filter(age<=110) %>%
  filter(!is.na(gender))


# drop if missing observation period end date ----
Pop<-Pop %>% 
  filter(!is.na(observation_period_end_date))

# add prior observation time -----
Pop<-Pop %>%  
  mutate(prior_obs_days=as.numeric(difftime(cohort_start_date,
                                            observation_period_start_date,
                                            units="days"))) %>% 
  mutate(prior_obs_years=prior_obs_days/365.25)

# make sure all have year of prior history ---
Pop<-Pop %>%
  filter(prior_obs_years>=1)
# the above removes those with 0.999931 but 365 days removes 21 people - due to rounding errors


# need to make new end of observation period to 1/1/2019 ----
Pop<-Pop %>% 
  mutate(observation_period_end_date_2019 = ifelse(observation_period_end_date >= '2019-01-01', '2019-01-01', NA)) %>%
  mutate(observation_period_end_date_2019 = as.Date(observation_period_end_date_2019) ) %>%
  mutate(observation_period_end_date_2019 = coalesce(observation_period_end_date_2019, observation_period_end_date))
  
 
# binary death outcome (for survival) ---
# need to take into account follow up
# if death date is > 1/1/2019 set death to 0
Pop<-Pop %>% 
  mutate(status= ifelse(!is.na(death_date), 1, 0 )) %>%
  mutate(status= ifelse(death_date > observation_period_end_date_2019 , 0, status )) %>% 
  mutate(status= ifelse(is.na(status), 0, status )) %>%
  mutate_at(vars(status), 
            list(factor))

# calculate follow up in years
Pop<-Pop %>%  
  mutate(time_days=as.numeric(difftime(observation_period_end_date_2019,
                                            cohort_start_date,
                                            units="days"))) %>% 
  mutate(time_years=time_days/365.25) 


# remove people with end of observation end date == cohort entry
Pop<-Pop %>%
  filter(time_days != 0)


# remove females with a diagnosis with prostate cancer
# use the cohortDefinition to find out the cohort id for prostate
PC_id <- as.numeric(cohortDefinitionSet[grep("Prostate", cohortDefinitionSet$cohortName, ignore.case = TRUE), ][,2])

#filter out those who are female with prostate cancer
Pop<-Pop %>%
  filter(!(gender == "Female" & cohort_definition_id == PC_id))

# table(Pop$cohort_definition_id == 8, Pop$gender)
# 
# table(Pop$cohort_definition_id)


# Run analysis ----
# info(logger, 'RUNNING ANALYSIS')
# source(here("2_Analysis","Analysis.R"))
# info(logger, 'ANALYSIS RAN')
# 
# # Tidy up and save ----
# Survival.summary<-bind_rows(Survival.summary, .id = NULL)
# Survival.summary$db<-db.name
# Survival.summary<-Survival.summary %>% 
#   group_by(group, strata, outcome,pop, pop.type,
#            outcome.name,prior.obs.required, surv.type) %>% 
#   mutate(cum.n.event=cumsum(n.event))
# 
# Cohort.age.plot.data<-bind_rows(Cohort.age.plot.data, .id = NULL)
# 
# 
# save(Patient.characteristcis, 
#      file = paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData"))
# save(Survival.summary, 
#      file = paste0(output.folder, "/Survival.summary_", db.name, ".RData"))
# save(Model.estimates, 
#      file = paste0(output.folder, "/Model.estimates_", db.name, ".RData"))
# save(Cohort.age.plot.data, 
#      file = paste0(output.folder, "/Cohort.age.plot.data_", db.name, ".RData"))
# 
# # Time taken
# x <- abs(as.numeric(Sys.time()-start, units="secs"))
# info(logger, paste0("Study took: ", 
#                     sprintf("%02d:%02d:%02d:%02d", 
#                             x %/% 86400,  x %% 86400 %/% 3600, x %% 3600 %/% 
#                               60,  x %% 60 %/% 1)))
# 
# # # zip results
# print("Zipping results to output folder")
# unlink(paste0(output.folder, "/OutputToShare_", db.name, ".zip"))
# zipName <- paste0(output.folder, "/OutputToShare_", db.name, ".zip")
# 
# files<-c(log_file,
#          paste0(output.folder, "/Patient.characteristcis_", db.name, ".RData"),
#          paste0(output.folder, "/Survival.summary_", db.name, ".RData"),
#          paste0(output.folder, "/Model.estimates_", db.name, ".RData") ,
#          paste0(output.folder, "/Cohort.age.plot.data_", db.name, ".RData") )
# files <- files[file.exists(files)==TRUE]
# createZipFile(zipFile = zipName,
#               rootFolder=output.folder,
#               files = files)
# 
# print("Done!")
# print("-- If all has worked, there should now be a zip folder with your results in the output folder to share")
# print("-- Thank you for running the study!")
# Sys.time()-start
# # readLines(log_file)

