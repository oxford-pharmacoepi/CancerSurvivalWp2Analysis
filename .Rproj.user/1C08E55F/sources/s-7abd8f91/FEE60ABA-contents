# create folder name for QC plots
qc.plots.folder <- here("3_QCPlots") # for QCing and for troubleshooting

#Create folder for the results
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

if (!file.exists(qc.plots.folder)){
  dir.create(qc.plots.folder, recursive = TRUE)}

start<-Sys.time()
# extra options for running -----
# if you have already created the cohorts, you can set this to FALSE to skip instantiating these cohorts again
create.exposure.cohorts<- FALSE # need to set this up

# start log ----
log_file <- paste0(output.folder, "/log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# create study cohorts ----
# The study cohorts are various cancer cohorts that have been instantiated using CDM connector

# set a cdm reference -----
cdm <- CDMConnector::cdm_from_con(con = db, # connected using DBI dbConnect
                                  cdm_schema = cdm_database_schema, #schema of the database
                                  cdm_tables = tbl_group("clinical"), # which sets of tables needed
                                  write_schema = results_database_schema) # need this to show where to write results to


# read the cohorts using CDM connector
outcome_cohorts <- CDMConnector::readCohortSet(here(
  "1_InstantiateCohorts",
  "Cohorts" 
))


#create a cdm reference for your cohorts
cdm <- CDMConnector::generateCohortSet(cdm, outcome_cohorts,
                                       cohortTableName = cohortTableStem,
                                       overwrite = TRUE)

#check it has worked # need to update this to have whatever person has set as name
cdm$ehdenwp2cancerextrap %>% 
  group_by(cohort_definition_id) %>%
  tally()
# cohortTableStem

# get variables for analysis ---
Pop<-cdm$person %>% 
  inner_join(cdm$ehdenwp2cancerextrap,
             by = c("person_id" = "subject_id" )) %>%
  select(person_id,gender_concept_id, 
         year_of_birth, month_of_birth, day_of_birth,
         cohort_start_date,
         cohort_definition_id)  %>% 
  left_join(cdm$observation_period %>% 
              select("person_id",  "observation_period_start_date", "observation_period_end_date") %>% 
              distinct(),
            by = "person_id") %>% 
  left_join(cdm$death %>% 
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
  mutate(age_gr=ifelse(age<30,  "<30",
                       ifelse(age>=30 &  age<=39,  "30-39",
                              ifelse(age>=40 & age<=49,  "40-49",
                                     ifelse(age>=50 & age<=59,  "50-59",
                                            ifelse(age>=60 & age<=69, "60-69", 
                                                   ifelse(age>=70 & age<=79, "70-79", 
                                                   
                                                   ifelse(age>=80 & age<=89, "80-89",      
                                                          ifelse(age>=90, ">=90",
                                                                 NA))))))))) %>% 
  mutate(age_gr= factor(age_gr, 
                        levels = c("<30","30-39","40-49", "50-59",
                                   "60-69", "70-79","80-89",">=90"))) 
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

# create sex:agegp categorical variables
Pop <- Pop %>%
  unite('genderAgegp', c(gender,age_gr), remove = FALSE) %>%
  mutate(genderAgegp= factor(genderAgegp, 
                      levels = c("Female_<30","Female_30-39","Female_40-49", "Female_50-59",
                                 "Female_60-69", "Female_70-79","Female_80-89","Female_>=90",
                                 "Male_<30","Male_30-39","Male_40-49", "Male_50-59",
                                 "Male_60-69", "Male_70-79","Male_80-89","Male_>=90"))) 

# drop if missing observation period end date ----
Pop<-Pop %>% 
  filter(!is.na(observation_period_end_date))

# add prior observation time -----
Pop<-Pop %>%  
  mutate(prior_obs_days=as.numeric(difftime(cohort_start_date,
                                            observation_period_start_date,
                                            units="days"))) %>% 
  mutate(prior_obs_years=prior_obs_days/365)

# make sure all have year of prior history ---
Pop<-Pop %>%
  filter(prior_obs_years>=1)

# need to make new end of observation period to 1/1/2019 ----
Pop<-Pop %>% 
  mutate(observation_period_end_date_2019 = ifelse(observation_period_end_date >= '2019-01-01', '2019-01-01', NA)) %>%
  mutate(observation_period_end_date_2019 = as.Date(observation_period_end_date_2019) ) %>%
  mutate(observation_period_end_date_2019 = coalesce(observation_period_end_date_2019, observation_period_end_date))
  
 
# binary death outcome (for survival) ---
# need to take into account follow up
# if death date is > 1/1/2019 set death to 0
Pop<-Pop %>% 
  mutate(status= ifelse(!is.na(death_date), 2, 1 )) %>%
  mutate(status= ifelse(death_date > observation_period_end_date_2019 , 1, status )) %>% 
  mutate(status= ifelse(is.na(status), 1, status ))

# calculate follow up in years
Pop<-Pop %>%  
  mutate(time_days=as.numeric(difftime(observation_period_end_date_2019,
                                            cohort_start_date,
                                            units="days"))) %>% 
#  mutate(time_years=time_days/365) 
mutate(time_years=time_days/365) 


# remove people with end of observation end date == cohort entry
Pop<-Pop %>%
  filter(time_days != 0)


#plotting frequency of cancers for QC checks --

cancernumb <- as.data.frame(table(Pop$cohort_definition_id))
cancernumb$name <- gsub("Cancer", "", outcome_cohorts$cohortName)
cancernumb$name <- gsub("MaleOnly", "", outcome_cohorts$cohortName)

p<-ggplot(data=cancernumb, aes(x=name, y=Freq)) +
  geom_bar(stat="identity", fill = "cadetblue2") +
  geom_text(aes(label=Freq), vjust=0.5, hjust = 0.8, color="black", size=3.5) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  coord_flip()+
  theme_bw()


plotname <- paste0("QCSampleNumbers", db.name,".pdf")

pdf(here("3_QCPlots", plotname),
    width = 7, height = 5)
print(p, newpage = FALSE)
dev.off()

# plot numbers by gender
gendern <- Pop %>%
  group_by(cohort_definition_id, gender) %>%
  tally() %>% 
  rename(name = cohort_definition_id) %>%
  inner_join(outcome_cohorts[,c(1:2)], by = c("name" = "cohortId")) %>%
  mutate(cohortName = str_replace_all(cohortName, 'Cancer', '')) %>%
  mutate(cohortName = str_replace_all(cohortName, 'MaleOnly', '')) %>%
  collect()

q <- gendern %>%
  ggplot(aes(fill = gender, y = n, x = as.factor(cohortName) )) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

plotname <- paste0("QCSampleGenderStrat", db.name,".pdf")

pdf(here("3_QCPlots", plotname),
    width = 7, height = 5)
print(q, newpage = FALSE)
dev.off()

# Functions for analysis -----

# get the risk table
RiskSetCount <- function(timeindex, survivaltime) {
  atrisk <- NULL
  for (t in timeindex)
    atrisk <- c(atrisk, sum(survivaltime >= t))
  return(atrisk)
}

# hazard function over time extraction ----
as.data.frame.bshazard <- function(x, ...) {
  with(x, data.frame(time,hazard,lower.ci,upper.ci))
}

# measuring time in minutes using tictoc package
toc_min <- function(tic,toc,msg="") {
  mins <- round((((toc-tic)/60)),2)
  outmsg <- paste0(mins, " minutes elapsed")
}

# Setting up information for extrapolation methods to be used
extrapolations <- c("gompertz", "weibull", "weibullph" , "exp", "llogis", "lnorm", "gengamma", "spline1", "spline3", "spline5") 
extrapolations_formatted <- c("Gompertz", "Weibull", "WeibullPH" ,"Exponential", "Log-logistic", "Log-normal", "Generalised Gamma", "Spline (1 knot)", "Spline (3 knots)", "Spline (5 knots)")
# setting up time for extrapolation
#t <- seq(0, timeinyrs*365, by=1)
t <- seq(0, timeinyrs*365, by=50) # just for debugging 


#Run analysis ----
#whole population
# info(logger, 'RUNNING ANALYSIS FOR WHOLE POPULATION')
# source(here("2_Analysis","Analysis.R"))
# info(logger, 'ANALYSIS RAN FOR WHOLE POPULATION')


#gender stratification
# if(RunGenderStrat == TRUE){
# 
#   info(logger, 'RUNNING ANALYSIS FOR GENDER STRATIFICATION')
#   source(here("2_Analysis","AnalysisGenderStrat.R"))
#   info(logger, 'ANALYSIS RAN FOR GENDER STRAT')
#   
# }

#age stratification
# if(RunAgeStrat == TRUE){
#   
#   info(logger, 'RUNNING ANALYSIS FOR AGE STRATIFICATION')
#   source(here("2_Analysis","AnalysisAgeStrat.R"))
#   info(logger, 'ANALYSIS RAN FOR AGE STRAT')
#   
# }

#age*gender stratification
# if(RunGenderStrat == TRUE & RunAgeStrat == TRUE ){
#   
#   info(logger, 'RUNNING ANALYSIS FOR AGE*GENDER STRATIFICATION')
#   source(here("2_Analysis","AnalysisAgeGenderStrat.R"))
#   info(logger, 'ANALYSIS RAN FOR AGE STRAT')
#   
# }



# # Tidy up results and save ----

# extract results from all and gender and age stratifications and save them as a RData file


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
# print("-- If all has worked, there should now be a zip folder with your results in the results to share")
# print("-- Thank you for running the study!")
# Sys.time()-start
# # readLines(log_file)

