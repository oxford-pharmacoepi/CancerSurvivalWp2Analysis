# calculating the number of years of extrapolation for your database ----
# amount of followup in your database plus 10 years
timeinyrs <- as.numeric(floor(((lubridate::as_date("2019-12-31") - lubridate::as_date(startdate)) / 365))) + 10

#Create folder for the results
if (!file.exists(output.folder)){
  dir.create(output.folder, recursive = TRUE)}

#start the clock
start<-Sys.time()

# start log ----
log_file <- paste0(output.folder, "/", db.name, "_log.txt")
logger <- create.logger()
logfile(logger) <- log_file
level(logger) <- "INFO"

# create study cohorts ----

# get concept sets from cohorts----
cancer_concepts <- CodelistGenerator::codesFromCohort(
  path = here::here("1_InstantiateCohorts", "Cohorts" ) ,
  cdm = cdm,
  withConceptDetails = FALSE)


# instantiate the cohorts with no prior history 
cdm <- CDMConnector::generateConceptCohortSet(
  cdm,
  conceptSet = cancer_concepts,
  name = "outcome",
  limit = "first",
  requiredObservation = c(0, 0),
  end = "observation_period_end_date",
  overwrite = TRUE )



if(priorhistory == TRUE){
  
  # add in prior history
  cdm$outcome <- cdm$outcome %>% 
    PatientProfiles::addPriorObservation(
      cdm = cdm,
      indexDate = "cohort_start_date")
  
  #for those with prior history remove those with less than 365 days of prior history
  cdm$outcome <- cdm$outcome %>% 
    filter(prior_observation >= 365) %>% 
    select(-c(prior_observation))
  
}

cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                   reason="Excluded patients with less than 365 prior history" )


info(logger, "SUBSETTING CDM")
cdm <- CDMConnector::cdmSubsetCohort(cdm, "outcome")
info(logger, "SUBSETTED CDM")

# instantiate exclusion any prior history of malignancy
info(logger, "INSTANTIATE EXCLUSION ANY MALIGNANT NEOPLASTIC DISEASE (EX SKIN CANCER)")

codelistExclusion <- CodelistGenerator::codesFromConceptSet(here::here("1_InstantiateCohorts", "Exclusion"), cdm)
# add cancer concepts to exclusion concepts to make sure we capture all exclusions
codelistExclusion <- list(unique(Reduce(union_all, c(cancer_concepts, codelistExclusion))))

#rename list of concepts
names(codelistExclusion) <- "anymalignancy"

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = codelistExclusion,
                                              name = "exclusion",
                                              overwrite = TRUE)

info(logger, "INSTANTIATED EXCLUSION ANY MALIGNANT NEOPLASTIC DISEASE (EX SKIN CANCER)")

# create a flag of anyone with MALIGNANT NEOPLASTIC DISEASE (excluding skin cancer) prior to cancer diagnoses in our cohorts
cdm$outcome <- cdm$outcome %>%
  PatientProfiles::addCohortIntersect(
    cdm = cdm,
    targetCohortTable = "exclusion",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    flag = TRUE,
    count = FALSE,
    date = FALSE,
    days = FALSE,
    window = list(c(-Inf, -1))
  )

# get data variables
cdm$outcome <- cdm$outcome %>%
  # this section uses patient profiles to add in age and age groups as well as
  # sex and prior history
  PatientProfiles::addDemographics(
    age = TRUE,
    ageName = "age",
    ageGroup =  list(
      "age_gr" =
        list(
          "18 to 39" = c(18, 39),
          "40 to 49" = c(40, 49),
          "50 to 59" = c(50, 59),
          "60 to 69" = c(60, 69),
          "70 to 79" = c(70, 79),
          "80 +" = c(80, 150)
        )
    )
  ) %>%
  
  # this section adds in date of death, removes those with a diagnosis outside the study period and
  # date.
  # Also code sets the end date 31 dec 19 for those with observation period past this date
  # and removes death date for people with death past dec 2019 (end of study period)
  
  dplyr::left_join(cdm$death %>%
                     select("person_id",  "death_date") %>%
                     distinct(),
                   by = c("subject_id"= "person_id")) %>%
  dplyr::left_join(cdm$observation_period %>%
                     select("person_id",  "observation_period_end_date") %>%
                     distinct(),
                   by = c("subject_id"= "person_id")) %>%
  CDMConnector::computeQuery() %>%
  dplyr::filter(cohort_start_date >= startdate) %>%
  dplyr::filter(cohort_start_date <= '2019-12-31') %>%
  dplyr::mutate(observation_period_end_date_2019 = ifelse(observation_period_end_date >= '2019-12-31', '2019-12-31', NA)) %>%
  dplyr::mutate(observation_period_end_date_2019 = as.Date(observation_period_end_date_2019) ) %>%
  dplyr::mutate(observation_period_end_date_2019 = ifelse(is.na(observation_period_end_date_2019), observation_period_end_date, observation_period_end_date_2019 )) %>%
  dplyr::mutate(status = death_date) %>%
  dplyr::mutate(status = ifelse(death_date > '2019-12-31', NA, status)) %>%
  dplyr::mutate(status = ifelse(death_date > observation_period_end_date_2019, NA, status)) %>%
  dplyr::mutate(status = ifelse(is.na(status), 1, 2 )) %>%
  dplyr::mutate(time_days = observation_period_end_date_2019 - cohort_start_date ) %>%
  dplyr::mutate(time_years = time_days / 365) %>%
  dplyr::filter(age_gr != "None") %>%
  dplyr::mutate(sex_age_gp = str_c(age_gr, sex, sep = "_" ),
                future_observation = time_days) %>%
  dplyr::rename(anymalignancy = flag_anymalignancy_minf_to_m1 ) %>%
  CDMConnector::computeQuery()

# # see if there is prostate cancer in database then run this code and put in both if statements
# # remove females from prostate cancer cohort (misdiagnosis)
# # get cohort definition id for prostate cancer
if( "Prostate" %in% names(cancer_concepts) == TRUE){
  
  prostateID <- CDMConnector::cohortSet(cdm$outcome) %>%
    dplyr::filter(cohort_name == "Prostate") %>%
    dplyr::pull("cohort_definition_id") %>%
    as.numeric()
  
  # remove females from prostate cancer cohort (misdiagnosis)
  cdm$outcome <- cdm$outcome %>%
    dplyr::filter(!(sex == "Female" & cohort_definition_id == prostateID))
}

# remove those with any a prior malignancy (apart from skin cancer in prior history)
cdm$outcome <- cdm$outcome %>%
  dplyr::filter(anymalignancy != 1)

#update the attrition
cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                   reason="Exclude patients with any prior history of maglinancy (ex skin cancer)" )


# remove those with date of death and cancer diagnosis on same date
cdm$outcome <- cdm$outcome %>% 
  dplyr::filter(time_days > 0)

cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                    reason="Exclude patients with death date same as cancer diagnosis date" )


# remove any people who have multiple cancer diagnosis on the same day
cdm$outcome <- cdm$outcome %>%
  dplyr::group_by(subject_id, 
           cohort_definition_id,
           cohort_start_date,
           cohort_end_date,
           sex,
           prior_observation,
           future_observation,
           age_gr ,                  
           death_date,
           observation_period_end_date,  
           observation_period_end_date_2019,
           status,                          
           time_days,
           time_years,
           sex_age_gp) %>%
  dplyr::summarise(count = max(1, na.rm = TRUE), .groups = "drop") %>%
  dplyr::filter(count == 1) %>%
  dplyr::ungroup() %>% 
  dplyr::select(!c(count))

cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                    reason="Exclude patients with multiple cancers on different sites diagnosed on same day" )

# only run analysis where we have counts more than 200 ----
cancer_cohorts <- CDMConnector::cohortSet(cdm$outcome) %>%
  dplyr::inner_join(CDMConnector::cohortCount(cdm$outcome), by = "cohort_definition_id") %>%
  dplyr::arrange(cohort_definition_id) %>% 
  dplyr::filter(number_subjects >= 200)

# filter the data to cohorts that have more than 200 patients
id <- cohortCount(cdm$outcome) %>% dplyr::filter(number_subjects >= 200) %>% dplyr::pull("cohort_definition_id")
cdm$outcome <- cdm$outcome %>% filter(cohort_definition_id %in% id)

#update the attrition
cdm$outcome <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome,
                                                   reason="Removing cancer cohorts from analysis with less than 200 patients" )


# collect to use for analysis
Pop <- cdm$outcome %>% dplyr::collect() 

# Setting up information for extrapolation methods to be used ---
extrapolations <- c("gompertz", "weibullph" , "exp", "llogis", "lnorm", "gengamma",
                    "spline1", 
                    "spline3")

extrapolations_formatted <- c("Gompertz", 
                              "WeibullPH" ,
                              "Exponential", 
                              "Log-logistic",
                              "Log-normal",
                              "Generalised Gamma", 
                              "Spline (1 knot)",
                              "Spline (3 knots)")

# Setting up information for extrapolation methods to be used ---
# extrapolations <- c("gompertz", "weibullph" , "exp", "llogis", "lnorm", "gengamma",
#                     "spline1", 
#                     "spline3",
#                     "spline1o", 
#                     "spline3o",
#                     "spline1n", 
#                     "spline3n")
# 
# extrapolations_formatted <- c("Gompertz", 
#                               "WeibullPH" ,
#                               "Exponential", 
#                               "Log-logistic",
#                               "Log-normal",
#                               "Generalised Gamma", 
#                               "Spline Hazard (1 knot)",
#                               "Spline Hazard (3 knots)" ,
#                               "Spline Odds (1 knot)",
#                               "Spline Odds (3 knots)" ,
#                               "Spline Normal (1 knot)",
#                               "Spline Normal (3 knots)")

# setting up time for extrapolation ----
t <- seq(0, timeinyrs*365.25, by=40)

#Run analysis ----

#pick up functions
source(here::here("2_Analysis","Functions.R"))

#whole population
print(paste0("1 of 5: RUNNING ANALYSIS FOR WHOLE POPULATION")) 
info(logger, 'RUNNING ANALYSIS FOR WHOLE POPULATION')
source(here::here("2_Analysis","Analysis.R"))
info(logger, 'ANALYSIS RAN FOR WHOLE POPULATION')
print(paste0("1 of 5: FINISHED ANALYSIS FOR WHOLE POPULATION")) 

#sex analysis
print(paste0("2 of 5: RUNNING ANALYSIS FOR SEX")) 
info(logger, 'RUNNING ANALYSIS FOR SEX')
source(here::here("2_Analysis","AnalysisSex.R"))
info(logger, 'ANALYSIS RAN FOR SEX')
print(paste0("2 of 5: ANALYSIS RAN FOR SEX")) 

#age analysis
print(paste0("3 of 5: RUNNING ANALYSIS FOR AGE")) 
info(logger, 'RUNNING ANALYSIS FOR AGE')
source(here::here("2_Analysis","AnalysisAge.R"))
info(logger, 'ANALYSIS RAN FOR AGE')
print(paste0("3 of 5: ANALYSIS RAN FOR AGE")) 

# age*sex analysis KM only
print(paste0("4 of 5: RUNNING ANALYSIS FOR AGE*SEX ONLY KM")) 
info(logger, 'RUNNING ANALYSIS FOR AGE*SEX ONLY KM')
source(here::here("2_Analysis","AnalysisAgeSex.R"))
info(logger, 'ANALYSIS RAN FOR AGE*SEX ONLY KM')
print(paste0("4 of 5: ANALYSIS RAN FOR AGE*SEX ONLY KM")) 

#running tableone characterisation
print(paste0("5 of 5: RUNNING TABLE ONE CHARACTERISATION")) 
info(logger, 'RUNNING TABLE ONE CHARACTERISATION')
source(here::here("2_Analysis","Tableone.R"))
info(logger, 'TABLE ONE CHARACTERISATION RAN')
print(paste0("5 of 5: TABLE ONE CHARACTERISATION RAN")) 


print(paste0("SAVING RESULTS")) 
##################################################################
# Tidy up results and save ----

# survival KM and extrapolated data -----
survivalResults <- dplyr::bind_rows(
  observedkmcombined ,  
  observedkmcombined_sex , 
  observedkmcombined_sexA , 
  observedkmcombined_age , 
  observedkmcombined_ageA , 
  observedkmcombined_age_sex,
  extrapolatedfinal,
  extrapolatedfinalsex,
  extrapolatedfinalsexS,
  extrapolatedfinalage,
  extrapolatedfinalageS
) %>%
  dplyr::mutate(Database = db.name) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)), Sex, "Male")) %>% 
  dplyr::select(!c(n.risk, n.event, n.censor, std.error)) %>% 
  dplyr::filter(time != 0)

#risk table ----
riskTableResults <- dplyr::bind_rows(
  risktableskm , 
  risktableskm_sex , 
  risktableskm_sexA , 
  risktableskm_age ,
  risktableskm_ageA,
  risktableskm_age_sex
  ) %>%
  dplyr::mutate(Database = db.name) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)), Sex, "Male"))

# KM median results, survival probabilites and predicted from extrapolations ----
medianResults <- dplyr::bind_rows( 
  medkmcombined ,
  medkmcombined_sex , 
  medkmcombined_sexA , 
  medkmcombined_age ,
  medkmcombined_ageA ,
  medkmcombined_age_sex,
  predmedmeanfinal,
  predmedmeanfinalsex,
  predmedmeanfinalsexS,
  predmedmeanfinalage,
  predmedmeanfinalageS) %>%
  dplyr::mutate(Database = db.name) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)), Sex, "Male")) 

# hazard over time results -----
hazOverTimeResults <- dplyr::bind_rows( 
  hotkmcombined , 
  hotkmcombined_sex, 
  hotkmcombined_sexA, 
  hotkmcombined_age, 
  hotkmcombined_ageA,
  hotkmcombined_age_sex,
  hazardotfinal, 
  hazardotfinalsex, 
  hazardotfinalsexS,
  hazardotfinalage,
  hazardotfinalageS
) %>%
  dplyr::mutate(Database = db.name) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)), Sex,  "Male"))


# GOF results for extrapolated results (adjusted and stratified)
GOFResults <- dplyr::bind_rows( 
  goffinal,
  goffinalsex, 
  goffinalsexS,
  goffinalage,
  goffinalageS
) %>%
  dplyr::mutate(Database = db.name) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)), Sex, "Male")) %>% 
  dplyr::select(!c(N, events, censored)) 

# parameters of the extrapolated models
ExtrpolationParameters <- dplyr::bind_rows(
  parametersfinal ,
  parametersfinalsex,
  parametersfinalsexS,
  parametersfinalage,
  parametersfinalageS
) %>%
  dplyr::mutate(Database = db.name) %>%
  dplyr::relocate(Cancer, Method, Stratification, Adjustment, Sex, Age, Database) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)), Sex, "Male"))


# add a render file for the shiny app for filtering ----
CancerStudied <- c("Breast" , "Colorectal"  , 
                   "Head and Neck"  , "Liver" ,
                   "Lung", "Pancreatic"  ,
                   "Prostate", "Stomach" )
Method <- c("Kaplan-Meier", extrapolations_formatted)
SexStudied <- (rep(rep(c("Male", "Female"), each = length(Method)), length(CancerStudied)))
AgeStudied <- (rep(rep(c("80 +" , "18 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79"), each = length(Method)), length(CancerStudied)))


# what has been run
runs <- survivalResults %>% 
  dplyr::select(c("Cancer",
            "Method" ,
            "Stratification",
            "Adjustment",
            "Sex",
            "Age" )) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(Run = "Yes") %>% 
  tidyr::unite(ID, c( Cancer, Method, Age, Sex, Adjustment, Stratification ), remove = FALSE) %>% 
  dplyr::select(c(ID, Run))

# ALL
AnalysisRunAll <- tibble(
  Cancer = rep(CancerStudied, each = length(Method)),
  Method = rep(Method, length(CancerStudied)),
  Age = rep("All", by = (length(CancerStudied)*length(Method))),
  Sex = rep("Both", by = (length(CancerStudied)*length(Method))),
  Adjustment = rep("None", by = (length(CancerStudied)*length(Method))),
  Stratification = rep("None", by = (length(CancerStudied)*length(Method))) ) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)),Sex, "Male"))

# SEX STRATIFICATION
AnalysisRunSexS <- tibble(
  Cancer = rep(CancerStudied, each = (length(Method)*2)),
  Method = rep(Method, (length(CancerStudied)*2)),
  Age = rep("All", by = ((length(CancerStudied))*(length(Method))*2)),
  Sex = SexStudied,
  Adjustment = rep("None", by = ((length(CancerStudied))*(length(Method))*2)),
  Stratification = rep("Sex", by = ((length(CancerStudied))*(length(Method))*2))) %>% 
  dplyr::filter(Cancer != "Prostate")

# SEX ADJUSTED
AnalysisRunSexA <- tibble(
  Cancer = rep(CancerStudied, each = (length(Method)*2)),
  Method = rep(Method, (length(CancerStudied)*2)),
  Age = rep("All", by = ((length(CancerStudied))*(length(Method))*2)),
  Sex = SexStudied,
  Stratification = rep("None", by = ((length(CancerStudied))*(length(Method))*2)),
  Adjustment = rep("Sex", by = ((length(CancerStudied))*(length(Method))*2))) %>% 
  dplyr::filter(Cancer != "Prostate")

# AGE STRATIFICATION
AnalysisRunAgeS <- tibble(
  Cancer = rep(CancerStudied, each = (length(Method)*6)),
  Method = rep(Method, (length(CancerStudied)*6)),
  Sex = rep("Both", by = ((length(CancerStudied))*(length(Method))*6)),
  Age = AgeStudied,
  Adjustment = rep("None", by = ((length(CancerStudied))*(length(Method))*6)),
  Stratification = rep("Age", by = ((length(CancerStudied))*(length(Method))*6))) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)),Sex, "Male"))

# AGE ADJUSTED
AnalysisRunAgeA <- tibble(
  Cancer = rep(CancerStudied, each = (length(Method)*6)),
  Method = rep(Method, (length(CancerStudied)*6)),
  Sex = rep("Both", by = ((length(CancerStudied))*(length(Method))*8)),
  Age = AgeStudied,
  Stratification = rep("None", by = ((length(CancerStudied))*(length(Method))*6)),
  Adjustment = rep("Age", by = ((length(CancerStudied))*(length(Method))*6))) %>% 
  dplyr::mutate(Sex = if_else(!(grepl("Prostate", Cancer, fixed = TRUE)),Sex, "Male"))

# combine results
AnalysisRunSummary <- dplyr::bind_rows(AnalysisRunAll,
                                AnalysisRunSexS ,
                                AnalysisRunSexA,
                                AnalysisRunAgeS,
                                AnalysisRunAgeA ) %>% 
  tidyr::unite(ID, c( Cancer, Method, Age, Sex, Adjustment, Stratification ), remove = FALSE)


# combine with what has been run to get a rendered file of results summary
AnalysisRunSummary <- 
  dplyr::left_join(AnalysisRunSummary , runs, by = "ID") %>% 
  dplyr::select(!c(ID)) %>% 
  dplyr::mutate(Database = cdm_name(cdm),
         Run = ifelse(is.na(Run), "No", Run))


# snapshot the cdm
snapshotcdm <- CDMConnector::snapshot(cdm) %>% 
  mutate(Database = CDMConnector::cdm_name(cdm)) %>% 
  mutate(StudyPeriodStartDate = startdate)

#get attrition for the cohorts and add cohort identification
attritioncdm <- CDMConnector::cohort_attrition(cdm$outcome) %>% 
  dplyr::left_join(
    cohortSet(cdm$outcome) %>% select(c("cohort_definition_id", "cohort_name")),
    by = join_by(cohort_definition_id),
    relationship = "many-to-many",
    keep = FALSE
  ) %>% 
  dplyr::relocate(cohort_name) %>% 
  dplyr::mutate(Database = cdm_name(cdm)) %>% 
  dplyr::rename(Cancer = cohort_name)

# save results as csv for data partner can review
info(logger, "SAVING RESULTS")
readr::write_csv(survivalResults, paste0(here::here(output.folder),"/", cdm_name(cdm), "_survival_estimates.csv"))
readr::write_csv(riskTableResults, paste0(here::here(output.folder),"/", cdm_name(cdm), "_risk_table.csv"))
readr::write_csv(medianResults, paste0(here::here(output.folder),"/", cdm_name(cdm), "_median_mean_survprob_survival.csv"))
readr::write_csv(hazOverTimeResults, paste0(here::here(output.folder),"/", cdm_name(cdm), "_hazard_overtime.csv"))
readr::write_csv(GOFResults, paste0(here::here(output.folder),"/", cdm_name(cdm), "_goodness_of_fit.csv"))
readr::write_csv(ExtrpolationParameters, paste0(here::here(output.folder),"/", cdm_name(cdm), "_extrapolation_parameters.csv"))
readr::write_csv(AnalysisRunSummary, paste0(here::here(output.folder),"/", cdm_name(cdm), "_analyses_run_summary.csv"))
readr::write_csv(tableone_final, paste0(here::here(output.folder),"/", cdm_name(cdm), "_tableone_summary.csv"))
readr::write_csv(snapshotcdm, paste0(here::here(output.folder),"/", cdm_name(cdm), "_cdm_snapshot.csv"))
readr::write_csv(attritioncdm, paste0(here::here(output.folder),"/", cdm_name(cdm), "_cohort_attrition.csv"))
info(logger, "SAVED RESULTS")

# # Time taken
x <- abs(as.numeric(Sys.time()-start, units="secs"))

info(logger, paste0("Study took: ",
                    sprintf("%02d:%02d:%02d:%02d",
                            x %/% 86400,  x %% 86400 %/% 3600, x %% 3600 %/%
                              60,  x %% 60 %/% 1)))

print(paste0("SAVED RESULTS")) 
# zip results
print("Zipping results to output folder")

zip::zip(
zipfile = here::here(output.folder, paste0("Results_", cdmName(cdm), ".zip")),
files = list.files(output.folder),
root = output.folder)

print("Study done!")
print(paste0("Study took: ",
                         sprintf("%02d:%02d:%02d:%02d",
                                 x %/% 86400,  x %% 86400 %/% 3600, x %% 3600 %/%
                                   60,  x %% 60 %/% 1)))
print("-- If all has worked, there should now be a zip folder with your results in the Results folder to share")
print("-- Thank you for running the study! :)")

Sys.time()-start

readLines(log_file)