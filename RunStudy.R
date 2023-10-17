# calculating the number of years of extrapolation for your database ----
# amount of followup in your database plus 10 years
# so if your databases has 20 years of followup you would be 30 here
timeinyrs <- as.numeric(floor(((as.Date("2019-12-31") - as.Date(startdate)) / 365))) + 10

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
cancerconcepts <- CodelistGenerator::codesFromCohort(
  path = here("1_InstantiateCohorts", "Cohorts" ) ,
  cdm = cdm,
  withConceptDetails = FALSE)


# read the cohorts using CDM connector (need this for analysis) ----
outcome_cohorts <- CDMConnector::readCohortSet(here(
  "1_InstantiateCohorts",
  "Cohorts" 
))

# Other settings ---- for troubleshooting
# if you have instantiated outcome cohorts already set this to TRUE
instantiatedCancerCohorts <- FALSE

if(instantiatedCancerCohorts == TRUE){
  
cdm <- CDMConnector::cdm_from_con(con = db,
                                    cdm_schema = cdm_database_schema,
                                    write_schema = results_database_schema,
                                    cohort_tables = c(outcome))  
  
  
} else {
#create a cdm reference for your cohorts ----
  # using generate concept cohort set
cdm <- CDMConnector::generateConceptCohortSet(
  cdm,
  conceptSet = cancerconcepts,
  name = "outcome",
  limit = "first",
  requiredObservation = c(365, 0),
  end = "observation_period_end_date",
  overwrite = TRUE )

}


info(logger, "SUBSETTING CDM")
cdm <- cdmSubsetCohort(cdm, "outcome")
info(logger, "SUBSETTED CDM")

# instantiate exclusion
info(logger, "INSTANTIATE EXCLUSION ANY MALIGNANT NEOPLASTIC DISEASE (EX SKIN CANCER)")

codelistExclusion <- codesFromConceptSet(here("1_InstantiateCohorts", "Exclusion"), cdm)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                              conceptSet = codelistExclusion, 
                                              name = "exclusion",
                                              overwrite = TRUE)

info(logger, "INSTANTIATED EXCLUSION ANY MALIGNANT NEOPLASTIC DISEASE (EX SKIN CANCER)")

# use patient profiles to create a flag of anyone with MALIGNANT NEOPLASTIC DISEASE (excluding skin cancer) prior to cancer diagnoses in our cohorts
cdm$outcome <- cdm$outcome %>% 
  addCohortIntersect(
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



# get variables for analysis ---
cdm$analysis <- cdm$outcome %>% 
# this section uses patient profiles to add in age and age groups as well as
# sex and prior history
  addDemographics(
    age = TRUE,
    ageName = "age",
    ageGroup =  list(
      "age_gr" =
        list(
          "18 to 29" = c(18, 29),
          "30 to 39" = c(30, 39),
          "40 to 49" = c(40, 49),
          "50 to 59" = c(50, 59),
          "60 to 69" = c(60, 69),
          "70 to 79" = c(70, 79),
          "80 to 89" = c(80, 89),
          "> 90" = c(90, 150)
        )
    )
  ) %>% 

  # this section adds in date of death, removes those with a diagnosis outside the study period and
  # date.
  # Also code sets the end date 31 dec 19 for those with observation period past this date
  # and removes death date for people with death past dec 2019 (end of study period)
  
  left_join(cdm$death %>% 
              select("person_id",  "death_date") %>% 
              distinct(),
            by = c("subject_id"= "person_id")) %>% 
  left_join(cdm$observation_period %>% 
              select("person_id",  "observation_period_end_date") %>% 
              distinct(),
            by = c("subject_id"= "person_id")) %>% 
  compute_query() %>% 
  filter(cohort_start_date >= startdate) %>% 
  filter(cohort_start_date <= '2019-12-31') %>% 
  mutate(observation_period_end_date_2019 = ifelse(observation_period_end_date >= '2019-12-31', '2019-12-31', NA)) %>%
  mutate(observation_period_end_date_2019 = as.Date(observation_period_end_date_2019) ) %>%
  mutate(observation_period_end_date_2019 = coalesce(observation_period_end_date_2019, observation_period_end_date)) %>% 
  mutate(status = death_date) %>% 
  mutate(status = ifelse(death_date > '2019-12-31', NA, status)) %>% 
  mutate(status = ifelse(death_date > observation_period_end_date_2019, NA, status)) %>% 
  mutate(status = ifelse(is.na(status), 1, 2 )) %>% 
  mutate(time_days = observation_period_end_date_2019 - cohort_start_date ) %>% 
  mutate(time_years=time_days/365) %>% 
  filter(age_gr != "None") %>% 
  mutate(sex_age_gp = str_c(age_gr, sex, sep = "_"),
         future_observation = time_days) %>%
  rename(anymalignacy = flag_cancerexcludnonmelaskincancer_minf_to_m1 ) %>% 
  
  compute_query()

# use this to show the SQL code
#%>% 
  #show_query()

# remove females from prostate cancer cohort (misdiagnosis)
# get cohort definition id for prostate cancer
prostateID <- outcome_cohorts %>% 
  filter(outcome_cohorts$cohort_name == "IncidentProstateCancer") %>% 
  select(cohort_definition_id) %>% 
  as.numeric()

# remove females from prostate cancer cohort (misdiagnosis)
cdm$analysis <- cdm$analysis %>% 
  filter(!(sex == "Female" & cohort_definition_id == prostateID))


# remove those with any a prior malignancy (apart from skin cancer in prior history)
cdm$analysis <- cdm$analysis %>% 
  filter(anymalignacy != 1)

#update the attrition
cdm$analysis <- recordCohortAttrition(cohort = cdm$analysis,
                                      reason="Exclude patients with any prior history of maglinancy (ex skin cancer)" )


# remove those with date of death and cancer diagnosis on same date
cdm$analysis <- cdm$analysis %>% 
  filter(time_days != 0)

cdm$analysis <- recordCohortAttrition(cohort = cdm$analysis,
                                       reason="Exclude patients with death date same as cancer diagnosis date" )

# take the first cancer in history to make sure incident cases
cdm$analysis <- cdm$analysis %>% 
  group_by(subject_id) %>%
  slice_min(order_by = c(cohort_start_date)) %>%
  ungroup() %>% 
  compute_query()

# update cohort attrition table with reason
cdm$analysis <- recordCohortAttrition(cohort = cdm$analysis,
                                       reason="Excluding patients with other selected cancers" )

# remove any people who have multiple cancer diagnosis on the same day
cdm$analysis <- cdm$analysis %>% 
  group_by(subject_id) %>% 
  filter( n() == 1 ) %>% 
  ungroup() %>% 
  compute_query()

cdm$analysis <- recordCohortAttrition(cohort = cdm$analysis,
                                      reason="Exclude patients with multiple cancers on different sites diagnosed on same day" )

# collect to use for analysis
Pop <- cdm$analysis %>% collect() 

#plotting frequency of cancers for QC checks --
cancernumb <- as.data.frame(table(Pop$cohort_definition_id))
cancernumb$name <- gsub("Cancer", "", outcome_cohorts$cohort_name)
cancernumb$name <- gsub("MaleOnly", "", outcome_cohorts$cohort_name)

p <-ggplot(data=cancernumb, aes(x=name, y=Freq)) +
  geom_bar(stat="identity", fill = "cadetblue2") +
  geom_text(aes(label=Freq), vjust=0.5, hjust = 0.8, color="black", size=3.5) +
theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  coord_flip()+
  theme_bw()


plotname <- paste0("QCSampleNumbers", db.name,".pdf")

pdf(here(qc.plots.folder, plotname),
    width = 7, height = 5)
print(p, newpage = FALSE)
dev.off()

# plot numbers by gender
sexn <- Pop %>%
  group_by(cohort_definition_id, sex) %>%
  tally() %>% 
  rename(name = cohort_definition_id) %>%
  inner_join(outcome_cohorts[,c(1:2)], by = c("name" = "cohort_definition_id")) %>%
  mutate(cohort_name = str_replace_all(cohort_name, 'Cancer', '')) %>%
  mutate(cohort_name = str_replace_all(cohort_name, 'MaleOnly', '')) %>%
  collect()

q <- sexn %>%
  ggplot(aes(fill = sex, y = n, x = as.factor(cohort_name) )) +
  geom_bar(position = "dodge", stat = "identity") +
  xlab("Cancer") +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

plotname <- paste0("QCSampleGenderStrat", db.name,".pdf")

pdf(here(qc.plots.folder, plotname),
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

nice.num1<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}

nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}

# Setting up information for extrapolation methods to be used ---
extrapolations <- c("gompertz", "weibullph" , "exp", "llogis", "lnorm", "gengamma", "spline1", "spline3", "spline5") 
extrapolations_formatted <- c("Gompertz", "WeibullPH" ,"Exponential", "Log-logistic", "Log-normal", "Generalised Gamma", "Spline (1 knot)", "Spline (3 knots)", "Spline (5 knots)")


# setting up time for extrapolation ----
t <- seq(0, timeinyrs*365.25, by=90) # can make smaller can plot to see if it affects results

#Run analysis ----
# set up so notation doesnt include scientific
options(scipen = 999)

#whole population
info(logger, 'RUNNING ANALYSIS FOR WHOLE POPULATION')
source(here("2_Analysis","Analysis.R"))
info(logger, 'ANALYSIS RAN FOR WHOLE POPULATION')

#sex analysis
info(logger, 'RUNNING ANALYSIS FOR SEX')
source(here("2_Analysis","AnalysisSex.R"))
info(logger, 'ANALYSIS RAN FOR SEX')

#age analysis
info(logger, 'RUNNING ANALYSIS FOR AGE')
source(here("2_Analysis","AnalysisAge.R"))
info(logger, 'ANALYSIS RAN FOR AGE')

#set option back to zero
options(scipen = 0)

#running tableone characterisation
info(logger, 'RUNNING TABLE ONE ANALYSIS')
source(here("2_Analysis","Tableone.R"))
info(logger, 'TABLE ONE ANALYSIS RAN')
  
##################################################################

# Tidy up results and save ----

# survival KM and extrapolated data -----
survivalResults <- bind_rows(
  observedkmcombined ,  
  observedkmcombined_sex , 
  observedkmcombined_sexA , 
  observedkmcombined_age , 
  observedkmcombined_ageA , 
  extrapolatedfinal,
  extrapolatedfinalsex,
  extrapolatedfinalsexS,
  extrapolatedfinalage,
  extrapolatedfinalageS
) %>%
  mutate(Database = db.name) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)), Sex, "Male")) %>% 
  select(!c(n.risk, n.event, n.censor, std.error)) %>% 
  filter(time != 0)

#risk table ----
riskTableResults <- bind_rows(
  risktableskm , 
  risktableskm_sex , 
  risktableskm_sexA , 
  risktableskm_age ,
  risktableskm_ageA 
  ) %>%
  mutate(Database = db.name) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)), Sex, "Male"))


# median results KM and predicted median and mean extrapolations ----
medianResults <- bind_rows( 
  medkmcombined ,
  medkmcombined_sex , 
  medkmcombined_sexA , 
  medkmcombined_age ,
  medkmcombined_ageA ,
  predmedmeanfinal,
  predmedmeanfinalsexS,
  predmedmeanfinalageS) %>%
  mutate(Database = db.name) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)), Sex, "Male")) %>% 
  select(!c(n.max, n.start))

# hazard over time results -----
hazOverTimeResults <- bind_rows( 
  hotkmcombined , 
  hotkmcombined_sex, 
  hotkmcombined_sexA, 
  hotkmcombined_age, 
  hotkmcombined_ageA,
  hazardotfinal, 
  hazardotfinalsex, 
  hazardotfinalsexS,
  hazardotfinalage,
  hazardotfinalageS
) %>%
  mutate(Database = db.name) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)), Sex,  "Male"))

# GOF results for extrapolated results (adjusted and stratified)
GOFResults <- bind_rows( 
  goffinal,
  goffinalsex, 
  goffinalsexS,
  goffinalage,
  goffinalageS
) %>%
  mutate(Database = db.name) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)), Sex, "Male")) %>% 
  select(!c(N, events, censored))


# parameters of the extrapolated models
ExtrpolationParameters <-bind_rows(
  parametersfinal ,
  parametersfinalsex,
  parametersfinalsexS,
  parametersfinalage,
  parametersfinalageS
) %>%
  mutate(Database = db.name) %>%
  relocate(Cancer, Method, Stratification, Adjustment, Sex, Age, Database) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)), Sex, "Male"))

# survival probabilities km and predicted probabilities
survivalProbabilities <- bind_rows(
  survprobtablekm ,
  survprobtablekm_sex ,
  survprobtablekm_sexA ,
  survprobtablekm_age,
  survprobtablekm_ageA,
  predsurvivalprobfinal,
  predsurvivalprobfinalsex,
  predsurvivalprobfinalage,
  predsurvivalprobfinalsexS,
  predsurvivalprobfinalageS
) %>%
  mutate(Database = db.name) %>%
  relocate(Cancer, Method, Stratification, Adjustment, Sex, Age, Database) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)), Sex, "Male")) %>% 
  filter(time == 1.0 | time == 5.0 | time == 10.0)


# add a render file for the shiny app for filtering ----
CancerStudied <- c("IncidentBreastCancer" , "IncidentColorectalCancer"  , 
                   "IncidentHeadNeckCancer"  , "IncidentLiverCancer" ,
                   "IncidentLungCancer", "IncidentPancreaticCancer"  ,
                   "IncidentProstateCancer", "IncidentStomachCancer" )
Method <- c("Kaplan-Meier", extrapolations_formatted)
SexStudied <- (rep(rep(c("Male", "Female"), each = length(Method)), length(CancerStudied)))
AgeStudied <- (rep(rep(c("> 90" , "18 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80 to 89"), each = length(Method)), length(CancerStudied)))


# what has been run
runs <- survivalProbabilities %>% 
  select(c("Cancer",
            "Method" ,
            "Stratification",
            "Adjustment",
            "Sex",
            "Age" )) %>% 
  distinct() %>% 
  mutate(Run = "Yes") %>% 
  unite(ID, c( Cancer, Method, Age, Sex, Adjustment, Stratification ), remove = FALSE) %>% 
  select(c(ID, Run))

# ALL
AnalysisRunAll <- tibble(
  Cancer = rep(CancerStudied, each = length(Method)),
  Method = rep(Method, length(CancerStudied)),
  Age = rep("All", by = (length(CancerStudied)*length(Method))),
  Sex = rep("Both", by = (length(CancerStudied)*length(Method))),
  Adjustment = rep("None", by = (length(CancerStudied)*length(Method))),
  Stratification = rep("None", by = (length(CancerStudied)*length(Method))) ) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)),Sex, "Male"))

# SEX STRATIFICATION
AnalysisRunSexS <- tibble(
  Cancer = rep(CancerStudied, each = (length(Method)*2)),
  Method = rep(Method, (length(CancerStudied)*2)),
  Age = rep("All", by = ((length(CancerStudied))*(length(Method))*2)),
  Sex = SexStudied,
  Adjustment = rep("None", by = ((length(CancerStudied))*(length(Method))*2)),
  Stratification = rep("Sex", by = ((length(CancerStudied))*(length(Method))*2))) %>% 
  filter(Cancer != "IncidentProstateCancer")

# SEX ADJUSTED
AnalysisRunSexA <- tibble(
  Cancer = rep(CancerStudied, each = (length(Method)*2)),
  Method = rep(Method, (length(CancerStudied)*2)),
  Age = rep("All", by = ((length(CancerStudied))*(length(Method))*2)),
  Sex = SexStudied,
  Stratification = rep("None", by = ((length(CancerStudied))*(length(Method))*2)),
  Adjustment = rep("Sex", by = ((length(CancerStudied))*(length(Method))*2))) %>% 
  filter(Cancer != "IncidentProstateCancer")

# AGE STRATIFICATION
AnalysisRunAgeS <- tibble(
  Cancer = rep(CancerStudied, each = (length(Method)*8)),
  Method = rep(Method, (length(CancerStudied)*8)),
  Sex = rep("Both", by = ((length(CancerStudied))*(length(Method))*8)),
  Age = AgeStudied,
  Adjustment = rep("None", by = ((length(CancerStudied))*(length(Method))*8)),
  Stratification = rep("Age", by = ((length(CancerStudied))*(length(Method))*8))) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)),Sex, "Male"))

# AGE ADJUSTED
AnalysisRunAgeA <- tibble(
  Cancer = rep(CancerStudied, each = (length(Method)*8)),
  Method = rep(Method, (length(CancerStudied)*8)),
  Sex = rep("Both", by = ((length(CancerStudied))*(length(Method))*8)),
  Age = AgeStudied,
  Stratification = rep("None", by = ((length(CancerStudied))*(length(Method))*8)),
  Adjustment = rep("Age", by = ((length(CancerStudied))*(length(Method))*8))) %>% 
  mutate(Sex = if_else(!(grepl("IncidentProstateCancer", Cancer, fixed = TRUE)),Sex, "Male"))

# combine results
AnalysisRunSummary <- bind_rows(AnalysisRunAll,
                                AnalysisRunSexS ,
                                AnalysisRunSexA,
                                AnalysisRunAgeS,
                                AnalysisRunAgeA ) %>% 
  unite(ID, c( Cancer, Method, Age, Sex, Adjustment, Stratification ), remove = FALSE)


# combine with what has been run to get a rendered file of results summary
AnalysisRunSummary <- 
  left_join(AnalysisRunSummary , runs, by = "ID") %>% 
  select(!c(ID)) %>% 
  mutate(Database = cdm_name(cdm))

# snapshot the cdm
snapshotcdm <- snapshot(cdm)

#get attrition for the cohorts and add cohort identification
attritioncdm <- cohort_attrition(cdm$analysis) %>% 
  left_join(outcome_cohorts, 
            by = join_by(cohort_definition_id),
            relationship = "many-to-many",
            keep = FALSE
            ) %>% 
  select(!c(cohort, json)) %>% 
  relocate(cohort_name)

# save results as csv for data partner can review
info(logger, "SAVING RESULTS")
write_csv(survivalResults, paste0(here(output.folder),"/", cdm_name(cdm), "_survival_estimates.csv"))
write_csv(riskTableResults, paste0(here(output.folder),"/", cdm_name(cdm), "_risk_table.csv"))
write_csv(medianResults, paste0(here(output.folder),"/", cdm_name(cdm), "_median_mean_survival.csv"))
write_csv(hazOverTimeResults, paste0(here(output.folder),"/", cdm_name(cdm), "_hazard_overtime.csv"))
write_csv(GOFResults, paste0(here(output.folder),"/", cdm_name(cdm), "_goodness_of_fit.csv"))
write_csv(ExtrpolationParameters, paste0(here(output.folder),"/", cdm_name(cdm), "_extrapolation_parameters.csv"))
write_csv(survivalProbabilities, paste0(here(output.folder),"/", cdm_name(cdm), "_survival_probabilities.csv"))
write_csv(AnalysisRunSummary, paste0(here(output.folder),"/", cdm_name(cdm), "_analyses_run_summary.csv"))
write_csv(tableone, paste0(here(output.folder),"/", cdm_name(cdm), "_tableone_summary.csv"))
write_csv(snapshotcdm, paste0(here(output.folder),"/", cdm_name(cdm), "_cdm_snapshot.csv"))
write_csv(attritioncdm, paste0(here(output.folder),"/", cdm_name(cdm), "_cohort_attrition.csv"))
info(logger, "SAVED RESULTS")

# # Time taken
x <- abs(as.numeric(Sys.time()-start, units="secs"))

info(logger, paste0("Study took: ",
                    sprintf("%02d:%02d:%02d:%02d",
                            x %/% 86400,  x %% 86400 %/% 3600, x %% 3600 %/%
                              60,  x %% 60 %/% 1)))

# zip results
print("Zipping results to output folder")

zip::zip(
zipfile = here(output.folder, paste0("Results_", cdmName(cdm), ".zip")),
files = list.files(output.folder),
root = output.folder)

# for saving in rds format for dashboard
survival_study_results <- list(survivalResults ,
                               riskTableResults,
                               medianResults ,
                               hazOverTimeResults,
                               GOFResults,
                               ExtrpolationParameters,
                               survivalProbabilities,
                               AnalysisRunSummary,
                               tableone,
                               snapshotcdm,
                               attritioncdm)

names(survival_study_results) <- c(paste0("survival_estimates"),
                                   paste0("risk_table_results"),
                                   paste0("median_survival_results"),
                                   paste0("hazard_overtime_results"),
                                   paste0("goodness_of_fit_results"),
                                   paste0("extrapolation_parameters"),
                                   paste0("survival_probabilities"),
                                   paste0("analyses_run_summary"),
                                   paste0("tableone_summary"),
                                   paste0("cdm_snapshot"),
                                   paste0("cohort_attrition"))


saveRDS(survival_study_results, file = paste0(here::here("shiny", "data"), "/Results.rds"))

print("Study done!")
print(paste0("Study took: ",
                         sprintf("%02d:%02d:%02d:%02d",
                                 x %/% 86400,  x %% 86400 %/% 3600, x %% 3600 %/%
                                   60,  x %% 60 %/% 1)))
print("-- If all has worked, there should now be a zip folder with your results in the Results folder to share")
print("-- Thank you for running the study! :)")

Sys.time()-start

readLines(log_file)