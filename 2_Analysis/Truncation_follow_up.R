if(db.name == "CRN"){ 
  
  db <- DBI::dbConnect(dbms,
                       dbname = server_dbi,
                       port = port,
                       host = host, 
                       user = user, 
                       password = password)
  
  cdm <- CDMConnector::cdm_from_con(con = db, 
                                    cdm_schema = cdm_database_schema,
                                    write_schema = c("schema" = results_database_schema, 
                                                     "prefix" = table_stem),
                                    cdm_name = db.name)
  
  
}

# instantiate the cohorts with no prior history 
cdm <- CDMConnector::generateConceptCohortSet(
  cdm,
  conceptSet = cancer_concepts,
  name = "outcome_trunc",
  limit = "first",
  requiredObservation = c(0, 0),
  end = 730,
  overwrite = TRUE )

# add in prior history


if(priorhistory == TRUE){
  
  cdm$outcome_trunc <- cdm$outcome_trunc %>% 
    PatientProfiles::addPriorObservation(
      cdm = cdm,
      indexDate = "cohort_start_date")
  
  #for those with prior history remove those with less than 365 days of prior history
  cdm$outcome_trunc <- cdm$outcome_trunc %>% 
    filter(prior_observation >= 365) %>% 
    select(-c(prior_observation))
  
}

cdm$outcome_trunc <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_trunc,
                                                   reason="Excluded patients with less than 365 prior history" )

# this step adds in a filter which only includes patients who are present in IMASIS's tumour registry
# if(db.name == "IMASIS"){
#   
#   cdm$outcome_trunc <- cdm$outcome_trunc %>% 
#     dplyr::left_join(cdm$condition_occurrence %>%
#                        select("person_id",  "condition_type_concept_id") %>%
#                        distinct(),
#                      by = c("subject_id"= "person_id")) %>% 
#     dplyr::filter(condition_type_concept_id == 32879 )
#   
#   cdm$outcome_trunc <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_trunc,
#                                                      reason="Removing patients in registry" )
# }

info(logger, "SUBSETTING CDM")
cdm <- CDMConnector::cdmSubsetCohort(cdm, "outcome_trunc")
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
cdm$outcome_trunc <- cdm$outcome_trunc %>%
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

# remove any patients with other cancers on same date not in our list of cancers
# get the any malignancy codelist
codelistExclusion1 <- CodelistGenerator::codesFromConceptSet(here::here("1_InstantiateCohorts", "Exclusion"), cdm)

# merge all concepts for all cancers together
codes2remove <- list(unique(Reduce(union_all, c(cancer_concepts))))
names(codes2remove) <- "allmalignancy"

# remove lists from our cancers of interest from the any malignancy list
codes2remove <- list(codelistExclusion1$cancerexcludnonmelaskincancer[!codelistExclusion1$cancerexcludnonmelaskincancer %in% codes2remove$allmalignancy])
names(codes2remove) <- "allmalignancy"

#instantiate any malignancy codes minus our cancers of interest
cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm,
                                              conceptSet = codes2remove ,
                                              name = "allmalignancy",
                                              overwrite = TRUE)

# create a flag of anyone with MALIGNANT NEOPLASTIC DISEASE (excluding skin cancer) ON cancer diagnosis date but removing our codes of interest
# in doing so we are capturing people with other cancers on the same day and wont exclude everyone
cdm$outcome_trunc <- cdm$outcome_trunc %>%
  PatientProfiles::addCohortIntersect(
    cdm = cdm,
    targetCohortTable = "allmalignancy",
    targetStartDate = "cohort_start_date",
    targetEndDate = "cohort_end_date",
    flag = TRUE,
    count = FALSE,
    date = FALSE,
    days = FALSE,
    window = list(c(0, 0))
  )


# get data variables
cdm$outcome_trunc <- cdm$outcome_trunc %>%
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
  
  prostateID <- CDMConnector::cohortSet(cdm$outcome_trunc) %>%
    dplyr::filter(cohort_name == "Prostate") %>%
    dplyr::pull("cohort_definition_id") %>%
    as.numeric()
  
  # remove females from prostate cancer cohort (misdiagnosis)
  cdm$outcome_trunc <- cdm$outcome_trunc %>%
    dplyr::filter(!(sex == "Female" & cohort_definition_id == prostateID))
}

#update the attrition after those outside the study period are removed
cdm$outcome_trunc <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_trunc,
                                                   reason="Exclude patients outside study period" )

# remove those with any a prior malignancy (apart from skin cancer in prior history)
cdm$outcome_trunc <- cdm$outcome_trunc %>%
  dplyr::filter(anymalignancy != 1)

#update the attrition
cdm$outcome_trunc <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_trunc,
                                                   reason="Exclude patients with any prior history of maglinancy (ex skin cancer)" )


# remove those with date of death and cancer diagnosis on same date
cdm$outcome_trunc <- cdm$outcome_trunc %>% 
  dplyr::filter(time_days > 0)

cdm$outcome_trunc <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_trunc,
                                                   reason="Exclude patients with death date same as cancer diagnosis date" )

# removes any patients with multiple cancers on same date (just the cancers of interest at the moment)
cdm$outcome_trunc <- cdm$outcome_trunc %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

cdm$outcome_trunc <- cdm$outcome_trunc %>%
  dplyr::filter(flag_allmalignancy_0_to_0 != 1)

cdm$outcome_trunc <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_trunc,
                                                   reason="Exclude patients with multiple cancers on different sites diagnosed on same day" )

# only run analysis where we have counts more than 200 ----
cancer_cohorts <- CDMConnector::cohortSet(cdm$outcome_trunc) %>%
  dplyr::inner_join(CDMConnector::cohortCount(cdm$outcome_trunc), by = "cohort_definition_id") %>%
  dplyr::arrange(cohort_definition_id) %>% 
  dplyr::filter(number_subjects >= 200)

# filter the data to cohorts that have more than 200 patients
id <- cohortCount(cdm$outcome_trunc) %>% dplyr::filter(number_subjects >= 200) %>% dplyr::pull("cohort_definition_id")
cdm$outcome_trunc <- cdm$outcome_trunc %>% filter(cohort_definition_id %in% id)

#update the attrition
cdm$outcome_trunc <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_trunc,
                                                   reason="Removing cancer cohorts from analysis with less than 200 patients" )

# add bespoke code for ECI (Edinburgh cancer registry) to remove males from breast cancer cohort due to ethical approval
if(db.name == "ECI"){
  
  breastID <- CDMConnector::cohortSet(cdm$outcome_trunc) %>%
    dplyr::filter(cohort_name == "Breast") %>%
    dplyr::pull("cohort_definition_id") %>%
    as.numeric()
  
  # remove males from breast cancer cohort
  cdm$outcome_trunc <- cdm$ooutcome_trunc %>% 
    dplyr::filter(sex == "Female" & cohort_definition_id == breastID)
  
  
  cdm$outcome_trunc <- CDMConnector::recordCohortAttrition(cohort = cdm$outcome_trunc,
                                                     reason="Removing male breast cancer patients" )
}

# collect to use for analysis
Pop_truncated <- cdm$outcome_trunc %>% dplyr::collect() 


#################################################
# WHOLE POPULATION
################################################

###########################################

# Extrapolation analysis for whole population ------

tic("Extrapolation analysis for whole population")
info(logger, 'Extrapolation analysis for whole population START')

# Initiate lists to store output ---- 
extrapolations_allt <- list() # extrapolation over time
gof_haz_allt <- list() # goodness of fit
hazot_allt <- list() # hazard over time 
parameters_allt <- list() # parameters from each model
pred_median_mean_allt <- list() # extract the predicted median and RMST, surv prob 1,5,10 from extrapolation methods

# Running analysis for each cancer
for(j in 1:nrow(cancer_cohorts)) {
  
  # create empty lists for temp results for each cancer
  extrap_results_temp <- list() 
  gof_results_temp <- list()
  hazot_results_temp <- list()
  parameters_results_temp <- list() 
  pred_median_mean_results_temp <- list() 
  
  #subset the data by cancer type
  data <- Pop_truncated %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline hazard scale
      tryCatch(
        model <- flexsurv::flexsurvspline (formula=Surv(time_years,status-1)~1,data=data,k = 1, scale = "hazard"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          #extrapolation
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
          
          knots.p <- model[["knots"]] %>%
            stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          # median and mean survival predictions from extrapolation
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          
          # survival predicted probabilities from extrapolations
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Age = "All", 
                          Sex = "Both" )
          
          
          rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
    } else if(extrapolations[i] == "spline1o") {
      # 1knotspline odds
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1)~1 ,data=data, k = 1, scale = "odds"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
          
          knots.p <- model[["knots"]] %>%
            stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          # median and mean survival predictions from extrapolation
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean10yr = est) %>% 
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          # survival predicted probabilities from extrapolations
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Age = "All", 
                          Sex = "Both" )
          
          
          rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
          
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        ) }
      
      
    } else if(extrapolations[i] == "spline1n") {
      # 1knotspline normal scale
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1)~1,data=data, k = 1, scale = "normal"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
          
          knots.p <- model[["knots"]] %>%
            stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          # median and mean survival predictions from extrapolation
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean10yr = est) %>% 
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          # survival predicted probabilities from extrapolations
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Age = "All", 
                          Sex = "Both" )
          
          
          rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
          
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }      
      
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1)~1, data=data, k = 3, scale = "hazard"),
        error = function(e){info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
          
          knots.p <- model[["knots"]] %>%
            stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          # median and mean survival predictions from extrapolation
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time)) 
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean10yr = est) %>% 
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          # survival predicted probabilities from extrapolations
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>%  
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Age = "All", 
                          Sex = "Both")
          
          
          rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
      
    } else if(extrapolations[i] == "spline3o") {
      # 3knotspline odds
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1)~1, data=data, k = 3, scale = "odds"),
        error = function(e){info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
          
          knots.p <- model[["knots"]] %>%
            stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          # median and mean survival predictions from extrapolation
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time)) 
          
          
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean10yr = est) %>% 
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          # survival predicted probabilities from extrapolations
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Age = "All", 
                          Sex = "Both")
          
          
          rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
    } else if(extrapolations[i] == "spline3n") {
      # 3knotspline normal
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1)~1, data=data, k = 3, scale = "normal"),
        error = function(e){info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
          
          knots.p <- model[["knots"]] %>%
            stats::setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p )
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          # median and mean survival predictions from extrapolation
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time)) 
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean10yr = est) %>% 
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          # survival predicted probabilities from extrapolations
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Age = "All", 
                          Sex = "Both")
          
          
          rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
      
    } else {
      
      #carry out models for different parametric methods survival
      tryCatch(
        model <- flexsurv::flexsurvreg(Surv(time_years, status)~1, data=data, dist=extrapolations[i]),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          # extrapolations
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          #grab the parameters from the model
          parameters_results_temp[[i]] <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" ) 
          
          #extract the hazard function over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard", tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All", Sex = "Both" )
          
          # median and mean survival predictions from extrapolation
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean10yr = est) %>% 
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          # survival predicted probabilities from extrapolations
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean, pr_mean5, pr_mean10, pr_median, pr_survival_prob )
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Age = "All", 
                          Sex = "Both" )
          
          
          rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5) 
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for overall model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
    }
    
  }
  
  extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
  gofcombined <- dplyr::bind_rows(gof_results_temp)
  hotcombined <- dplyr::bind_rows(hazot_results_temp)   
  parcombined <- dplyr::bind_rows(parameters_results_temp)
  medcombined <- dplyr::bind_rows(pred_median_mean_results_temp)
  
  extrapolations_allt[[j]] <- extrapolatedcombined
  gof_haz_allt[[j]] <- gofcombined
  hazot_allt[[j]] <- hotcombined
  parameters_allt[[j]] <-  parcombined
  pred_median_mean_allt[[j]] <- medcombined
  
  #print out progress               
  print(paste0(cancer_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalt <- dplyr::bind_rows(extrapolations_allt)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None", Truncated = "Yes")
goffinalt <- dplyr::bind_rows(gof_haz_allt)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None", Truncated = "Yes")
hazardotfinalt <- dplyr::bind_rows(hazot_allt)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None", Truncated = "Yes")
parametersfinalt <- dplyr::bind_rows(parameters_allt)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None", Truncated = "Yes") %>% 
  dplyr::relocate(shape, .after = Sex) %>% 
  dplyr::relocate(rate, .after = Sex) %>% 
  dplyr::mutate(rate = coalesce(rate, `1`)) %>% 
  dplyr::select(!c(`1`))
predmedmeanfinalt <- dplyr::bind_rows(pred_median_mean_allt)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "None", Truncated = "Yes")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for whole population COMPLETED')


#################################################
# SEX ANALYSIS
#################################################
tic("Extrapolation analysis for sex adjustment")

info(logger, 'Extrapolation analysis for sex adjustment START')

# Initiate lists to store output within loop ---- 
extrapolations_sex <- list() # extrapolation over time
gof_haz_sex <- list() # goodness of fit
hazot_sex <- list() # hazard over time 
parameters_sex <- list() # parameters from each model
pred_median_mean_sex <- list() # extract the predicted median and RMST, surv prob 1,5,10 from extrapolation methods

for(j in 1:nrow(cancer_cohorts)) { 
  
  #temp lists to store results
  extrap_results_temp <- list() 
  gof_results_temp <- list() 
  hazot_results_temp <- list() 
  parameters_results_temp <- list()
  pred_median_mean_results_temp <- list()
  
  #subset the data by cancer type
  data <- Pop_truncated %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  sexlevels <- data %>%
    dplyr::group_by(sex) %>% dplyr::summarise(count = n()) %>% dplyr::tally()
  
  # analysis wont run if only 1 sex present
  if(sexlevels == 2){
    
    #carry out extrapolation for each cancer
    for(i in 1:length(extrapolations)) {   # Head of for-loop
      
      if(extrapolations[i] == "spline1") {
        
        # 1knotspline
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ sex ,data=data,k = 1, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], "error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], "warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            #extrapolation
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            # median and mean predicted survival
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean10yr = est) %>%
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean5yr = est) %>%
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "sex" ) %>% 
              dplyr::inner_join(pr_mean10, by = "sex" ) %>% 
              dplyr::inner_join(pr_median, by = "sex" ) %>% 
              dplyr::inner_join(pr_survival_prob, by = "sex")
            
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            rm(model, pr_mean, pr_median, pr_mean5, pr_mean10, pr_survival_prob)
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      } else if(extrapolations[i] == "spline1o") {
        # 1knotspline odds
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ sex , data=data , k = 1, scale = "odds"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All")%>% 
              dplyr::rename(Sex = sex)
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Age = "All" ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All") %>% 
              dplyr::rename(Sex = sex)
            
            # median and mean predicted survival
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean10yr = est) %>%
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean5yr = est) %>%
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "sex" ) %>% 
              dplyr::inner_join(pr_mean10, by = "sex" ) %>% 
              dplyr::inner_join(pr_median, by = "sex" ) %>% 
              dplyr::inner_join(pr_survival_prob, by = "sex")
            
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            rm(model, pr_mean, pr_median, pr_mean5, pr_mean10, pr_survival_prob)
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
      } else if(extrapolations[i] == "spline1n") {
        # 1knotspline odds
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ sex , data=data , k = 1, scale = "normal"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All")%>% 
              dplyr::rename(Sex = sex)
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Age = "All" ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All") %>% 
              dplyr::rename(Sex = sex)
            
            # median and mean predicted survival
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean10yr = est) %>%
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean5yr = est) %>%
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>%  
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "sex" ) %>% 
              dplyr::inner_join(pr_mean10, by = "sex" ) %>% 
              dplyr::inner_join(pr_median, by = "sex" ) %>% 
              dplyr::inner_join(pr_survival_prob, by = "sex")
            
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            rm(model, pr_mean, pr_median, pr_mean5, pr_mean10, pr_survival_prob)
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
      } else if(extrapolations[i] == "spline3") {
        # 3knotspline
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ sex, data=data, k = 3, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All")%>% 
              dplyr::rename(Sex = sex)
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            # median and mean predicted survival
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean10yr = est) %>%
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean5yr = est) %>%
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "sex" ) %>% 
              dplyr::inner_join(pr_mean10, by = "sex" ) %>% 
              dplyr::inner_join(pr_median, by = "sex" ) %>% 
              dplyr::inner_join(pr_survival_prob, by = "sex")
            
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            rm(model, pr_mean, pr_median, pr_mean5, pr_mean10, pr_survival_prob)
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
      } else if(extrapolations[i] == "spline3o") {
        # 3knotspline odds
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ sex, data=data, k = 3, scale = "odds"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All")%>% 
              dplyr::rename(Sex = sex)
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            # median and mean predicted survival
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean10yr = est) %>%
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean5yr = est) %>%
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "sex" ) %>% 
              dplyr::inner_join(pr_mean10, by = "sex" ) %>% 
              dplyr::inner_join(pr_median, by = "sex" ) %>% 
              dplyr::inner_join(pr_survival_prob, by = "sex")
            
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            rm(model, pr_mean, pr_median, pr_mean5, pr_mean10, pr_survival_prob)
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      } else if(extrapolations[i] == "spline3n") {
        # 3knotspline normal
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ sex, data=data, k = 3, scale = "normal"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All")%>% 
              dplyr::rename(Sex = sex)
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            # median and mean predicted survival
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean10yr = est) %>%
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean5yr = est) %>%
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "sex" ) %>% 
              dplyr::inner_join(pr_mean10, by = "sex" ) %>% 
              dplyr::inner_join(pr_median, by = "sex" ) %>% 
              dplyr::inner_join(pr_survival_prob, by = "sex")
            
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            rm(model, pr_mean, pr_median, pr_mean5, pr_mean10, pr_survival_prob)
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      } else {
        
        #carry out models for different parametric methods survival
        tryCatch(
          model <- flexsurv::flexsurvreg(Surv(time_years, status) ~ sex, data=data, dist=extrapolations[i]),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            # extrapolations
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All") %>% 
              dplyr::rename(Sex = sex)
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Age = "All" ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            #grab the parameters from the model
            parameters_results_temp[[i]] <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], Age = "All" ) %>% 
              dplyr::slice(rep(1:n(), each = 2)) %>% 
              dplyr::mutate(Sex = levels(as.factor(data$sex)))
            
            #extract the hazard function over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            # median and mean predicted survival
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean10yr = est) %>%
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
              dplyr::rename(rmean5yr = est) %>%
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>%
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "sex" ) %>% 
              dplyr::inner_join(pr_mean10, by = "sex" ) %>% 
              dplyr::inner_join(pr_median, by = "sex" ) %>% 
              dplyr::inner_join(pr_survival_prob, by = "sex")
            
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Age = "All" ) %>% 
              dplyr::rename(Sex = sex)
            
            rm(model, pr_mean, pr_median, pr_mean5, pr_mean10, pr_survival_prob)
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
          
        }
        
      }
      
    }
    
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_results_temp)   
    parcombined <- dplyr::bind_rows(parameters_results_temp)
    medcombined <- dplyr::bind_rows(pred_median_mean_results_temp)
    
    extrapolations_sex[[j]] <- extrapolatedcombined
    gof_haz_sex[[j]] <- gofcombined
    hazot_sex[[j]] <- hotcombined
    parameters_sex[[j]] <-  parcombined
    pred_median_mean_sex[[j]] <- medcombined
    
    #print out progress               
    print(paste0(cancer_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))
    
  } else {
    
    print(paste0("sex stratification extrapolation analysis not carried out for ", cancer_cohorts$cohort_name[j], " due to only 1 sex present " , Sys.time()))
    
  }
  
  
}

if(db.name != "ECI"){ 
# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalsext <- dplyr::bind_rows(extrapolations_sex) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Sex", Truncated = "Yes")
goffinalsext <- dplyr::bind_rows(gof_haz_sex) %>%
  dplyr::mutate(Stratification = "None" , Adjustment = "Sex", Truncated = "Yes")
hazardotfinalsext <- dplyr::bind_rows(hazot_sex) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Sex", Truncated = "Yes")
parametersfinalsext <- dplyr::bind_rows(parameters_sex)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Sex", Truncated = "Yes") %>% 
  dplyr::relocate(shape, .after = Sex) %>% 
  dplyr::relocate(rate, .after = Sex) 
predmedmeanfinalsext <- dplyr::bind_rows(pred_median_mean_sex)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Sex", Truncated = "Yes")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for sex adjustment COMPLETE')
}

########################################
# SEX STRATIFICATION EXTRAPOLATION
#######################################

tic("extrapolation analysis with sex stratification")
info(logger, 'extrapolation analysis for sex stratification START')

extrapolations_sexS <- list()
gof_haz_sexS <- list()
hazot_sexS <- list()
parameters_sexS <- list()
pred_median_mean_sexS <- list()

for(j in 1:nrow(cancer_cohorts)) { 
  
  #temp results
  extrap_results_temp <- list() 
  gof_results_temp <- list() 
  hazot_results_temp <- list()
  parameters_results_temp <- list() 
  pred_median_mean_results_temp <- list() 
  
  #for each sex 
  extrap_sex <- list()
  gof_sex <- list()
  hot_sex <- list()   
  par_sex <- list()
  med_sex <- list()
  
  #subset the data by cancer type
  data <- Pop_truncated %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  sexlevels <- data %>%
    dplyr::group_by(sex) %>% dplyr::summarise(count = n()) %>% dplyr::tally()
  
  sexvalues <- data %>%
    dplyr::group_by(sex) %>% dplyr::summarise(count = n())
  
  # analysis wont run if only 1 sex present
  if(sexlevels == 2){
    
    for (sexl in 1:nrow(sexvalues)) {
      
      data_sex <- data %>% 
        dplyr::filter(sex == sexvalues$sex[sexl])
      
      #split per sex then run extrapolations
      print(paste0("extrapolations for stratification"," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j]," ", sexvalues$sex[sexl] ," started"))
      
      #carry out extrapolation for each cancer
      for(i in 1:length(extrapolations)) {   # Head of for-loop
        
        if(extrapolations[i] == "spline1") {
          
          # 1knotspline
          tryCatch(
            model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ 1 ,data=data_sex,k = 1, scale = "hazard"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
              #extrapolation
              extrap_results_temp[[i]] <- model %>%
                summary(t=t/365, tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i],
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #get the goodness of fit for each model
              gof_results_temp[[i]] <- model %>%
                glance() %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #grab the parameters and knots from the model
              coefs.p <- model[["coefficients"]] %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All" ) 
              
              knots.p <- model[["knots"]] %>%
                setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value)
              
              parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
                dplyr::mutate(Sex = data_sex$sex[sexl])
              
              # hazard over time
              hazot_results_temp[[i]] <- model %>%
                summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              # median and mean predicted survival
              pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
                dplyr::rename(median = est) %>% 
                dplyr::mutate(median = round(median, 4),
                              lcl = round(lcl, 4),
                              ucl = round(ucl, 4)) %>% 
                dplyr::rename(lower_median = lcl, upper_median = ucl)
              
              pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean = est) %>% 
                dplyr::mutate(rmean = round(rmean, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean5yr = est) %>% 
                dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se5yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean10yr = est) %>% 
                dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se10yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
                dplyr::mutate(est = round((est*100),4),
                              lcl = round((lcl*100),4),
                              ucl = round((ucl*100),4)) %>% 
                dplyr::rename("surv" = est,
                              "lower" = lcl,
                              "upper" = ucl) %>% 
                tidyr::pivot_wider(names_from = time, 
                                   values_from = c(surv, lower, upper),
                                   names_prefix = " year ",
                                   names_sep = "")
              
              pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
              pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All",
                              Sex = data_sex$sex[sexl]) 
              
              rm(model, pr_mean, pr_median,pr_mean5, pr_mean10, pr_survival_prob)
              
              #print out progress               
              print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
              
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }
          
        } else if(extrapolations[i] == "spline1o") {
          # 1knotspline odds
          
          tryCatch(
            model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ 1 , data=data_sex , k = 1, scale = "odds"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
              extrap_results_temp[[i]] <- model %>%
                summary(t=t/365, tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i],
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #get the goodness of fit for each model
              gof_results_temp[[i]] <- model %>%
                glance() %>%
                dplyr::mutate(Method = extrapolations_formatted[i],
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All" ,
                              Sex = data_sex$sex[sexl])
              
              #extract parameters
              #grab the parameters and knots from the model
              coefs.p <- model[["coefficients"]] %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value) %>%
                dplyr::mutate(Method = extrapolations_formatted[i],
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All") 
              
              knots.p <- model[["knots"]] %>%
                setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value)
              
              parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
                dplyr::mutate(Sex = data_sex$sex[sexl])
              
              # hazard over time
              hazot_results_temp[[i]] <- model %>%
                summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              # median and mean predicted survival
              pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
                dplyr::rename(median = est) %>% 
                dplyr::mutate(median = round(median, 4),
                              lcl = round(lcl, 4),
                              ucl = round(ucl, 4)) %>% 
                dplyr::rename(lower_median = lcl, upper_median = ucl)
              
              pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean = est) %>% 
                dplyr::mutate(rmean = round(rmean, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean5yr = est) %>% 
                dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se5yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean10yr = est) %>% 
                dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se10yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
                dplyr::mutate(est = round((est*100),4),
                              lcl = round((lcl*100),4),
                              ucl = round((ucl*100),4)) %>% 
                dplyr::rename("surv" = est,
                              "lower" = lcl,
                              "upper" = ucl) %>%  
                tidyr::pivot_wider(names_from = time, 
                                   values_from = c(surv, lower, upper),
                                   names_prefix = " year ",
                                   names_sep = "")
              
              pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
              pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All",
                              Sex = data_sex$sex[sexl]) 
              
              rm(model, pr_mean, pr_median,pr_mean5, pr_mean10, pr_survival_prob)
              
              #print out progress               
              print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
              
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }
          
        } else if(extrapolations[i] == "spline1n") {
          # 1knotspline normal
          
          tryCatch(
            model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ 1 , data=data_sex , k = 1, scale = "normal"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
              extrap_results_temp[[i]] <- model %>%
                summary(t=t/365, tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i],
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #get the goodness of fit for each model
              gof_results_temp[[i]] <- model %>%
                glance() %>%
                dplyr::mutate(Method = extrapolations_formatted[i],
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All" ,
                              Sex = data_sex$sex[sexl])
              
              #extract parameters
              #grab the parameters and knots from the model
              coefs.p <- model[["coefficients"]] %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value) %>%
                dplyr::mutate(Method = extrapolations_formatted[i],
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All") 
              
              knots.p <- model[["knots"]] %>%
                setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value)
              
              parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
                dplyr::mutate(Sex = data_sex$sex[sexl])
              
              # hazard over time
              hazot_results_temp[[i]] <- model %>%
                summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              # median and mean predicted survival
              pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
                dplyr::rename(median = est) %>% 
                dplyr::mutate(median = round(median, 4),
                              lcl = round(lcl, 4),
                              ucl = round(ucl, 4)) %>% 
                dplyr::rename(lower_median = lcl, upper_median = ucl)
              
              pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean = est) %>% 
                dplyr::mutate(rmean = round(rmean, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean5yr = est) %>% 
                dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se5yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean10yr = est) %>% 
                dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se10yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
                dplyr::mutate(est = round((est*100),4),
                              lcl = round((lcl*100),4),
                              ucl = round((ucl*100),4)) %>% 
                dplyr::rename("surv" = est,
                              "lower" = lcl,
                              "upper" = ucl) %>%  
                tidyr::pivot_wider(names_from = time, 
                                   values_from = c(surv, lower, upper),
                                   names_prefix = " year ",
                                   names_sep = "")
              
              pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
              pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All",
                              Sex = data_sex$sex[sexl]) 
              
              rm(model, pr_mean, pr_median,pr_mean5, pr_mean10, pr_survival_prob)
              
              #print out progress               
              print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
              
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }  
          
          
        } else if(extrapolations[i] == "spline3") {
          # 3knotspline
          
          tryCatch(
            model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ 1, data=data_sex, k = 3, scale = "hazard"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
              extrap_results_temp[[i]] <- model %>%
                summary(t=t/365, tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #get the goodness of fit for each model
              gof_results_temp[[i]] <- model %>%
                glance() %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #extract parameters
              #grab the parameters and knots from the model
              coefs.p <- model[["coefficients"]] %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All") 
              
              knots.p <- model[["knots"]] %>%
                setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value)
              
              parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
                dplyr::mutate(Sex = data_sex$sex[sexl])
              
              # hazard over time
              hazot_results_temp[[i]] <- model %>%
                summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All" ,
                              Sex = data_sex$sex[sexl])
              
              # median and mean predicted survival
              pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
                dplyr::rename(median = est) %>% 
                dplyr::mutate(median = round(median, 4),
                              lcl = round(lcl, 4),
                              ucl = round(ucl, 4)) %>% 
                dplyr::rename(lower_median = lcl, upper_median = ucl)
              
              pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean = est) %>% 
                dplyr::mutate(rmean = round(rmean, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean5yr = est) %>% 
                dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se5yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean10yr = est) %>% 
                dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se10yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
                dplyr::mutate(est = round((est*100),4),
                              lcl = round((lcl*100),4),
                              ucl = round((ucl*100),4)) %>% 
                dplyr::rename("surv" = est,
                              "lower" = lcl,
                              "upper" = ucl) %>%  
                tidyr::pivot_wider(names_from = time, 
                                   values_from = c(surv, lower, upper),
                                   names_prefix = " year ",
                                   names_sep = "")
              
              pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
              pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All",
                              Sex = data_sex$sex[sexl]) 
              
              rm(model, pr_mean, pr_median,pr_mean5, pr_mean10, pr_survival_prob)
              
              #print out progress               
              print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
              
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }
          
          
        } else if(extrapolations[i] == "spline3o") {
          # 3knotspline odds
          
          tryCatch(
            model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ 1, data=data_sex, k = 3, scale = "odds"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
              extrap_results_temp[[i]] <- model %>%
                summary(t=t/365, tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #get the goodness of fit for each model
              gof_results_temp[[i]] <- model %>%
                glance() %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #extract parameters
              #grab the parameters and knots from the model
              coefs.p <- model[["coefficients"]] %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All") 
              
              knots.p <- model[["knots"]] %>%
                setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value)
              
              parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
                dplyr::mutate(Sex = data_sex$sex[sexl])
              
              # hazard over time
              hazot_results_temp[[i]] <- model %>%
                summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All" ,
                              Sex = data_sex$sex[sexl])
              
              # median and mean predicted survival
              pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
                dplyr::rename(median = est) %>% 
                dplyr::mutate(median = round(median, 4),
                              lcl = round(lcl, 4),
                              ucl = round(ucl, 4)) %>% 
                dplyr::rename(lower_median = lcl, upper_median = ucl)
              
              pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean = est) %>% 
                dplyr::mutate(rmean = round(rmean, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean5yr = est) %>% 
                dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se5yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean10yr = est) %>% 
                dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se10yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
                dplyr::mutate(est = round((est*100),4),
                              lcl = round((lcl*100),4),
                              ucl = round((ucl*100),4)) %>% 
                dplyr::rename("surv" = est,
                              "lower" = lcl,
                              "upper" = ucl) %>% 
                tidyr::pivot_wider(names_from = time, 
                                   values_from = c(surv, lower, upper),
                                   names_prefix = " year ",
                                   names_sep = "")
              
              pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
              pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All",
                              Sex = data_sex$sex[sexl]) 
              
              rm(model, pr_mean, pr_median,pr_mean5, pr_mean10, pr_survival_prob)
              
              #print out progress               
              print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
              
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }
          
          
        } else if(extrapolations[i] == "spline3n") {
          # 3knotspline normal
          
          tryCatch(
            model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ 1, data=data_sex, k = 3, scale = "normal"),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
              extrap_results_temp[[i]] <- model %>%
                summary(t=t/365, tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #get the goodness of fit for each model
              gof_results_temp[[i]] <- model %>%
                glance() %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #extract parameters
              #grab the parameters and knots from the model
              coefs.p <- model[["coefficients"]] %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All") 
              
              knots.p <- model[["knots"]] %>%
                setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value)
              
              parameters_results_temp[[i]] <- dplyr::bind_cols(coefs.p,  knots.p ) %>% 
                dplyr::mutate(Sex = data_sex$sex[sexl])
              
              # hazard over time
              hazot_results_temp[[i]] <- model %>%
                summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All" ,
                              Sex = data_sex$sex[sexl])
              
              # median and mean predicted survival
              pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
                dplyr::rename(median = est) %>% 
                dplyr::mutate(median = round(median, 4),
                              lcl = round(lcl, 4),
                              ucl = round(ucl, 4)) %>% 
                dplyr::rename(lower_median = lcl, upper_median = ucl)
              
              pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean = est) %>% 
                dplyr::mutate(rmean = round(rmean, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean5yr = est) %>% 
                dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se5yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean10yr = est) %>% 
                dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se10yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
                dplyr::mutate(est = round((est*100),4),
                              lcl = round((lcl*100),4),
                              ucl = round((ucl*100),4)) %>% 
                dplyr::rename("surv" = est,
                              "lower" = lcl,
                              "upper" = ucl) %>%  
                tidyr::pivot_wider(names_from = time, 
                                   values_from = c(surv, lower, upper),
                                   names_prefix = " year ",
                                   names_sep = "")
              
              pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
              pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All",
                              Sex = data_sex$sex[sexl]) 
              
              rm(model, pr_mean, pr_median,pr_mean5, pr_mean10, pr_survival_prob)
              
              #print out progress               
              print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
              
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }
          
    
        } else {
          
          #carry out models for different parametric methods survival
          tryCatch(
            model <- flexsurv::flexsurvreg(Surv(time_years, status) ~ 1, data=data_sex, dist=extrapolations[i]),
            error = function(e){
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
            warning = function(w){
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          )
          
          if (exists("model") == TRUE) {
            
            tryCatch({
              
              # extrapolations
              extrap_results_temp[[i]] <- model %>%
                summary(t=t/365, tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i],
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #get the goodness of fit for each model
              gof_results_temp[[i]] <- model %>%
                glance() %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #grab the parameters from the model
              parameters_results_temp[[i]] <- model[["coefficients"]] %>%
                tibble::enframe() %>%
                tidyr::pivot_wider(names_from = name, values_from = value) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j],
                              Age = "All",
                              Sex = data_sex$sex[sexl])
              
              #extract the hazard function over time
              hazot_results_temp[[i]] <- model %>%
                summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All" ,
                              Sex = data_sex$sex[sexl])
              
              # median and mean predicted survival
              pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
                dplyr::rename(median = est) %>% 
                dplyr::mutate(median = round(median, 4),
                              lcl = round(lcl, 4),
                              ucl = round(ucl, 4)) %>% 
                dplyr::rename(lower_median = lcl, upper_median = ucl)
              
              pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean = est) %>% 
                dplyr::mutate(rmean = round(rmean, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean5yr = est) %>% 
                dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se5yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
                dplyr::rename(rmean10yr = est) %>% 
                dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                              se = round(se, 4),
                              time = round(time,4)) %>% 
                dplyr::rename(se10yr = se) %>% 
                dplyr::select(-c(lcl, ucl, time))
              
              pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
                dplyr::mutate(est = round((est*100),4),
                              lcl = round((lcl*100),4),
                              ucl = round((ucl*100),4)) %>% 
                dplyr::rename("surv" = est,
                              "lower" = lcl,
                              "upper" = ucl) %>% 
                tidyr::pivot_wider(names_from = time, 
                                   values_from = c(surv, lower, upper),
                                   names_prefix = " year ",
                                   names_sep = "")
              
              pred_median_mean_results_temp[[i]] <- dplyr::bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
              pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
                dplyr::mutate(Method = extrapolations_formatted[i], 
                              Cancer = cancer_cohorts$cohort_name[j], 
                              Age = "All",
                              Sex = data_sex$sex[sexl]) 
              
              rm(model, pr_mean, pr_median,pr_mean5, pr_mean10, pr_survival_prob)
              
              #print out progress               
              print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
              
            }, 
            
            error = function(e) {
              cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
            
            warning = function(w) {
              cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for sex model", "\n")
              info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
            
            )
            
          }
          
          
        }
        
        
      }
      
      #put results for gender here
      extrap_sex[[sexl]] <- dplyr::bind_rows(extrap_results_temp)
      gof_sex[[sexl]] <- dplyr::bind_rows(gof_results_temp)
      hot_sex[[sexl]] <- dplyr::bind_rows(hazot_results_temp)   
      par_sex[[sexl]] <- dplyr::bind_rows(parameters_results_temp)
      med_sex[[sexl]] <- dplyr::bind_rows(pred_median_mean_results_temp)
      
      # clear the lists again ready for next iteration
      extrap_results_temp <- list() 
      gof_results_temp <- list() 
      hazot_results_temp <- list() 
      parameters_results_temp <- list() 
      pred_median_mean_results_temp <- list()
      
    }
    
    extrapolatedcombined <- dplyr::bind_rows(extrap_sex)
    gofcombined <- dplyr::bind_rows(gof_sex)
    hotcombined <- dplyr::bind_rows(hot_sex)   
    parcombined <- dplyr::bind_rows(par_sex)
    medcombined <- dplyr::bind_rows(med_sex)
    
    extrapolations_sexS[[j]] <- extrapolatedcombined
    gof_haz_sexS[[j]] <- gofcombined
    hazot_sexS[[j]] <- hotcombined
    parameters_sexS[[j]] <-  parcombined
    pred_median_mean_sexS[[j]] <- medcombined
    
    #print out progress               
    print(paste0(cancer_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))
    
  } else {
    
    print(paste0("sex stratification extrapolation analysis not carried out for ", cancer_cohorts$cohort_name[j], " due to only 1 sex present " , Sys.time()))
    
  }
  
  
}

if(db.name != "ECI"){ 
# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalsexSt <- dplyr::bind_rows(extrapolations_sexS) %>%
  dplyr::mutate(Stratification = "Sex", Adjustment = "None", Truncated = "Yes")
goffinalsexSt <- dplyr::bind_rows(gof_haz_sexS) %>%
  dplyr::mutate(Stratification = "Sex" , Adjustment = "None", Truncated = "Yes")
hazardotfinalsexSt <- dplyr::bind_rows(hazot_sexS) %>%
  dplyr::mutate(Stratification = "Sex", Adjustment = "None", Truncated = "Yes")
parametersfinalsexSt <- dplyr::bind_rows(parameters_sexS)  %>%
  dplyr::mutate(Stratification = "Sex", Adjustment = "None", Truncated = "Yes") %>% 
  dplyr::relocate(shape, .after = Sex) %>% 
  dplyr::relocate(rate, .after = Sex) %>% 
  dplyr::mutate(rate = coalesce(rate, `1`)) %>% 
  dplyr::select(!c(`1`))
predmedmeanfinalsexSt <- dplyr::bind_rows(pred_median_mean_sexS)  %>%
  dplyr::mutate(Stratification = "Sex", Adjustment = "None", Truncated = "Yes")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for sex stratification COMPLETE')
}

####################################################################################################
#################################################
# AGE ANALYSIS
#################################################
tic("Extrapolation analysis for age adjustment")

info(logger, 'Extrapolation analysis for age adjustment START')

# Initiate lists to store output within loop ---- 
extrapolations_age <- list() # extrapolation over time
gof_haz_age <- list() # goodness of fit
hazot_age <- list() # hazard over time
parameters_age <- list() # parameters from each model
pred_median_mean_age <- list() # extract the predicted median and RMST and surv prob at 1 5 and 10 years from extrapolation methods

for(j in 1:nrow(cancer_cohorts)) {
  
  # set up temp tables
  extrap_results_temp <- list() 
  gof_results_temp <- list()
  hazot_results_temp <- list() 
  parameters_results_temp <- list()
  pred_median_mean_results_temp <- list()
  
  #subset the data by cancer type
  data <- Pop_truncated %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  #create a reference level in this case we are using 70-79 (for most cancers highest numbers of people diagnosed)
  data$age_gr <- as.factor(data$age_gr)
  data$age_gr <-  relevel(data$age_gr, "70 to 79")
  
  agelevels <- data %>%
    dplyr::group_by(age_gr) %>% dplyr::summarise(count = n())
  
  #carry out extrapolation for each cancer
  for(i in 1:length(extrapolations)) {   # Head of for-loop
    
    if(extrapolations[i] == "spline1") {
      
      # 1knotspline
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr ,data=data,k = 1, scale = "hazard"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          #extrapolation
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i],
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], Cancer = cancer_cohorts$cohort_name[j], Sex = "Both" ) 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      } 
      
      
    } else if(extrapolations[i] == "spline1o") {
      # 1knotspline odds
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr , data=data , k = 1, scale = "odds"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j],
                          Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
                          dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      } 
      
      
    } else if(extrapolations[i] == "spline1n") {
      # 1knotspline normal
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr , data=data , k = 1, scale = "normal"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j],
                          Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>%  
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      } 
      
      
    } else if(extrapolations[i] == "spline3") {
      # 3knotspline
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr, data=data, k = 3, scale = "hazard"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j],
                          Sex = "Both")%>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      } 
      
      
    } else if(extrapolations[i] == "spline3o") {
      # 3knotspline odds
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr, data=data, k = 3, scale = "odds"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j],
                          Sex = "Both")%>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      } 
      
    } else if(extrapolations[i] == "spline3n") {
      # 3knotspline normal
      
      tryCatch(
        model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ age_gr, data=data, k = 3, scale = "normal"),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j],
                          Sex = "Both")%>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #extract parameters
          #grab the parameters and knots from the model
          coefs.p <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") 
          
          knots.p <- model[["knots"]] %>%
            setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value)
          
          parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          # hazard over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>%
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      } 
      
      
    } else {
      
      #carry out models for different parametric methods survival
      tryCatch(
        model <- flexsurv::flexsurvreg(Surv(time_years, status) ~ age_gr, data=data, dist=extrapolations[i]),
        error = function(e){
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
        warning = function(w){
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
      )
      
      if (exists("model") == TRUE) {
        
        tryCatch({
          
          # extrapolations
          extrap_results_temp[[i]] <- model %>%
            summary(t=t/365, tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j],
                          Sex = "Both") %>% 
            dplyr::rename(Age = age_gr)
          
          #get the goodness of fit for each model
          gof_results_temp[[i]] <- model %>%
            broom::glance() %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #grab the parameters from the model
          parameters_results_temp[[i]] <- model[["coefficients"]] %>%
            tibble::enframe() %>%
            tidyr::pivot_wider(names_from = name, values_from = value) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::slice(rep(1:n(), each = nrow(agelevels))) %>% 
            dplyr::mutate(Age = levels(as.factor(data$age_gr)))
          
          #extract the hazard function over time
          hazot_results_temp[[i]] <- model %>%
            summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          # median and mean predicted survival
          pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
            dplyr::rename(median = est) %>% 
            dplyr::mutate(median = round(median, 4),
                          lcl = round(lcl, 4),
                          ucl = round(ucl, 4)) %>% 
            dplyr::rename(lower_median = lcl, upper_median = ucl)
          
          pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
            dplyr::rename(rmean = est) %>% 
            dplyr::mutate(rmean = round(rmean, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean5yr = est) %>%
            dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se5yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>%
            dplyr::rename(rmean10yr = est) %>%
            dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                          se = round(se, 4),
                          time = round(time,4)) %>%
            dplyr::rename(se10yr = se) %>% 
            dplyr::select(-c(lcl, ucl, time))
          
          pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
            dplyr::mutate(est = round((est*100),4),
                          lcl = round((lcl*100),4),
                          ucl = round((ucl*100),4)) %>% 
            dplyr::rename("surv" = est,
                          "lower" = lcl,
                          "upper" = ucl) %>% 
            tidyr::pivot_wider(names_from = time, 
                               values_from = c(surv, lower, upper),
                               names_prefix = " year ",
                               names_sep = "")
          
          pred_median_mean_results_temp[[i]] <- dplyr::inner_join(pr_mean, pr_mean5, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_mean10, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_median, by = "age_gr" ) %>% 
            dplyr::inner_join(pr_survival_prob, by = "age_gr")
          
          pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
            dplyr::mutate(Method = extrapolations_formatted[i], 
                          Cancer = cancer_cohorts$cohort_name[j], 
                          Sex = "Both" ) %>% 
            dplyr::rename(Age = age_gr)
          
          rm(model, pr_mean, pr_median, pr_survival_prob, pr_mean10, pr_mean5)
          
          #print out progress               
          print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,cancer_cohorts$cohort_name[j], " completed"))
          
          
        }, 
        
        error = function(e) {
          cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
        
        warning = function(w) {
          cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
          info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        
        )
        
      }
      
      
    }
    
    
  }
  
  extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
  gofcombined <- dplyr::bind_rows(gof_results_temp)
  hotcombined <- dplyr::bind_rows(hazot_results_temp)   
  parcombined <- dplyr::bind_rows(parameters_results_temp)
  medcombined <- dplyr::bind_rows(pred_median_mean_results_temp)
  
  extrapolations_age[[j]] <- extrapolatedcombined
  gof_haz_age[[j]] <- gofcombined
  hazot_age[[j]] <- hotcombined
  parameters_age[[j]] <-  parcombined
  pred_median_mean_age[[j]] <- medcombined
  
  #print out progress               
  print(paste0(cancer_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))
  
} 

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalaget <- dplyr::bind_rows(extrapolations_age) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age", Truncated = "Yes")
goffinalaget <- dplyr::bind_rows(gof_haz_age) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age", Truncated = "Yes")
hazardotfinalaget <- dplyr::bind_rows(hazot_age) %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age", Truncated = "Yes")
parametersfinalaget <- dplyr::bind_rows(parameters_age)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age", Truncated = "Yes") %>% 
  dplyr::relocate(shape, .after = Age) %>% 
  dplyr::relocate(rate, .after = Age) 
predmedmeanfinalaget <- dplyr::bind_rows(pred_median_mean_age)  %>%
  dplyr::mutate(Stratification = "None", Adjustment = "Age", Truncated = "Yes")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for age adjustment COMPLETE')


########################################
# AGE STRATIFICATION EXTRAPOLATION
#######################################

tic("extrapolation analysis with age stratification")
info(logger, 'extrapolation analysis for age stratification START')

extrapolations_ageS <- list()
gof_haz_ageS <- list()
hazot_ageS <- list()
parameters_ageS <- list()
pred_median_mean_ageS <- list()

for(j in 1:nrow(cancer_cohorts)) { 
  
  #temp results
  extrap_results_temp <- list() 
  gof_results_temp <- list() 
  hazot_results_temp <- list()
  parameters_results_temp <- list() 
  pred_median_mean_results_temp <- list()
  
  #for each age 
  extrap_age <- list()
  gof_age <- list()
  hot_age <- list()   
  par_age <- list()
  med_age <- list()   
  
  #subset the data by cancer type
  data <- Pop_truncated %>%
    dplyr::filter(cohort_definition_id == cancer_cohorts$cohort_definition_id[j])
  
  agelevels <- data %>%
    dplyr::group_by(age_gr) %>% dplyr::summarise(count = n()) %>% tally()
  
  agevalues <- data %>%
    dplyr::group_by(age_gr) %>% dplyr::summarise(count = n())
  
  for (agel in 1:nrow(agevalues)) {
    
    data_age <- data %>% 
      dplyr::filter(age_gr == agevalues$age_gr[agel])
    
    #split per gender then run extrapolations
    print(paste0("extrapolations for stratification"," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] ," ", agevalues$age_gr[agel] ," started"))
    
    #carry out extrapolation for each cancer
    for(i in 1:length(extrapolations)) {   # Head of for-loop
      
      if(extrapolations[i] == "spline1") {
        
        # 1knotspline
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ 1 ,data=data_age ,k = 1, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            #extrapolation
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ) 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      } else if(extrapolations[i] == "spline1o") {
        # 1knotspline odds
        
        tryCatch(
          model <- flexsurv::flexsurvspline(formula=Surv(time_years,status-1) ~ 1, data=data_age, k = 1, scale = "odds"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1"  ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4) ) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      } else if(extrapolations[i] == "spline1n") {
        # 1knotspline normal
        
        tryCatch(
          model <- flexsurv::flexsurvspline	(formula=Surv(time_years,status-1) ~ 1, data=data_age, k = 1, scale = "normal"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1"  ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
      } else if(extrapolations[i] == "spline3") {
        # 3knotspline
        
        tryCatch(
          model <- flexsurv::flexsurvspline	(formula=Surv(time_years,status-1) ~ 1, data=data_age, k = 3, scale = "hazard"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4) ) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      } else if(extrapolations[i] == "spline3o") {
        # 3knotspline odds
        
        tryCatch(
          model <- flexsurv::flexsurvspline	(formula=Surv(time_years,status-1) ~ 1, data=data_age, k = 3, scale = "odds"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      } else if(extrapolations[i] == "spline3n") {
        # 3knotspline normal
        
        tryCatch(
          model <- flexsurv::flexsurvspline	(formula=Surv(time_years,status-1) ~ 1, data=data_age, k = 3, scale = "normal"),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i],
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            #extract parameters
            #grab the parameters and knots from the model
            coefs.p <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j],
                            Sex = "Both") 
            
            knots.p <- model[["knots"]] %>%
              setNames(., c("SplineLowerB", "SplineInternal1" , "SplineInternal2", "SplineInternal3" ,"SplineUpperB")) %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value)
            
            parameters_results_temp[[i]] <- bind_cols(coefs.p,  knots.p ) %>% 
              dplyr::mutate(Age = names(table(data_age$age_gr)))
            
            # hazard over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
        }
        
        
      } else {
        
        #carry out models for different parametric methods survival
        tryCatch(
          model <- flexsurv::flexsurvreg(Surv(time_years, status) ~ 1, data=data_age, dist=extrapolations[i]),
          error = function(e){
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " error not carried out \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e)) } ,
          warning = function(w){
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " warning problem with model \n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
        )
        
        if (exists("model") == TRUE) {
          
          tryCatch({
            
            # extrapolations
            extrap_results_temp[[i]] <- model %>%
              summary(t=t/365, tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both",
                            Age = names(table(data_age$age_gr)))
            
            #get the goodness of fit for each model
            gof_results_temp[[i]] <- model %>%
              broom::glance() %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            #grab the parameters from the model
            parameters_results_temp[[i]] <- model[["coefficients"]] %>%
              tibble::enframe() %>%
              tidyr::pivot_wider(names_from = name, values_from = value) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            #extract the hazard function over time
            hazot_results_temp[[i]] <- model %>%
              summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both" ,
                            Age = names(table(data_age$age_gr)))
            
            # median and mean survival predictions from extrapolation
            pr_median <- summary(model, type = "median", ci = TRUE, tidy = T) %>% 
              dplyr::rename(median = est) %>% 
              dplyr::mutate(median = round(median, 4),
                            lcl = round(lcl, 4),
                            ucl = round(ucl, 4)) %>% 
              dplyr::rename(lower_median = lcl, upper_median = ucl)
            
            pr_mean <- summary(model, type = "rmst", se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean = est) %>% 
              dplyr::mutate(rmean = round(rmean, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean5 <- summary(model, type = "rmst", t = 5, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean5yr = est) %>% 
              dplyr::mutate(rmean5yr = round(rmean5yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se5yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            pr_mean10 <- summary(model, type = "rmst", t = 10, se = TRUE, tidy = T) %>% 
              dplyr::rename(rmean10yr = est) %>% 
              dplyr::mutate(rmean10yr = round(rmean10yr, 4),
                            se = round(se, 4),
                            time = round(time,4)) %>% 
              dplyr::rename(se10yr = se) %>% 
              dplyr::select(-c(lcl, ucl, time))
            
            # survival predicted probabilities from extrapolations
            pr_survival_prob <- summary(model, type = "survival", t = c(1,5,10), ci = TRUE, tidy = T) %>% 
              dplyr::mutate(est = round((est*100),4),
                            lcl = round((lcl*100),4),
                            ucl = round((ucl*100),4)) %>% 
              dplyr::rename("surv" = est,
                            "lower" = lcl,
                            "upper" = ucl) %>% 
              tidyr::pivot_wider(names_from = time, 
                                 values_from = c(surv, lower, upper),
                                 names_prefix = " year ",
                                 names_sep = "")
            
            pred_median_mean_results_temp[[i]] <- bind_cols(pr_mean,pr_mean5, pr_mean10, pr_median, pr_survival_prob )
            pred_median_mean_results_temp[[i]] <- pred_median_mean_results_temp[[i]] %>% 
              dplyr::mutate(Method = extrapolations_formatted[i], 
                            Cancer = cancer_cohorts$cohort_name[j], 
                            Sex = "Both", 
                            Age = names(table(data_age$age_gr)))
            
            rm(model,pr_survival_prob, pr_mean, pr_median, pr_mean10, pr_mean5 )
            
            #print out progress               
            print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " , cancer_cohorts$cohort_name[j] , " completed"))
            
            
          }, 
          
          error = function(e) {
            cat(conditionMessage(e), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " model not carried out for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," model not carried out ", e))} ,
          
          warning = function(w) {
            cat(conditionMessage(w), "for", cancer_cohorts$cohort_name[j] , ":", extrapolations[i], " potential problem with model for age model", "\n")
            info(logger, paste0(cancer_cohorts$cohort_name[j], " : ", extrapolations[i]," potential problem with model ", w))}
          
          )
          
          
        }
        
      }
      
    }
    
    #put results for age here
    extrap_age[[agel]] <- dplyr::bind_rows(extrap_results_temp)
    gof_age[[agel]] <- dplyr::bind_rows(gof_results_temp)
    hot_age[[agel]] <- dplyr::bind_rows(hazot_results_temp)   
    par_age[[agel]] <- dplyr::bind_rows(parameters_results_temp)
    med_age[[agel]] <- dplyr::bind_rows(pred_median_mean_results_temp)
    
    # clear the lists again ready for next iteration
    extrap_results_temp <- list() 
    gof_results_temp <- list() 
    hazot_results_temp <- list() 
    parameters_results_temp <- list() 
    pred_median_mean_results_temp <- list()
    
    
    
  }
  
  extrapolatedcombined <- dplyr::bind_rows(extrap_age)
  gofcombined <- dplyr::bind_rows(gof_age)
  hotcombined <- dplyr::bind_rows(hot_age)   
  parcombined <- dplyr::bind_rows(par_age)
  medcombined <- dplyr::bind_rows(med_age)
  
  extrapolations_ageS[[j]] <- extrapolatedcombined
  gof_haz_ageS[[j]] <- gofcombined
  hazot_ageS[[j]] <- hotcombined
  parameters_ageS[[j]] <-  parcombined
  pred_median_mean_ageS[[j]] <- medcombined
  
  #print out progress               
  print(paste0(cancer_cohorts$cohort_name[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalageSt <- dplyr::bind_rows(extrapolations_ageS) %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None", Truncated = "Yes")
goffinalageSt <- dplyr::bind_rows(gof_haz_ageS) %>%
  dplyr::mutate(Stratification = "Age" , Adjustment = "None", Truncated = "Yes")
hazardotfinalageSt <- dplyr::bind_rows(hazot_ageS) %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None", Truncated = "Yes")
parametersfinalageSt <- dplyr::bind_rows(parameters_ageS)  %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None", Truncated = "Yes") %>%
  dplyr::relocate(shape, .after = Sex) %>%
  dplyr::relocate(rate, .after = Sex) %>%
  dplyr::mutate(rate = coalesce(rate, `1`)) %>%
  dplyr::select(!c(`1`))
predmedmeanfinalageSt <- dplyr::bind_rows(pred_median_mean_ageS)  %>%
  dplyr::mutate(Stratification = "Age", Adjustment = "None", Truncated = "Yes")

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for age stratification COMPLETE')

