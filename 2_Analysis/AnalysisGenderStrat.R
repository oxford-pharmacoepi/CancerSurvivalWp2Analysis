########################################
# GENDER STRATIFICATION
#######################################

# km survival, risk table, median survival, hazard over time from the observed data for each cancer where both genders are present ----
tic("KM analysis with gender stratification")
info(logger, 'KM analysis for gender stratification START')

# KM observed
observedkm_gender <- list()
observedmedianKM_gender <- list()
observedhazotKM_gender <- list()
observedrisktableKM_gender <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  #creates a test that determines if both genders in the data
  genderlevels <- data %>%
    group_by(gender) %>% summarise(count = n()) %>% tally()
  
  # analysis wont run if only 1 gender present
  if(genderlevels == 2){
    
    #carry out km estimate
    observedkm_gender[[j]] <- survfit (Surv(time_years, status) ~ gender, data=data) %>%
      tidy() %>%
      rename(Gender = strata) %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = str_replace(Gender, "gender=Male", "Male"), Gender = str_replace(Gender,"gender=Female", "Female")) %>%
      filter(n.risk >= 5) #remove entries with less than 5 patients
    
    print(paste0("KM for observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    # get the risk table ---
    grid <- seq(0,floor(max(data$time_years)),by=2)
    observedrisktableKM_gender[[j]] <- RiskSetCount(grid,data$time_years[data$gender == "Male"])%>%
      rbind(grid) %>% as.data.frame() %>%
      `colnames<-`(grid) %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All") %>%
      slice(1) %>%
      rbind(RiskSetCount(grid,data$time_years[data$gender == "Female"]))%>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = c("Male", "Female"))
    
    print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    
    # KM median survival ---
    modelKM <- survfit(Surv(time_years, status) ~ gender, data=data) %>%
      summary()
    
    # median survival ---
    observedmedianKM_gender[[j]] <- modelKM$table %>%
      as.data.frame() %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohortName[j], 
             Age = "All" ,
             Gender = c("Male", "Female"))
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    # hazard over time ---
    observedhazotKM_gender[[j]] <- group_by(data,gender) %>% 
      do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
      ungroup %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Age = "All")
    
    print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohortName[j], "gender strat completed"))
    
    
  }
  
} # this closes the loop on the analysis containing both genders

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined_gender <- dplyr::bind_rows(observedkm_gender) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined_gender <- dplyr::bind_rows(observedmedianKM_gender) 

hotkmcombined_gender <- dplyr::bind_rows(observedhazotKM_gender) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci, Gender = gender )

#generate the risk table and remove entries < 5 patients
risktableskm_gender <- dplyr::bind_rows(observedrisktableKM_gender) 
risktableskm_gender <- risktableskm_gender %>%
  mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.== 0, NA, .))) %>%  
  mutate_at(.vars = c(1:(ncol(risktableskm_gender)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
  replace(is.na(.), 0) %>%
  relocate(Cancer)


ResultsKM_GENDER <- list("KM_observed_gender" = observedkmcombined_gender, 
                         "KM_MedianSur_gender" = medkmcombined_gender,
                         "KM_hazard_rate_gender" = hotkmcombined_gender,
                         "KM_risktable_gender" = risktableskm_gender)

#write to excel
openxlsx::write.xlsx(ResultsKM_GENDER, file = here("Results", db.name ,"cancer_KM_observed_results_GENDER.xlsx"))

# observedkmcombined_gender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_GENDER.xlsx"), sheet = 1)
# medkmcombined_gender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_GENDER.xlsx"), sheet = 2)
# hotkmcombined_gender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_GENDER.xlsx"), sheet = 3)
# risktableskm_gender <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_KM_observed_results_GENDER.xlsx"), sheet = 4)


toc(func.toc=toc_min)
info(logger, 'KM analysis for gender stratification COMPLETE')

###########################################

# Extrapolation analysis for gender stratification ------

tic("Extrapolation analysis for gender stratification")
info(logger, 'Extrapolation analysis for gender stratification START')


