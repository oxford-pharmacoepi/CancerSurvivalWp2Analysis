########################################
# AGE STRATIFICATION
#######################################

# km survival, risk table, median survival, hazard over time from the observed data for each cancer where both genders are present ----
tic("KM analysis with gender stratification")
info(logger, 'KM analysis for gender stratification START')

# KM observed
observedkm_age <- list()
observedmedianKM_age <- list()
observedhazotKM_age <- list()
observedrisktableKM_age <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  # get the risk table ---
  grid <- seq(0,floor(max(data$time_years)),by=2)
  
  observedrisktableKM_age[[j]] <- RiskSetCount(grid,data$time_years[data$age_gr == "<30"]) %>%
    rbind(grid) %>% as.data.frame() %>%
    `colnames<-`(grid) %>%
    slice(1) %>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "30-39"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "40-49"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "50-59"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "60-69"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "70-79"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == "80-89"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$age_gr == ">=90"]))%>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Gender = "Both", Age = c("<30" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90")) %>%
    mutate_at(.vars = c(1:(ncol(observedrisktableKM_age[[j]])-4)), funs(ifelse(.== 0, NA, .))) %>%  
    mutate_at(.vars = c(1:(ncol(observedrisktableKM_age[[j]])-4)), funs(ifelse(.<= 5, "<5", .))) %>%
    replace(is.na(.), 0) %>%
    mutate(across(everything(), as.character))
  
  # filter that removes data where there are more than 75% missing data points after removing columns with less than 5 entries
  # remove entries < 5 patients turn to zero
  filterdatatest <- observedrisktableKM_age[[j]] %>%
    mutate_at(.vars = c(1:(ncol(observedrisktableKM_age[[j]])-4)), funs(ifelse(.=="<5", 0, .))) %>%
    replace(is.na(.), 0) 
  
  # calculate the number of columns 
  elgcols <- ncol(filterdatatest) - 4 
  #count the number of zeros across the rows
  filterdatatest <- filterdatatest %>% 
    mutate(count=rowSums(.[1:elgcols]==0), percentzero = ((count/elgcols)*100) ) %>%
    filter(percentzero != 75) %>%
    filter(percentzero < 75)
  
  #create filter function to put into results below
  target <- filterdatatest$Age
  
  #filter data removing data with > 75% missingness
  data <- data %>%
    filter((age_gr %in% target))
  
  #put a flag here for log that sub groups were removed due to missingness so can keep a record
  #TBC
  
  #carry out km estimate
  observedkm_age[[j]] <- survfit (Surv(time_years, status) ~ age_gr, data=data) %>%
    tidy() %>%
    rename(Age = strata) %>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], 
           Age = str_replace(Age, "age_gr=<30", "<30"),
           Age = str_replace(Age, "age_gr=30-39", "30-39"),
           Age = str_replace(Age, "age_gr=40-49", "40-49"),
           Age = str_replace(Age, "age_gr=50-59", "50-59"),
           Age = str_replace(Age, "age_gr=60-69", "60-69"),
           Age = str_replace(Age, "age_gr=70-79", "70-79"),
           Age = str_replace(Age, "age_gr=80-89", "80-89"),
           Age = str_replace(Age, "age_gr=>=90", ">=90"),
           Gender = "Both")
  
  print(paste0("KM for observed data age strat ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))

  
  # KM median survival---
  modelKM <- survfit(Surv(time_years, status) ~ age_gr, data=data) %>%
    summary()
  
  observedmedianKM_age[[j]] <- modelKM$table %>%
    as.data.frame() %>%
    mutate(Method = "Kaplan-Meier", 
           Cancer = outcome_cohorts$cohortName[j], 
           Age = target ,
           Gender = "Both" )
  
  print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
  
  # hazard function over time ----
  observedhazotKM_age[[j]] <- group_by(data,age_gr) %>% 
    do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
    ungroup %>%
    mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Gender = "Both")

  print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohortName[j], " age strat completed"))
  
  
}

# take the results from a list (one element for each cancer) and put into dataframe for KM survival
observedkmcombined_age <- dplyr::bind_rows(observedkm_age) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined_age <- dplyr::bind_rows(observedmedianKM_age) 

hotkmcombined_age <- dplyr::bind_rows(observedhazotKM_age) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci, Age = age_gr )

#generate the risk table and remove entries < 5 patients
risktableskm_age <- dplyr::bind_rows(observedrisktableKM_age)
# risktableskm_age <- risktableskm_age %>%
#   mutate_at(.vars = c(1:(ncol(risktableskm_age)-4)), funs(ifelse(.== 0, NA, .))) %>%  
#   mutate_at(.vars = c(1:(ncol(risktableskm_age)-4)), funs(ifelse(.<= 5, "<5", .))) %>%
#   replace(is.na(.), 0) %>%
#   relocate(Cancer)

ResultsKM_AGE <- list("KM_observed_age" = observedkmcombined_age, 
                      "KM_MedianSur_age" = medkmcombined_age,
                      "KM_hazard_rate_age" = hotkmcombined_age,
                      "KM_risktable_age" = risktableskm_age)

#write to excel
openxlsx::write.xlsx(ResultsKM_AGE, file = here("Results", db.name ,"cancer_KM_observed_results_AGE.xlsx"))

toc(func.toc=toc_min)

info(logger, 'KM analysis for AGE stratification COMPLETE')

###########################################

# Extrapolation analysis for gender stratification ------

tic("Extrapolation analysis for gender stratification")

info(logger, 'Extrapolation analysis for gender stratification START')

# Initiate lists to store output within loop ---- 
extrapolations_gender <- list()
gof_haz_gender <- list()
hazot_gender <- list()
parameters_gender <- list()

# Initiate temp lists to store outputs ---- 
extrap_results_temp <- list() # Create empty list for extrapolations (this will be removed)
gof_results_temp <- list() # Create empty list goodness of fit (AIC/BIC)
hazot_results_temp <- list() #Create empty list for hazard over time
parameters_results_temp <- list() #Create empty list for model parameters

# Run extrapolations for all cancers for gender stratification ---
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  
  data <- Pop %>%
    filter(cohort_definition_id == j)
  
  #creates a test that determines if both genders in the data
  genderlevels <- data %>%
    group_by(gender) %>% summarise(count = n()) %>% tally()
  
  # analysis wont run if only 1 gender present
  if(genderlevels == 2){
    
    #carry out extrapolation for each cancer for each extrapolation method
    for(i in 1:length(extrapolations)) {   
      
      if(extrapolations[i] == "spline1") { 
        
        # 1knotspline
        model <- flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 1, scale = "hazard")
        
        #extrapolation # will need this to check results can remove once checked
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All" ) %>%
          rename(Gender = gender)
        
        #grab the parameters from the model
        parameters_results_temp[[i]] <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(value, name) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All") 
        
        # hazard over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All" ) %>%
          rename(Gender = gender)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "GenderStrat" )
        
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
        
      } else if(extrapolations[i] == "spline3") {
        # 3knotspline
        model <- flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 3, scale = "hazard")
        
        #extrapolation # will need this to check results can remove once checked
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All" ) %>%
          rename(Gender = gender)
        
        #grab the parameters from the model
        parameters_results_temp[[i]] <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(value, name) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All") 
        
        # hazard over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All" ) %>%
          rename(Gender = gender)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "GenderStrat" )
        
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
        
      } else if(extrapolations[i] == "spline5") {
        # 5knotspline
        model <- flexsurvspline(formula=Surv(time_years,status-1)~gender,data=data,k = 5, scale = "hazard")
        
        #extrapolation # will need this to check results can remove once checked
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All" ) %>%
          rename(Gender = gender)
        
        #grab the parameters from the model
        parameters_results_temp[[i]] <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(value, name) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All") 
        
        # hazard over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All" ) %>%
          rename(Gender = gender)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "GenderStrat" )
        
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
        
        
      } else {
        
        #carry out models for different parametric methods survival
        model <- flexsurvreg(Surv(time_years, status)~gender, data=data, dist=extrapolations[i])
        
        # extrapolations
        extrap_results_temp[[i]] <- model %>%
          summary(t=t/365, tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All" ) %>%
          rename(Gender = gender)
        
        #grab the parameters from the model
        parameters_results_temp[[i]]  <- model[["coefficients"]] %>%
          enframe() %>%
          pivot_wider(value, name) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All")  
        
        #extract the hazard function over time
        hazot_results_temp[[i]] <- model %>%
          summary(t=(t + 1)/365, type = "hazard",tidy = TRUE) %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All" ) %>%
          rename(Gender = gender)
        
        #get the goodness of fit for each model
        gof_results_temp[[i]] <- model %>%
          glance() %>%
          mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "GenderStrat" )
        
        #print out progress               
        print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
        
      }
      
      #combine all results
      extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
      gofcombined <- dplyr::bind_rows(gof_results_temp)
      hotcombined <- dplyr::bind_rows(hazot_results_temp) %>%
        filter(time > 0) # remove rows with inf/NAs
      
      
      #put the results from each cancer in separate list
      extrapolations_gender[[j]] <- extrapolatedcombined
      gof_haz_gender[[j]] <- gofcombined
      hazot_gender[[j]] <- hotcombined
      parameters_gender[[j]] <-  parameters_results_temp
      
      
    }
    
    #print out progress               
    print(paste0(outcome_cohorts$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
    
    
  } else {
    
    
    print(paste0("Gender stratification extrapolation analysis not carried out for ", outcome_cohorts$cohortName[j], " due to only 1 gender present " , Sys.time()))
    
  }
  
  
}

# Merge results together from each cancer and extrapolation into a dataframe ---
extrapolatedfinalGender <- dplyr::bind_rows(extrapolations_gender)
goffinalGender <- dplyr::bind_rows(gof_haz_gender)
hazardotfinalGender <- dplyr::bind_rows(hazot_gender)

#save files in results folder ---
Results_GENDER <- list("extrapolation_gender" = extrapolatedfinalGender, 
                       "hazardrate_gender" = hazardotfinalGender,
                       "GOF_gender" = goffinalGender)

#write results to excel ---
openxlsx::write.xlsx(Results_GENDER, file = here("Results", db.name , "cancer_extrapolation_results_GENDER.xlsx"))

# extracting parameters for each model for each cancer
# create empty lists for parameters extraction
GompertzP <- list()
weibullP <- list()
weibullPHP <- list()
ExponentialP <- list()
LoglogP <- list()
LognormP <- list()
GenGammaP <- list()
Spline1kP <- list()
Spline3kP <- list()
Spline5kP <- list()


# pull out the extrapolation parameters in a separate list for each cancer 
# (so for example all exponential parameters for all cancers will be in same list)

for(j in 1:nrow(outcome_cohorts)) { 
  
  GompertzP[[j]] <- parameters_gender[[j]] %>% pluck(1) 
  weibullP[[j]] <- parameters_gender[[j]] %>% pluck(2) 
  weibullPHP[[j]] <- parameters_gender[[j]] %>% pluck(3) 
  ExponentialP[[j]] <- parameters_gender[[j]] %>% pluck(4) 
  LoglogP[[j]] <- parameters_gender[[j]] %>% pluck(5) 
  LognormP[[j]] <- parameters_gender[[j]] %>% pluck(6) 
  GenGammaP[[j]] <- parameters_gender[[j]] %>% pluck(7) 
  Spline1kP[[j]] <- parameters_gender[[j]] %>% pluck(8) 
  Spline3kP[[j]] <- parameters_gender[[j]] %>% pluck(9) 
  Spline5kP[[j]] <- parameters_gender[[j]] %>% pluck(10) 
  
}


# grab the parameters from the list and row bind
GompertzParametersGender <- dplyr::bind_rows(GompertzP)
weibullParametersGender <- dplyr::bind_rows(weibullP)
weibullPHParametersGender <- dplyr::bind_rows(weibullPHP)
ExponentialParametersGender <- dplyr::bind_rows(ExponentialP)
LoglogParametersGender <- dplyr::bind_rows(LoglogP)
LognormParametersGender <- dplyr::bind_rows(LognormP)
GenGammaParametersGender <- dplyr::bind_rows(GenGammaP)
Spline1kParametersGender <- dplyr::bind_rows(Spline1kP)
Spline3kParametersGender <- dplyr::bind_rows(Spline3kP)
Spline5kParametersGender <- dplyr::bind_rows(Spline5kP)

#save files in results folder ---
Results_Parameters_GENDER <- list(
  "GompertzParametersGender" =  GompertzParametersGender ,
  "weibullParametersGender" =  weibullParametersGender ,
  "weibullPHParametersGender" =  weibullPHParametersGender,
  "ExponentialParametersGender" = ExponentialParametersGender,
  "LoglogParametersGender" = LoglogParametersGender,
  "LognormParametersGender" =  LognormParametersGender,
  "GenGammaParametersGender" = GenGammaParametersGender,
  "Spline1kParametersGender" = Spline1kParametersGender,
  "Spline3kParametersGender" = Spline3kParametersGender,
  "Spline5kParametersGender" = Spline5kParametersGender)

#write results to excel ---
openxlsx::write.xlsx(Results_Parameters_GENDER, file = here("Results", db.name , "cancer_extrapolation_modelParameters_GENDER.xlsx"))

toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for gender stratification COMPLETE')
