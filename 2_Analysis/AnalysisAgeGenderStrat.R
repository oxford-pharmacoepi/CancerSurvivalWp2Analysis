########################################
# GENDER*AGE STRATIFICATION
########################################

info(logger, 'KM analysis for age*gender stratification START')

# KM observed
observedkm_age_gender <- list()
observedmedianKM_age_gender <- list()
observedhazotKM_age_gender <- list()
observedrisktableKM_age_gender <- list()
target_age_gender <- list()

# loop to carry out for each cancer
for(j in 1:nrow(outcome_cohorts)) { 
  
  #subset the data by cancer type
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  # add a filter than removes data with 75% missingness
  grid <- seq(0,floor(max(data$time_years)),by=2)
  filter4genderage <- RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_<30"]) %>%
    rbind(grid) %>% as.data.frame() %>%
    `colnames<-`(grid) %>%
    slice(1) %>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_30-39"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_40-49"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_50-59"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_60-69"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_70-79"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_80-89"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_>=90"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_<30"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_30-39"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_40-49"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_50-59"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_60-69"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_70-79"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_80-89"]))%>%
    rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_>=90"]))%>%
    mutate(Method = "Kaplan-Meier", 
           Cancer = outcome_cohorts$cohortName[j],
           Gender = c(rep("Female", nlevels(data$age_gr) ), rep("Male", nlevels(data$age_gr))) ,
           Age = rep(c("<30" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90"), 2)) 
  
  # replace values < 5 in the data with 0 for filter
  filterdatatest <- filter4genderage %>%
    mutate_at(.vars = c(1:(ncol(filter4genderage)-4)), funs(ifelse(.== 0, NA, .))) %>%  
    mutate_at(.vars = c(1:(ncol(filter4genderage)-4)), funs(ifelse(.<= 5, 0, .))) %>%
    replace(is.na(.), 0) 
  
  # calculate the number of columns 
  elgcols <- ncol(filterdatatest) - 4 
  
  #count the number of zeros across the rows
  filterdatatest <- filterdatatest %>% 
    unite("GenderAge", c(Gender, Age), remove = FALSE) %>%
    mutate(count=rowSums(.[1:elgcols]==0), percentzero = ((count/elgcols)*100) ) %>%
    filter(percentzero != 75) %>%
    filter(percentzero < 75)
  
  #create filter function to put into results below
  target_age_gender[[j]] <- filterdatatest$GenderAge
  
  #filter data removing data groups with > 75% missingness
  data <- data %>%
    filter((genderAgegp %in% target_age_gender[[j]])) %>%
  droplevels()
  
  #creates a test that determines if both genders in the data
  genderlevels <- data %>%
    group_by(gender) %>% summarise(count = n()) %>% tally()
  
  if(genderlevels == 2){
    
    # get the risk table for data after filtering for missingness ---
    grid <- seq(0,floor(max(data$time_years)),by=2)
    observedrisktableKM_age_gender[[j]] <- RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_<30"]) %>%
      rbind(grid) %>% as.data.frame() %>%
      `colnames<-`(grid) %>%
      slice(1) %>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_30-39"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_40-49"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_50-59"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_60-69"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_70-79"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_80-89"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Female_>=90"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_<30"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_30-39"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_40-49"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_50-59"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_60-69"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_70-79"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_80-89"]))%>%
      rbind(RiskSetCount(grid,data$time_years[data$genderAgegp == "Male_>=90"]))%>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohortName[j],
             Age = filter4genderage$Age ,
            Gender = filter4genderage$Gender ) %>%
    unite("GenderAge", c(Gender, Age), remove = FALSE) %>%
      filter((GenderAge %in% target_age_gender[[j]]))
    
    print(paste0("Extract risk table ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    #carry out km estimate ---
    observedkm_age_gender[[j]] <- survfit (Surv(time_years, status) ~ genderAgegp, data=data) %>%
      tidy() %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], 
             Age = strata ,
             Age = str_replace(Age, "genderAgegp=Female_<30", "<30"),
             Age = str_replace(Age, "genderAgegp=Female_30-39", "30-39"),
             Age = str_replace(Age, "genderAgegp=Female_40-49", "40-49"),
             Age = str_replace(Age, "genderAgegp=Female_50-59", "50-59"),
             Age = str_replace(Age, "genderAgegp=Female_60-69", "60-69"),
             Age = str_replace(Age, "genderAgegp=Female_70-79", "70-79"),
             Age = str_replace(Age, "genderAgegp=Female_80-89", "80-89"),
             Age = str_replace(Age, "genderAgegp=Female_>=90", ">=90"),
             Age = str_replace(Age, "genderAgegp=Male_<30", "<30"),
             Age = str_replace(Age, "genderAgegp=Male_30-39", "30-39"),
             Age = str_replace(Age, "genderAgegp=Male_40-49", "40-49"),
             Age = str_replace(Age, "genderAgegp=Male_50-59", "50-59"),
             Age = str_replace(Age, "genderAgegp=Male_60-69", "60-69"),
             Age = str_replace(Age, "genderAgegp=Male_70-79", "70-79"),
             Age = str_replace(Age, "genderAgegp=Male_80-89", "80-89"),
             Age = str_replace(Age, "genderAgegp=Male_>=90", ">=90"),       
             Gender = strata,
             Gender = str_replace(Gender, "genderAgegp=Female_<30", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_30-39", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_40-49", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_50-59", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_60-69", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_70-79", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_80-89", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Female_>=90", "Female"),
             Gender = str_replace(Gender, "genderAgegp=Male_<30", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_30-39", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_40-49", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_50-59", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_60-69", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_70-79", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_80-89", "Male"),
             Gender = str_replace(Gender, "genderAgegp=Male_>=90", "Male"),
             strata = str_replace(strata, "genderAgegp=", "") ) %>%
      rename("GenderAge" = "strata")
    
    print(paste0("KM for observed data age strat ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    
    # KM median survival---
    modelKM <- survfit (Surv(time_years, status) ~ genderAgegp, data=data) %>%
      summary()
    
    observedmedianKM_age_gender[[j]] <- modelKM$table %>%
      as.data.frame() %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohortName[j],
             GenderAge = rownames(modelKM$table), 
             GenderAge = str_replace(GenderAge, "genderAgegp=", "")) %>%
            separate(col = "GenderAge", into = c("Gender", "Age"), sep = "_", remove = FALSE)
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    # hazard function over time ----
    observedhazotKM_age_gender[[j]] <- group_by(data,genderAgegp) %>% 
      do(as.data.frame(bshazard(Surv(time_years, status)~1, data=., verbose=FALSE))) %>% 
      ungroup %>%
      mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohortName[j], Gender = "Both", Age = "All")
    
    print(paste0("Hazard over time results ", Sys.time()," for ",outcome_cohorts$cohortName[j], " age strat completed"))
    
    
    
  } else {
    
    
    
    
print(paste0("Gender*Age stratification KM analysis not carried out for ", outcome_cohorts$cohortName[j], " due to only 1 gender present age stratification will have results " , Sys.time()))
    
    
    
  }
  

}



# take the results from a list (one element for each cancer) and put into dataframe ----
observedkmcombined_age_gender <- dplyr::bind_rows(observedkm_age_gender) %>%
  rename(est = estimate ,ucl = conf.high, lcl = conf.low )

medkmcombined_age_gender <- dplyr::bind_rows(observedmedianKM_age_gender) 

hotkmcombined_age_gender <- dplyr::bind_rows(observedhazotKM_age_gender) %>%
  rename(est = hazard, ucl = upper.ci, lcl = lower.ci, GenderAge = genderAgegp )

#generate the risk table and remove entries < 5 patients
risktableskm_age_gender <- dplyr::bind_rows(observedrisktableKM_age_gender) %>%
  replace(is.na(.), 0) 


ResultsKM_AGEGENDER <- list("KM_observed_age_gender" = observedkmcombined_age_gender, 
                      "KM_MedianSur_age_gender" = medkmcombined_age_gender,
                      "KM_hazard_rate_age_gender" = hotkmcombined_age_gender,
                      "KM_risktable_age_gender" = risktableskm_age_gender)

#write to excel
openxlsx::write.xlsx(ResultsKM_AGEGENDER, file = here("Results", db.name ,"cancer_KM_observed_results_AGEGENDER.xlsx"))

toc(func.toc=toc_min)

info(logger, 'KM analysis for AGE*GENDER stratification COMPLETE')


###########################################

# Extrapolation analysis for age*gender stratification ------

info(logger, 'Extrapolation analysis for age*gender stratification START')

# generating extrapolations ----
# Initiate templists to store output ---- will have to make folders for each cancer and loop
extrapolations_age_gender <- list()
gof_haz_age_gender <- list()
hazot_age_gender <- list()
parameters_age_gender <- list()

# Initiate templists to store output ---- 
extrap_results_temp <- list() # Create empty list for extrapolations
gof_results_temp <- list() # required to assess goodness of fit (AIC/BIC)
hazot_results_temp <- list() #required
parameters_results_temp <- list()

# Run extrapolations for all cancers for age_gender extrapolation ---
for(j in 1:nrow(cohortDefinitionSet)) { 
  
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  #filter data removing data groups with > 75% missingness
  data <- data %>%
    filter((genderAgegp %in% target_age_gender[[j]])) %>%
    droplevels()
  
  #creates a test that determines if both genders in the data
  genderlevels <- data %>%
    group_by(gender) %>% summarise(count = n()) %>% tally()
  
  if(genderlevels == 2){
    
  #carry out extrapolation for each cancer
  
  for(i in 1:length(extrapolations)) {   
    
    if(extrapolations[i] == "spline1") { 
      
      # 1knotspline
      model <- flexsurvspline(formula=Surv(time_years,status-1)~genderAgegp,data=data,k = 1, scale = "hazard")
      
      #extrapolation # will need this to check results can remove once checked
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = genderAgegp  )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = "AgeGender"  )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = genderAgegp  )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = "AgeGender"  )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else if (extrapolations[i] == "spline3") {
      
      # 3knotspline
      model <- flexsurvspline(formula=Surv(time_years,status-1)~genderAgegp,data=data,k = 3, scale = "hazard")
      
      #extrapolation # will need this to check results can remove once checked
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = genderAgegp  )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = "AgeGender"  )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = genderAgegp  )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = "AgeGender"  )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline5") {
      
      # 5knotspline
      model <- flexsurvspline(formula=Surv(time_years,status-1)~genderAgegp,data=data,k = 5, scale = "hazard")
      
      #extrapolation # will need this to check results can remove once checked
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = genderAgegp  )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = "AgeGender"  )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = genderAgegp  )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = "AgeGender"  )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else {
      
      #carry out models for different parametric methods survival
      model <- flexsurvreg(Surv(time_years, status)~genderAgegp, data=data, dist=extrapolations[i])
      
      # extrapolations
      #extrapolation # will need this to check results can remove once checked
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = genderAgegp  )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = "AgeGender"  )
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = genderAgegp  )
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "All", Gender = "Both", AgeGender = "AgeGender"  )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    }
    
    #combine all results
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_results_temp) %>%
      filter(time > 0) # remove rows with inf/NAs
    
    
    #put the results from each cancer in separate list
    extrapolations_age_gender[[j]] <- extrapolatedcombined
    gof_haz_age_gender[[j]] <- gofcombined
    hazot_age_gender[[j]] <- hotcombined
    parameters_age_gender[[j]] <-  parameters_results_temp
    
    
  }
  
  #print out progress               
  print(paste0(cohortDefinitionSet$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  

  
} else {
  
  
  
  
  print(paste0("Gender*Age stratification KM analysis not carried out for ", outcome_cohorts$cohortName[j], " due to only 1 gender present age stratification will have results " , Sys.time()))
  
  
  
}
  
  
}

# Merge results together from each cancer and extrpolation into a dataframe ---
extrapolatedfinalAgeGender <- dplyr::bind_rows(extrapolations_age_gender) 
goffinalAgeGender <- dplyr::bind_rows(gof_haz_age_gender) 
hazardotfinalAgeGender <- dplyr::bind_rows(hazot_age_gender)



#save files in results folder ---
Results_AGEGENDER <- list("extrapolation_age_gender" = extrapolatedfinalAgeGender, 
                          "hazardrate_age_gender" = hazardotfinalAgeGender,
                          "GOF_age_gender" = goffinalAgeGender)

#write results to excel ---
openxlsx::write.xlsx(Results_AGEGENDER, file = here("Results", db.name , "cancer_extrapolation_results_AGEGENDER.xlsx"))


# extracting parameters for each model for each cancer ----
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
  
  GompertzP[[j]] <- parameters_age_gender[[j]] %>% pluck(1) 
  weibullP[[j]] <- parameters_age_gender[[j]] %>% pluck(2) 
  weibullPHP[[j]] <- parameters_age_gender[[j]] %>% pluck(3) 
  ExponentialP[[j]] <- parameters_age_gender[[j]] %>% pluck(4) 
  LoglogP[[j]] <- parameters_age_gender[[j]] %>% pluck(5) 
  LognormP[[j]] <- parameters_age_gender[[j]] %>% pluck(6) 
  GenGammaP[[j]] <- parameters_age_gender[[j]] %>% pluck(7) 
  Spline1kP[[j]] <- parameters_age_gender[[j]] %>% pluck(8) 
  Spline3kP[[j]] <- parameters_age_gender[[j]] %>% pluck(9) 
  Spline5kP[[j]] <- parameters_age_gender[[j]] %>% pluck(10) 
  
}

# grab the parameters from the list and row bind
GompertzParametersAgeGender <- dplyr::bind_rows(GompertzP)
weibullParametersAgeGender <- dplyr::bind_rows(weibullP)
weibullPHParametersAgeGender <- dplyr::bind_rows(weibullPHP)
ExponentialParametersAgeGender <- dplyr::bind_rows(ExponentialP)
LoglogParametersAgeGender <- dplyr::bind_rows(LoglogP)
LognormParametersAgeGender <- dplyr::bind_rows(LognormP)
GenGammaParametersAgeGender <- dplyr::bind_rows(GenGammaP)
Spline1kParametersAgeGender <- dplyr::bind_rows(Spline1kP)
Spline3kParametersAgeGender <- dplyr::bind_rows(Spline3kP)
Spline5kParametersAgeGender <- dplyr::bind_rows(Spline5kP)

#save files in results folder ---
Results_Parameters_AGE <- list(
  "GompertzParametersAgeGender" =  GompertzParametersAgeGender ,
  "weibullParametersAgeGender" =  weibullParametersAgeGender ,
  "weibullPHParametersAgeGender" =  weibullPHParametersAgeGender,
  "ExponentialParametersAgeGender" = ExponentialParametersAgeGender,
  "LoglogParametersAgeGender" = LoglogParametersAgeGender,
  "LognormParametersAgeGender" =  LognormParametersAgeGender,
  "GenGammaParametersAgeGender" = GenGammaParametersAgeGender,
  "Spline1kParametersAgeGender" = Spline1kParametersAgeGender,
  "Spline3kParametersAgeGender" = Spline3kParametersAgeGender,
  "Spline5kParametersAgeGender" = Spline5kParametersAgeGender)

#write results to excel ---
openxlsx::write.xlsx(Results_Parameters_AGEGENDER, file = here("Results", db.name , "cancer_extrapolation_modelParameters_AGEGENDER.xlsx"))


toc(func.toc=toc_min)

info(logger, 'Extrapolation analysis for age*gender stratification COMPLETE')

