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
             Gender = c(rep("Female", nlevels(data$age_gr) ), rep("Male", nlevels(data$age_gr))) ,
             Age = rep(c("<30" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90"), 2)) %>%
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
            separate(col = "GenderAge", into = c("Gender", "Age"), sep = "_")
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    
  } else {
    
    
    
    
print(paste0("Gender*Age stratification KM analysis not carried out for ", outcome_cohorts$cohortName[j], " due to only 1 gender present age stratification will have results " , Sys.time()))
    
    
    
  }
  

}



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
  #for(j in 1:2) { 
  
  data <- Pop %>%
    filter(cohort_definition_id == j) 
  
  #filter data removing data groups with > 75% missingness
  data <- data %>%
    filter((genderAgegp %in% target_age_gender[[j]])) %>%
    droplevels()
  
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
      model <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data,k = 3, scale = "hazard")
      
      #extrapolation # will need this to check results can remove once checked
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = age_gr, Gender = "Both" )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both") 
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "AgeStrat", Gender = "Both" )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else if(extrapolations[i] == "spline5") {
      
      # 5knotspline
      model <- flexsurvspline(formula=Surv(time_years,status-1)~age_gr,data=data,k = 5, scale = "hazard")
      
      #extrapolation # will need this to check results can remove once checked
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = age_gr, Gender = "Both" )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both") 
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "AgeStrat", Gender = "Both" )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    } else {
      
      #carry out models for different parametric methods survival
      model <- flexsurvreg(Surv(time_years, status)~age_gr, data=data, dist=extrapolations[i])
      
      # extrapolations
      extrap_results_temp[[i]] <- model %>%
        summary(t=t/365, tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = age_gr, Gender = "Both" )
      
      #grab the parameters from the model
      parameters_results_temp[[i]] <- model[["coefficients"]] %>%
        enframe() %>%
        pivot_wider(value, name) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both") 
      
      # hazard over time
      hazot_results_temp[[i]] <- model %>%
        summary(t=(t + 1)/365, type = "hazard" , tidy = TRUE) %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Gender = "Both" ) %>%
        rename(Age = age_gr)
      
      #get the goodness of fit for each model
      gof_results_temp[[i]] <- model %>%
        glance() %>%
        mutate(Method = extrapolations_formatted[i], Cancer = outcome_cohorts$cohortName[j], Age = "AgeStrat", Gender = "Both" )
      
      #print out progress               
      print(paste0(extrapolations_formatted[i]," ", Sys.time()," for " ,outcome_cohorts$cohortName[j], " completed"))
      
    }
    
    #combine all results
    extrapolatedcombined <- dplyr::bind_rows(extrap_results_temp)
    gofcombined <- dplyr::bind_rows(gof_results_temp)
    hotcombined <- dplyr::bind_rows(hazot_results_temp) %>%
      filter(time > 0) # remove rows with inf/NAs
    
    
    #put the results from each cancer in separate list
    extrapolations_age[[j]] <- extrapolatedcombined
    gof_haz_age[[j]] <- gofcombined
    hazot_age[[j]] <- hotcombined
    parameters_age[[j]] <-  parameters_results_temp
    
    
  }
  
  #print out progress               
  print(paste0(cohortDefinitionSet$cohortName[j]," Extrapolation Analysis Completed ", Sys.time()))
  
}

# Merge results together from each cancer and extrpolation into a dataframe ---
extrapolatedfinalAgeGender <- dplyr::bind_rows(extrapolations_age_gender) %>%
  mutate(Age = genderAgegp ,
         Age = str_replace(Age, "Female_<30", "<30"),
         Age = str_replace(Age, "Female_30-39", "30-39"),
         Age = str_replace(Age, "Female_40-49", "40-49"),
         Age = str_replace(Age, "Female_50-59", "50-59"),
         Age = str_replace(Age, "Female_60-69", "60-69"),
         Age = str_replace(Age, "Female_70-79", "70-79"),
         Age = str_replace(Age, "Female_80-89", "80-89"),
         Age = str_replace(Age, "Female_>=90", ">=90"),
         Age = str_replace(Age, "Male_<30", "<30"),
         Age = str_replace(Age, "Male_30-39", "30-39"),
         Age = str_replace(Age, "Male_40-49", "40-49"),
         Age = str_replace(Age, "Male_50-59", "50-59"),
         Age = str_replace(Age, "Male_60-69", "60-69"),
         Age = str_replace(Age, "Male_70-79", "70-79"),
         Age = str_replace(Age, "Male_80-89", "80-89"),
         Age = str_replace(Age, "Male_>=90", ">=90"),       
         Gender = genderAgegp,
         Gender = str_replace(Gender, "Female_<30", "Female"),
         Gender = str_replace(Gender, "Female_30-39", "Female"),
         Gender = str_replace(Gender, "Female_40-49", "Female"),
         Gender = str_replace(Gender, "Female_50-59", "Female"),
         Gender = str_replace(Gender, "Female_60-69", "Female"),
         Gender = str_replace(Gender, "Female_70-79", "Female"),
         Gender = str_replace(Gender, "Female_80-89", "Female"),
         Gender = str_replace(Gender, "Female_>=90", "Female"),
         Gender = str_replace(Gender, "Male_<30", "Male"),
         Gender = str_replace(Gender, "Male_30-39", "Male"),
         Gender = str_replace(Gender, "Male_40-49", "Male"),
         Gender = str_replace(Gender, "Male_50-59", "Male"),
         Gender = str_replace(Gender, "Male_60-69", "Male"),
         Gender = str_replace(Gender, "Male_70-79", "Male"),
         Gender = str_replace(Gender, "Male_80-89", "Male"),
         Gender = str_replace(Gender, "Male_>=90", "Male")
  )

goffinalAgeGender <- dplyr::bind_rows(gof_haz_age_gender) %>%
  mutate(Age = "All" ,
         Gender = "Both",
         genderAgegp = "genderAgegp")

hazardotfinalAgeGender <- dplyr::bind_rows(hazot_age_gender)%>%
  mutate(Age = genderAgegp ,
         Age = str_replace(Age, "Female_<30", "<30"),
         Age = str_replace(Age, "Female_30-39", "30-39"),
         Age = str_replace(Age, "Female_40-49", "40-49"),
         Age = str_replace(Age, "Female_50-59", "50-59"),
         Age = str_replace(Age, "Female_60-69", "60-69"),
         Age = str_replace(Age, "Female_70-79", "70-79"),
         Age = str_replace(Age, "Female_80-89", "80-89"),
         Age = str_replace(Age, "Female_>=90", ">=90"),
         Age = str_replace(Age, "Male_<30", "<30"),
         Age = str_replace(Age, "Male_30-39", "30-39"),
         Age = str_replace(Age, "Male_40-49", "40-49"),
         Age = str_replace(Age, "Male_50-59", "50-59"),
         Age = str_replace(Age, "Male_60-69", "60-69"),
         Age = str_replace(Age, "Male_70-79", "70-79"),
         Age = str_replace(Age, "Male_80-89", "80-89"),
         Age = str_replace(Age, "Male_>=90", ">=90"),       
         Gender = genderAgegp,
         Gender = str_replace(Gender, "Female_<30", "Female"),
         Gender = str_replace(Gender, "Female_30-39", "Female"),
         Gender = str_replace(Gender, "Female_40-49", "Female"),
         Gender = str_replace(Gender, "Female_50-59", "Female"),
         Gender = str_replace(Gender, "Female_60-69", "Female"),
         Gender = str_replace(Gender, "Female_70-79", "Female"),
         Gender = str_replace(Gender, "Female_80-89", "Female"),
         Gender = str_replace(Gender, "Female_>=90", "Female"),
         Gender = str_replace(Gender, "Male_<30", "Male"),
         Gender = str_replace(Gender, "Male_30-39", "Male"),
         Gender = str_replace(Gender, "Male_40-49", "Male"),
         Gender = str_replace(Gender, "Male_50-59", "Male"),
         Gender = str_replace(Gender, "Male_60-69", "Male"),
         Gender = str_replace(Gender, "Male_70-79", "Male"),
         Gender = str_replace(Gender, "Male_80-89", "Male"),
         Gender = str_replace(Gender, "Male_>=90", "Male")
  )


# extract results for 1,5,10 years for extrapolated data
# extrapolation_predAgeGender <- subset(extrapolatedfinalAge, extrapolatedfinalAge$time == 1 |
#                                   extrapolatedfinalAge$time == 5 |
#                                   extrapolatedfinalAge$time == 10  )


# catch to remove hazard extrapolation where hazard cant be generated on observed data
# #create the filters pulls out the age and the cancer type where there is no data
# filteragegender <- as.data.frame(table(hotkmcombined_age_gender$genderAgegp, hotkmcombined_age_gender$Cancer)) %>%
#   rename(AgeGender = Var1, Cancer = Var2, n = Freq ) %>%
#   filter(n == 0) %>% 
#   mutate_if(is.factor, as.character) %>%
#   pull(AgeGender) 
# 
# filtercancer <- as.data.frame(table(hotkmcombined_age_gender$genderAgegp, hotkmcombined_age_gender$Cancer)) %>%
#   rename(AgeGender = Var1, Cancer = Var2, n = Freq ) %>%
#   filter(n == 0) %>% 
#   mutate_if(is.factor, as.character) %>%
#   pull(Cancer) 
# 
# #filter out the extrapolated data which doesnt have hazard over time for observed
# hazardotfinalAgeGender <- hazardotfinalAgeGender %>%
#   filter(!Cancer %in% filtercancer | !Age %in% filterage ) 


#save files in results folder ---
Results_AGEGENDER <- list("extrapolation_age_gender" = extrapolatedfinalAgeGender, 
                          "hazardrate_age_gender" = hazardotfinalAgeGender,
                          "GOF_age_gender" = goffinalAgeGender)

#write results to excel ---
openxlsx::write.xlsx(Results_AGEGENDER, file = here("Results", db.name , "cancer_extrapolation_results_AGEGENDER.xlsx"))

# extrapolatedfinalAge <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_AGE.xlsx"), sheet = 1)
# hazardotfinalAge <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_AGE.xlsx"), sheet = 2)
# goffinalAge <- read.xlsx(xlsxFile = here("Results", db.name , "cancer_extrapolation_results_AGE.xlsx"), sheet = 3)

info(logger, 'Extrapolation analysis for age*gender stratification COMPLETE')

