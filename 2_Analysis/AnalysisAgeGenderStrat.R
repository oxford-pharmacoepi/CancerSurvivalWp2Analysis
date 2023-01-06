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
    
    observedmedianKM_age_gender[[j]] <- 
      
      test <- modelKM$table %>%
      as.data.frame() %>%
      mutate(Method = "Kaplan-Meier", 
             Cancer = outcome_cohorts$cohortName[j],
             GenderAge = rownames(modelKM$table), 
             GenderAge = str_replace(GenderAge, "genderAgegp=", "")  )
    
             Gender = c(rep("Female", nlevels(data$age_gr) ), rep("Male", nlevels(data$age_gr))) ,
             Age = rep(c("<30" ,"30-39", "40-49" ,"50-59" ,"60-69", "70-79", "80-89" ,">=90"), 2))
    
    
    print(paste0("Median survival from KM from observed data ", Sys.time()," for ",outcome_cohorts$cohortName[j], " completed"))
    
    
    
    
  } else {
    
# code for running when only 1 gender for age*gender extrapolations
    
  }
  

}

