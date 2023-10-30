#creating table one for characterization of cancers

# subset the CDM for analysis table to make code run quicker
info(logger, "SUBSETTING CDM")
cdm <- cdmSubsetCohort(cdm, "analysis")
info(logger, "SUBSETTED CDM")


# depending on database type run different parts of the code for cancer registries
# we probably wont have access past conditions and medication history unless they are linked

if(priorhistory == TRUE){

# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS")
codelistMedications <- codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                                conceptSet = codelistMedications, 
                                name = "medications",
                                overwrite = TRUE)

info(logger, "INSTANTIATED MEDICATIONS")

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS")
codelistConditions <- codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)

cdm <- generateConceptCohortSet(cdm = cdm, 
                               conceptSet = codelistConditions,
                               name = "conditions",
                               overwrite = TRUE)

info(logger, "INSTANTIATED CONDITIONS")


info(logger, "CREATE TABLE ONE SUMMARY")

suppressWarnings(
tableone <- cdm$analysis %>%
  summariseCharacteristics(
    strata = list(c("sex"),c("age_gr")),
    minCellCount = 5,
    ageGroup = list(c(18, 29), c(30, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      )
    ),
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medications", value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = "conditions", value = "flag", window = c(-Inf, 0)
      )
    )
  )
)

suppressWarnings(
  
tableone_all_cancers <- cdm$analysis %>% 
  mutate(cohort_definition_id = 10) %>% 
  summariseCharacteristics(
    strata = list(c("sex"),c("age_gr")),
    minCellCount = 5,
    ageGroup = list(c(18, 29), c(30, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 150)),
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0)
      )
    ),
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medications", value = "flag", window = c(-365, 0)
      ),
      "Conditions" = list(
        targetCohortTable = "conditions", value = "flag", window = c(-Inf, 0)
      )
    )
  )  %>% 
  mutate(group_level = "All 8 Cancers")

)


tableone <- bind_rows(tableone, tableone_all_cancers) 

info(logger, "CREATED TABLE ONE SUMMARY")

} else {
  

info(logger, "CREATE TABLE ONE SUMMARY")

suppressWarnings(
tableone <- cdm$analysis %>%
  summariseCharacteristics(
    strata = list(c("sex"),c("age_gr")),
    minCellCount = 5,
    ageGroup = list(c(18, 29), c(30, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 150))
  )
)

suppressWarnings(
  
tableone_all_cancers <- cdm$analysis %>% 
  mutate(cohort_definition_id = 10) %>% 
  summariseCharacteristics(
    strata = list(c("sex"),c("age_gr")),
    minCellCount = 5,
    ageGroup = list(c(18, 29), c(30, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 150))
  )  %>% 
  mutate(group_level = "All Cancers")

)

info(logger, "CREATED TABLE ONE SUMMARY")

tableone <- bind_rows(tableone, tableone_all_cancers)

}

info(logger, "CREATING TABLE ONE")

# rename cancers with better formats
tableone <- tableone %>% 
  mutate(group_level = replace(group_level, group_level == "Breastcancer", "Breast")) %>%
  mutate(group_level = replace(group_level, group_level == "Crc", "Colorectal")) %>%
  mutate(group_level = replace(group_level, group_level == "Hncancer", "Head and Neck")) %>%
  mutate(group_level = replace(group_level, group_level == "Livercancer", "Liver")) %>%
  mutate(group_level = replace(group_level, group_level == "Lungcancer", "Lung")) %>%
  mutate(group_level = replace(group_level, group_level == "Pancreaticcancer", "Pancreatic")) %>%
  mutate(group_level = replace(group_level, group_level == "Prostatecancer", "Prostate")) %>%
  mutate(group_level = replace(group_level, group_level == "Stomachcancer", "Stomach")) 

# tidy up the table ones for papers and shiny

# overall
tableone_clean_temp <- list()
for(tableonecancer in 1:length(unique(tableone$group_level))) {
  
  tabledata <- tableone %>%
    filter(group_level == unique(tableone$group_level)[tableonecancer]) %>% 
    filter(strata_name == "Overall")
  
   tb1_temp <- reformat_table_one(tabledata) %>% 
     mutate(Cancer = unique(tabledata$group_level),
            Stratification = "none",
            Sex = "Both" ,
            Age = "All" ,
            Database = db.name)
  
  
   tableone_clean_temp[[tableonecancer]] <- tb1_temp
   rm(tb1_temp)
  
}
tableone_overall <- dplyr::bind_rows(tableone_clean_temp) 

# by sex
tableone_clean_temp <- list()
for(tableonecancer in 1:length(unique(tableone$group_level))) {
  
  
  tabledata <- tableone %>%
    filter(group_level == unique(tableone$group_level)[tableonecancer]) %>% 
    filter(strata_name == "sex") 
  
  if(unique(tableone$group_level)[tableonecancer] != "Prostate") {
  
  tb1_tempF <- tabledata %>% 
    filter(strata_level == "Female") %>% 
    reformat_table_one() %>% 
    mutate(Cancer = unique(tabledata$group_level),
           Stratification = "Sex",
           Sex = "Female" ,
           Age = "All" ,
           Database = db.name)
  
  tb1_tempM <- tabledata %>% 
    filter(strata_level == "Male") %>% 
    reformat_table_one() %>% 
    mutate(Cancer = unique(tabledata$group_level),
           Stratification = "Sex",
           Sex = "Male" ,
           Age = "All" ,
           Database = db.name)
  
  #combine sexes together
  tb1_temp <- bind_rows(tb1_tempF, tb1_tempM)
  
  rm(tb1_tempF, tb1_tempM)
  
  } else {
    
    tb1_tempM <- tabledata %>% 
      filter(strata_level == "Male") %>% 
      reformat_table_one() %>% 
      mutate(Cancer = unique(tabledata$group_level),
             Stratification = "Sex",
             Sex = "Male" ,
             Age = "All" ,
             Database = db.name)
    
    tb1_temp <- tb1_tempM
  }
  
  tableone_clean_temp[[tableonecancer]] <- tb1_temp
  
  rm(tb1_temp )
  
}
tableone_sex <- dplyr::bind_rows(tableone_clean_temp) 

# by age
tableone_clean_temp <- list()
for(tableonecancer in 1:length(unique(tableone$group_level))) {
  
  tabledata <- tableone %>%
    filter(group_level == unique(tableone$group_level)[tableonecancer]) %>% 
    filter(strata_name == "age_gr") 
  
tb1_temp_age <- list()
for(z in 1:length(unique(tabledata$strata_level))) {
  

  
# because some age groups do not have data need to have try catches to make sure loop still continues even if data not available
  tryCatch(
    {
      tb1_temp_age[[z]] <- tabledata %>% 
        filter(strata_level == unique(tabledata$strata_level)[z]) %>% 
        reformat_table_one() %>% 
        mutate(Cancer = unique(tabledata$group_level),
               Stratification = "Age",
               Sex = "Both",
               Age =  unique(tabledata$strata_level)[z] ,
               Database = db.name) %>% 
        dplyr::filter(!stringr::str_detect(Description, 'Age Group:'))
      
      },
    error = function(e) {info(logger, paste0(" Table one for 18 to 29 year olds not carried out for  ",unique(tableone$group_level)[tableonecancer], "see log for more information", e))},
    warning = function(w){info(logger, paste0(unique(tableone$group_level)[tableonecancer], ": ", w))}
  )  
}
  
tableone_clean_temp[[tableonecancer]] <- bind_rows(tb1_temp_age)
  
  rm(tb1_temp_age)
}

tableone_age <- dplyr::bind_rows(tableone_clean_temp) 


# combine all tableone outputs
tableone_final <- dplyr::bind_rows(tableone_overall, tableone_sex, tableone_age)


info(logger, "CREATED TABLE ONE")
