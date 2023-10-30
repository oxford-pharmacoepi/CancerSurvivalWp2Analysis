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
  mutate(group_level = "All 8 Cancers")

)

tableone <- bind_rows(tableone, tableone_all_cancers) 
  
  
}

# tidy up the table ones for papers and shiny


tableone_summary_breast <- tableone %>%
  filter(group_level == "Breastcancer" & strata_name == "Overall")

reformat_table_one <- function(table_one_summary){

  reformatted_table1 <- data.frame(x = character(),  y = character())
  
  n1 <- table_one_summary %>% dplyr::filter(variable == "Number subjects") %>% dplyr::pull(estimate)
  
  reformatted_table1 <- rbind(reformatted_table1,
                              data.frame(x = paste0("n"),
                                         y = paste0(n1)))

  # variables assembled by min/max etc
  cat_var <- table_one_summary %>% dplyr::filter(estimate_type == "min") %>% dplyr::select(variable) %>% dplyr::distinct() %>% dplyr::pull(variable)
  
  for (i in (1:length(cat_var))){
    reformatted_table1 <- rbind(reformatted_table1,
                                data.frame(x = paste0(cat_var[[i]], ": median (IQR)"),
                                           y = paste0(table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "median") %>% dplyr::pull(estimate),
                                                      " (",
                                                      table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q25") %>% dplyr::pull(estimate),
                                                      " - ",
                                                      table_one_summary %>% dplyr::filter(variable == cat_var[[i]]) %>% dplyr::filter(estimate_type == "q75") %>% dplyr::pull(estimate),
                                                      ")"))
    )
  }

  # age group variables
  age_var <- table_one_summary %>%
    dplyr::filter(variable == "Age group") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull()

  for (i in (1:length(age_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0("Age Group: ", age_var[[i]], " n (%)"),
                                                               y = paste0(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          #round(as.numeric(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)), digits = 1),
                                                                          as.numeric(table_one_summary %>% dplyr::filter(variable_level == age_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)),
                                                                          ")")) )
  }


  #condition variables
  condition_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Conditions flag -inf')) %>%
    dplyr::filter(!variable_level == "Malignant neoplastic disease") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)

  for (i in (1:length(condition_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(condition_var[[i]], " n (%)"),
                                                               y = paste0(table_one_summary %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          as.numeric(table_one_summary %>% dplyr::filter(variable_level == condition_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)),
                                                                          ")")))
  }

  #medication variables
  medication_var <- table_one_summary %>%
    dplyr::filter(stringr::str_detect(variable, 'Medications flag -365')) %>%
    dplyr::filter(!variable == "Ref medications antineoplastic agents") %>%
    dplyr::select(variable_level) %>%
    dplyr::distinct() %>%
    dplyr::pull(variable_level)

  for (i in (1:length(medication_var))){
    reformatted_table1 <- rbind(reformatted_table1, data.frame(x = paste0(medication_var[[i]], " n (%)"),
                                                               y = paste0(table_one_summary %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "count") %>% dplyr::pull(estimate),
                                                                          " (",
                                                                          as.numeric(table_one_summary %>% dplyr::filter(variable_level == medication_var[[i]]) %>% dplyr::filter(estimate_type == "percentage") %>% dplyr::pull(estimate)),
                                                                          ")")))
  }
  reformatted_table1 <- reformatted_table1 %>% dplyr::distinct()

  ###rename columns
  colnames(reformatted_table1) <- c("Description", "Value") 
  
  return(reformatted_table1)
  
}

testy <- reformat_table_one(tableone_summary_breast)




