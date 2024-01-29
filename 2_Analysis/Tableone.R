#creating table one for characterization of cancers
info(logger, "CREATING TABLE ONE SUMMARY")

print(paste0("Starting table one characterisations ", Sys.time()))

# runs for those with prior history as TRUE comorbidities and medication use is captured
if(priorhistory == TRUE){  

# subset the CDM for analysis table to make code run quicker
info(logger, "SUBSETTING CDM FOR CHARACTERISATION")
cdm <- CDMConnector::cdmSubsetCohort(cdm, "outcome")
info(logger, "SUBSETTED FOR CHARACTERISATION")


# instantiate medications
info(logger, "INSTANTIATE MEDICATIONS")
codelistMedications <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Medications"), cdm)

cdm <- DrugUtilisation::generateDrugUtilisationCohortSet(cdm = cdm, 
                                conceptSet = codelistMedications, 
                                name = "medications")

info(logger, "INSTANTIATED MEDICATIONS")

# instantiate conditions
info(logger, "INSTANTIATE CONDITIONS")
codelistConditions <- CodelistGenerator::codesFromConceptSet(here("1_InstantiateCohorts", "Conditions"), cdm)

cdm <- CDMConnector::generateConceptCohortSet(cdm = cdm, 
                               conceptSet = codelistConditions,
                               name = "conditions",
                               overwrite = TRUE)

info(logger, "INSTANTIATED CONDITIONS")

# instantiate obesity using diagnosis and measurements
info(logger, "INSTANTIATE OBESITY")

obesity_cohorts <- CDMConnector::readCohortSet(here::here(
  "1_InstantiateCohorts",
  "Obesity" 
))

cdm <- CDMConnector::generateCohortSet(cdm = cdm, 
                                              cohortSet = obesity_cohorts, 
                                              name = "obesity",
                                              computeAttrition = TRUE,
                                              overwrite = TRUE)

info(logger, "INSTANTIATED OBESITY")

info(logger, "CREATING TABLE ONE SUMMARY")

suppressWarnings(
  
tableone <- cdm$outcome %>%
  PatientProfiles::summariseCharacteristics(
    strata = list(c("sex"),c("age_gr"), c("sex", "age_gr" )),
    minCellCount = 10,
    tableIntersect = list(
      "Visits" = list(
        tableName = "visit_occurrence", value = "count", window = c(-365, 0))),
    cohortIntersect = list(
      "Medications" = list(
        targetCohortTable = "medications", value = "flag", window = c(-365, 0)),
      "Conditions" = list(
        targetCohortTable = "conditions", value = "flag", window = c(-Inf, 0)),
      "Obesity" = list(
        targetCohortTable = "obesity", value = "flag", window = c(-Inf, 0))
  
    )
  )

)

tableone <- tableone %>% 
  select(-c(result_type))


suppressWarnings(
  
  tableone_all_cancers <- cdm$outcome %>% 
    dplyr::mutate(cohort_definition_id = 10) %>% 
    PatientProfiles::summariseCharacteristics(
      strata = list(c("sex"),c("age_gr"), c("sex", "age_gr" )),
      minCellCount = 10,
      tableIntersect = list(
        "Visits" = list(
          tableName = "visit_occurrence", value = "count", window = c(-365, 0))),
      cohortIntersect = list(
        "Medications" = list(
          targetCohortTable = "medications", value = "flag", window = c(-365, 0)),
        "Conditions" = list(
          targetCohortTable = "conditions", value = "flag", window = c(-Inf, 0)),
        "outcome" = list(
          targetCohortTable = "outcome", value = "flag", window = c(0, 0)),
        "Obesity" = list(
          targetCohortTable = "obesity", value = "flag", window = c(-Inf, 0)
        )
      )
      
    ) %>% 
    
    dplyr::mutate(group_level = "cohort_name", 
                  group_name = "Overall") %>% 
    select(-c(result_type))
  
)


tableone_final <- dplyr::bind_rows(tableone, tableone_all_cancers) 

info(logger, "CREATED TABLE ONE SUMMARY")

} else {
  
info(logger, "CREATING TABLE ONE SUMMARY")
  
  tableone_final <- cdm$outcome %>%
    PatientProfiles::addCohortName() %>%
    dplyr::collect() %>%
    PatientProfiles::summariseResult(
      group = list("cohort_name"), 
      includeOverallGroup = TRUE,
      minCellCount = 10,
      strata = list(c("sex"),c("age_gr"), c("sex", "age_gr" )), 
      includeOverallStrata = TRUE,
      variables = list(
        "categorical" = c("sex", "age_gr", "cohort_name"),
        "dates" = c("cohort_start_date", "cohort_end_date"),
        "numeric" = c("age", "future_observation")
      ),
      functions = list(
        "categorical" = c("count", "percentage"),
        "dates" = c("min", "q25", "median", "q75", "max"),
        "numeric" = c("min", "q25", "median", "q75", "max")
      )
    ) %>%
    dplyr::mutate(variable = ifelse(variable == "Age gr", "Age group", variable))
  
  tableone_final <- tableone_final %>% 
    dplyr::mutate(variable = ifelse(variable == "cohort_name", "outcome", variable),
                  cdm_name = db.name) %>% 
    relocate(cdm_name)
  
  
  print(paste0("Tableone ", Sys.time()," completed"))
  
  info(logger, "CREATED TABLE ONE SUMMARY")

}


print(paste0("Completed table one characterisations ", Sys.time()))
