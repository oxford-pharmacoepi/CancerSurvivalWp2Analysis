#creating table one for characterization of cancers

print(paste0("Starting table one characterisations ", Sys.time()))

# subset the CDM for analysis table to make code run quicker
info(logger, "SUBSETTING CDM")
cdm <- CDMConnector::cdmSubsetCohort(cdm, "outcome")
info(logger, "SUBSETTED CDM")


# depending on database type run different parts of the code for cancer registries
# we probably wont have access past conditions and medication history unless they are linked

if(priorhistory == TRUE){

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

info(logger, "CREATE TABLE ONE SUMMARY")

suppressWarnings(
tableone <- cdm$outcome %>%
  PatientProfiles::summariseCharacteristics(
    strata = list(c("sex"),c("age_gr"), c("sex", "age_gr" )),
    minCellCount = 10,
    ageGroup = list(c(18, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 150)),
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
      ),
      "Obesity" = list(
        targetCohortTable = "obesity", value = "flag", window = c(-Inf, 0)
      )
  
    )
  )
)

suppressWarnings(
  tableone_all_cancers <- cdm$outcome %>% 
    dplyr::mutate(cohort_definition_id = 10) %>% 
    PatientProfiles::summariseCharacteristics(
      strata = list(c("sex"),c("age_gr"), c("sex", "age_gr" )),
      minCellCount = 10,
      ageGroup = list(c(18, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 150)),
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
    dplyr::mutate(group_level = "All Cancers")
  
)


tableone_final <- dplyr::bind_rows(tableone, tableone_all_cancers) 

info(logger, "CREATED TABLE ONE SUMMARY")

} else {
  

info(logger, "CREATE TABLE ONE SUMMARY")

suppressWarnings(
  
tableone <- cdm$outcome %>%
  PatientProfiles::summariseCharacteristics(
    strata = list(c("sex"),c("age_gr"), c("sex", "age_gr" )),
    minCellCount = 10,
    ageGroup = list(c(18, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 150)),
cohortIntersect = list(
  "outcome" = list(
    targetCohortTable = "outcome", value = "flag", window = c(0, 0) )
)

)

)

suppressWarnings(
  
tableone_all_cancers <- cdm$outcome %>% 
          dplyr::mutate(cohort_definition_id = 10) %>% 
  PatientProfiles::summariseCharacteristics(
            strata = list(c("sex"),c("age_gr"), c("sex", "age_gr" )),
            minCellCount = 5,
            ageGroup = list(c(18, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 150)),
            cohortIntersect = list(
              "outcome" = list(
                targetCohortTable = "outcome", value = "flag", window = c(0, 0)  
              )
            )
            
          ) %>% 
          dplyr::mutate(group_level = "All Cancers")
        
      )
      

info(logger, "CREATED TABLE ONE SUMMARY")

tableone_final <- dplyr::bind_rows(tableone, tableone_all_cancers)

}


print(paste0("Completed table one characterisations ", Sys.time()))
