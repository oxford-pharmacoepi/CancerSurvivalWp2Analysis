#creating table one for characterization of cancers

# subset the CDM for analysis table to make code run quicker
info(logger, "SUBSETTING CDM")
cdm <- cdmSubsetCohort(cdm, "analysis")
info(logger, "SUBSETTED CDM")
  
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
tableone <- cdm$analysis %>%
  summariseCharacteristics(
    strata = list(c(18, 29), c(30, 39), c(40, 49), c(50, 59), c(60, 69), c(70, 79), c(80, 89), c(90, 150)),
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

tableone_all_cancers <- cdm$analysis %>% 
  mutate(cohort_definition_id = 10) %>% 
  summariseCharacteristics(
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

tableone <- bind_rows(tableone, tableone_all_cancers) %>% 
  select(!c(result_type, group_name, strata_name, strata_level))