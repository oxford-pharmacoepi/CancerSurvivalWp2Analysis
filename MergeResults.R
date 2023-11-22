# Functions for merging results from multiple databases for results shiny

#save as elements of a list
final_results <- list(
  survivalResults,
  riskTableResults, 
  medianResults,
  hazOverTimeResults,
  GOFResults,
  ExtrpolationParameters,
  AnalysisRunSummary,
  tableone_final,
  snapshotcdm,
  attritioncdm
  
)


names(final_results) <- c(  "survivalResults",
                            "riskTableResults", 
                            "medianResults",
                            "hazOverTimeResults",
                            "GOFResults",
                            "ExtrpolationParameters",
                            "AnalysisRunSummary",
                            "tableone_final",
                            "snapshotcdm",
                            "attritioncdm"
                          
                          )

saveRDS(c(final_results),
     file = paste0(here::here(output.folder),"/", "Results.RDS"))



# #TBC
# 
# data <- Pop %>%
#   filter(cohort_definition_id == j)
# 
# #carry out km estimate ---
# #take every other row from results
# asd <- survfit (Surv(time_years, status) ~ 1, data=data) 
# 
# plot(asd, fun = "cloglog", xlab = "log (years)",
#      ylab = "log-log survival") 
# 
# asd <- survfit (Surv(time_years, status) ~ 1, data=data) %>% 
#   ggsurvplot(data = data, fun="cloglog", 
#              title = paste0("log-log plot age: ", outcome_cohorts$cohort_name[j])) + labs(x = "log(years)")
# 
# 
# cloglog_vals <- 1 - exp(-exp(x))
# 
# # cloglogvalues match survimer package 
# 
# asdf <- survfit (Surv(time_years, status) ~ 1, data=data) %>%
#   tidy() %>%
#   mutate(Method = "Kaplan-Meier", Cancer = outcome_cohorts$cohort_name[j], Age = "All", Sex = "Both") %>% 
#   mutate(cloglogvals = log(-log(estimate)) ) %>% 
#   mutate(logtime = log(time))
# 
# #using values from dataframe 
# plot(x = asdf$time , y = asdf$cloglogvals,  xlab = "log (years)",
#      ylab = "log-log survival", log='x')
# 
# #from using cloglog directly now both plots are identical
# plot(asd, fun = "cloglog", xlab = "log (years)",
#      ylab = "log-log survival") 




