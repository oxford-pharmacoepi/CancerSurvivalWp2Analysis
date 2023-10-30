library(here)
library(dplyr)
library(tidyr)
library(stringr)

# printing numbers with 1 decimal place and commas 
nice.num<-function(x) {
  trimws(format(round(x,1),
                big.mark=",", nsmall = 1, digits=1, scientific=FALSE))}
# printing numbers with 2 decimal place and commas 
nice.num2<-function(x) {
  trimws(format(round(x,2),
                big.mark=",", nsmall = 2, digits=2, scientific=FALSE))}
# printing numbers with 3 decimal place and commas 
nice.num3<-function(x) {
  trimws(format(round(x,3),
                big.mark=",", nsmall = 3, digits=3, scientific=FALSE))}
# for counts- without decimal place
nice.num.count<-function(x) {
  trimws(format(x,
                big.mark=",", nsmall = 0, digits=1, scientific=FALSE))}



#### Load and extract data -----
#data
study_results <- readRDS(here("data","Results.rds"))
# extract each element from the list to put results into r environment
list2env(study_results,globalenv())

# filter results for just km results
survival_km <- survival_estimates %>% 
  filter(Method == "Kaplan-Meier")
surv_prob_km <- survival_probabilities %>% 
  filter(Method == "Kaplan-Meier",
         Adjustment == "None") %>% 
  select(!c(Adjustment))
med_surv_km <- median_survival_results %>% 
  filter(Method == "Kaplan-Meier",
         Adjustment == "None") %>% 
  select(!c(Adjustment, `RMST time`))
hot_km <- hazard_overtime_results %>% 
  filter(Method == "Kaplan-Meier")

