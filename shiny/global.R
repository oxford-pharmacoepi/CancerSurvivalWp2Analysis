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
# extract each element from the list
list2env(study_results,globalenv())


prevalence_estimates <- readRDS(here("data","prevalence_estimates.rds"))
prevalence_attrition <- readRDS(here("data","prevalence_attrition.rds"))
incidence_estimates <- readRDS(here("data","incidence_estimates.rds"))
incidence_attrition <- readRDS(here("data","incidence_attrition.rds"))
# whole pop
survival_estimates_whole <- readRDS(here("data","survival_estimates.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2019" | CalendarYearGp == "2000 to 2021" ) %>%
  droplevels() %>% 
  rename(Sex = Gender)

survival_risk_table <- readRDS(here("data","survival_risk_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp)

survival_median_table <- readRDS(here("data","survival_median_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2019" | CalendarYearGp == "2000 to 2021" ) %>%
  droplevels()

survival_rates_table <- readRDS(here("data","survival_rates_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp == "2000 to 2019" | CalendarYearGp == "2000 to 2021" ) %>%
  droplevels() 

#calendar pop
survival_estimates_calendar <- readRDS(here("data","survival_estimates.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp != "2000 to 2019") %>% 
  filter(CalendarYearGp != "2000 to 2021" ) %>%
  droplevels() %>% 
  rename(Sex = Gender)

survival_risk_table_cy <- readRDS(here("data","survival_risk_table_cy.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp)

survival_median_table_cy <- readRDS(here("data","survival_median_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp != "2000 to 2019") %>% 
  filter(CalendarYearGp != "2000 to 2021" ) %>%
  droplevels()

survival_rates_table_cy <- readRDS(here("data","survival_rates_table.rds")) %>%
  rename(CalendarYearGp = CalenderYearGp) %>%
  filter(CalendarYearGp != "2000 to 2019") %>% 
  filter(CalendarYearGp != "2000 to 2021" ) %>%
  droplevels() %>%
  filter(time != 10) %>%
  filter( !(time == 5 & CalendarYearGp == "2020 to 2021" )) %>% 
  mutate(time = as.character(time)) %>% 
  rename(Sex = Gender)

table_one_results <- readRDS(here("data","table1_results.rds")) %>%
  filter(analysis == "Incidence") %>% 
  filter(!grepl("Prior_history_years",var)) 
  
survival_followup_table <- readRDS(here("data","survival_median_mean_follow_up.rds")) 



