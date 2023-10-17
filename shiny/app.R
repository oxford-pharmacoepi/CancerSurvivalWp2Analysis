#### PACKAGES -----
options(encoding = "UTF-8")

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
# library(ggthemes)
library(plotly)
library(here)
library(scales)
library(dplyr)
library(stringr)
library(tidyr)
library(epitools)
library(ggalt)

# install.packages("remotes")
# remotes::install_github("paleolimbot/libproj")
# devtools::install_github("hrbrmstr/ggalt", ref = "noproj")


#### UI -----
ui <-  fluidPage(theme = shinytheme("spacelab"),
                 
# title ------ 
# shown across tabs
titlePanel("Incidence and prevalence of different cancers"),
               
# set up: pages along the side -----  
                 navlistPanel(
                   
                   
## Introduction  -----  
tabPanel("Background", 
  tags$h3("Background"),
     tags$hr(),
   tags$h4(tags$strong("Please note, the results presented here should be considered as 
                       preliminary and subject to change.")),
   tags$hr(),
   # tags$h5("This app is a companion to the study ...."),
  HTML('<br>'),
  tags$h4("Abstract"),
  tags$hr()
), 
## Patient profiles ------ 
tabPanel("Patient Profiles",	  
    tags$h3("Patient Profiles"),
    tags$h5("A summary of study participant characteristics are shown below, for the study population as
    a whole and for those with a specific outcome of interest. Select the study population used,
    the study period,
    whether a year of prior history was required for individuals, 
    and which populations are to be summarised."),
    tags$hr(),
    
      div(style="display: inline-block;vertical-align:top; width: 150px;",
          pickerInput(inputId = "CohortProfileDatabaseSelector",
  label = "Database",
  choices = unique(Network.patient.characteristcis$db),
  selected = unique(Network.patient.characteristcis$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)
          ),
  
           div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "CohortProfileStudyPopulationTypeSelector", 
  label = "Study population", 
  choices = unique(Network.patient.characteristcis$pop[which(Network.patient.characteristcis$pop!="Vaccinated")]),
  selected = unique(Network.patient.characteristcis$pop[which(Network.patient.characteristcis$pop!="Vaccinated")])[1],
  options = list(
    `actions-box` = TRUE, 
    size = 10,
    `selected-text-format` = "count > 3"), 
  multiple = TRUE)),
  
  div(style="display: inline-block;vertical-align:top; width: 250px;",
                pickerInput(inputId = "CohortProfileStudyPopulationSelector", 
  label = "Study period", 
  choices = unique(Network.patient.characteristcis$pop.type[which(!is.na(Network.patient.characteristcis$pop.type))]),
  selected = unique(Network.patient.characteristcis$pop.type[which(!is.na(Network.patient.characteristcis$pop.type))])[1],
  options = list(
    `actions-box` = TRUE, 
    size = 10,
    `selected-text-format` = "count > 3"), 
  multiple = FALSE)),
    
  div(style="display: inline-block;vertical-align:top; width: 250px;",
    pickerInput(inputId = "CohortProfilePriorHistorySelector", 
  label = "Year of prior history required", 
  choices = rev(unique(Network.patient.characteristcis$prior.obs.required)), 
  selected = rev(unique(Network.patient.characteristcis$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE, 
    size = 10,
    `selected-text-format` = "count > 3"), 
  multiple = FALSE)),
  
  div(style="display: inline-block;vertical-align:top; width: 150px;",
  pickerInput(inputId = "CohortProfileOutcomeSelector",
  label = "Cohort",
  choices = c("Study population", sort(names(Network.patient.characteristcis)[!names(Network.patient.characteristcis) %in% c("id","var","db","pop", "study.year", "end.year","prior.obs.required" , "pop.type","Study population", "age_gr2",
                                                                                                                             "From September 2020" , "Before September 2020" )])),
  selected = c("Study population", sort(names(Network.patient.characteristcis)[!names(Network.patient.characteristcis) %in% c("id","var","db","pop", "study.year", "end.year","prior.obs.required" , "pop.type","Study population", "age_gr2",
                                                                                                                              "From September 2020" , "Before September 2020" )]))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),
  
  tags$hr(),
  DTOutput('tbl')
                            ),
                   
                   
## Incidence ----
 tabPanel("Cumulative Incidence",

    tags$h3("Cumulative Incidence"),
    tags$h5("The cumulative incidence of events over time. Select the
            study populations,a specific study month,
            whether a year of prior history was required for individuals
            to be included into the study, the statification of interest, and
            the outcome of interest. "),
    tags$hr(),

     div(style="display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(inputId = "IncDbSelector",
  label = "Database",
  choices = unique(Network.Survival.summary$db),
  selected = unique(Network.Survival.summary$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

             div(style="display: inline-block;vertical-align:top; width: 150px;",
                pickerInput(inputId = "IncStudyPopulationTypeSelector",
  label = "Study population",
  choices = unique(Network.Survival.summary$pop),
  selected = unique(Network.Survival.summary$pop)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 250px;",
      pickerInput(inputId = "IncPopulationSelector",
  label = "Study period",
  choices = unique(Network.Survival.summary$pop.type),
  selected = unique(Network.Survival.summary$pop.type)[1],
    # c("Date anchored: 1st January 2017",
    #           "Visit anchored",
    #           "Visit anchored (28 days follow up)"),
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "IncStudyPopulationSelector",
  label = "Year of prior history required",
  choices = rev(unique(Network.Survival.summary$prior.obs.required)),
  selected = rev(unique(Network.Survival.summary$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IncStrataSelector",
  label = "Strata",
  choices =
  c("Overall",
              "Sex",
              "Age group (<=44, 45-64, >=65)",
              "Age group (<=44, 45-64, >=65) and sex",
              "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)",
              "Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex",
              "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)",
              "Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex",
    "Condition of interest",
    "Medication of interest",
    "Condition or medication of interest") ,
  selected = "Overall",
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IncOutcomeSelector",
  label = "Outcome",
  choices = sort(unique(Network.Survival.summary$outcome.name)) ,
  selected = sort(unique(Network.Survival.summary$outcome.name))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),


    div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "IncSurvTypeSelector",
  label = "Type",
  choices = unique(Network.Survival.summary$surv.type) ,
  selected = unique(Network.Survival.summary$surv.type)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),


    div(style="display: inline-block;vertical-align:top; width: 250px;",
      pickerInput(inputId = "IncDaysSelector",
  label = "Time points (for table)",
  choices = c(sort(unique(Network.Survival.summary$time))) ,
  selected = c(sort(unique(Network.Survival.summary$time)))[91],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),
   tags$hr(),
 withSpinner( plotlyOutput("cumulative.incidence.plot")),
  tags$hr(),
  DTOutput('inc.tbl'),
   tags$hr()

 ),

## Modelling Age ----
tabPanel("Relative hazard ratios for age",

    tags$h3("Relative hazard ratios for age, stratified by sex"),
    tags$h5("Hazard ratios for age (relative to a reference age of 65), from models stratified by sex.
            Select the database,
            the study population, the study period,
            whether a year of prior history was required for individuals
            to be included into the study, the outcome of interest, and the
            type of model. "),
    tags$hr(),

     div(style="display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(inputId = "RfAgeDbSelector",
  label = "Database",
  choices = unique(Network.Model.estimates$db),
  selected = unique(Network.Model.estimates$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),
  
  div(style="display: inline-block;vertical-align:top; width: 250px;",
      pickerInput(inputId = "RfAgeStudyCohortSelector",
  label = "Study cohort",
  choices = unique(Network.Model.estimates$working.study.cohort),
  selected = unique(Network.Model.estimates$working.study.cohort)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

div(style="display: inline-block;vertical-align:top; width: 250px;",
      pickerInput(inputId = "RfAgePopulationSelector",
  label = "Study period",
  choices = unique(Network.Model.estimates$pop.type),
  selected = unique(Network.Model.estimates$pop.type)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "RfAgeStudyPopulationSelector",
  label = "Year of prior history required",
  choices = rev(unique(Network.Model.estimates$prior.obs.required)),
  selected = rev(unique(Network.Model.estimates$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfAgeOutcomeSelector",
  label = "Outcome",
  choices = sort(unique(Network.Model.estimates$working.outcome.name)) ,
  selected = sort(unique(Network.Model.estimates$working.outcome.name))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfAgeModelTypeSelector",
  label = "Type",
  choices = unique(Network.Model.estimates$model.type) ,
  selected = unique(Network.Model.estimates$model.type)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),    


  plotlyOutput("Estimates.plot.age"),
tags$hr(),
downloadButton("downloadAge",
               label = "Download data")
 ),
## Modelling Sex ----
tabPanel("Hazard ratios for sex",

    tags$h3("Hazard ratios for sex"),
    tags$h5("Hazard ratios for sex (male relative female).
            Select the database,
            the study population, the study period,
            whether a year of prior history was required for individuals
            to be included into the study, the outcome of interest, the
            type of model, and the modelling adjustment."),
    tags$hr(),

     div(style="display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(inputId = "RfSexDbSelector",
  label = "Database",
  choices = unique(Network.Model.estimates$db),
  selected = unique(Network.Model.estimates$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),
  
  div(style="display: inline-block;vertical-align:top; width: 250px;",
      pickerInput(inputId = "RfSexStudyCohortSelector",
  label = "Study cohort",
  choices = unique(Network.Model.estimates$working.study.cohort),
  selected = unique(Network.Model.estimates$working.study.cohort)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

div(style="display: inline-block;vertical-align:top; width: 250px;",
      pickerInput(inputId = "RfSexPopulationSelector",
  label = "Study period",
  choices = unique(Network.Model.estimates$pop.type),
  selected = unique(Network.Model.estimates$pop.type)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "RfSexStudyPopulationSelector",
  label = "Year of prior history required",
  choices = rev(unique(Network.Model.estimates$prior.obs.required)),
  selected = rev(unique(Network.Model.estimates$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfSexOutcomeSelector",
  label = "Outcome",
  choices = sort(unique(Network.Model.estimates$working.outcome.name)) ,
  selected = sort(unique(Network.Model.estimates$working.outcome.name))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfSexModelTypeSelector",
  label = "Model adjustment",
  choices = unique(Network.Model.estimates$model.type) ,
  selected = unique(Network.Model.estimates$model.type)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfSexModelSelector",
  label = "Type",
  choices = unique(Network.Model.estimates$model[!is.na(Network.Model.estimates$model)]) ,
  selected = unique(Network.Model.estimates$model[!is.na(Network.Model.estimates$model)])[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  plotlyOutput("Estimates.plot.sex"),
tags$hr(),
downloadButton("downloadSex",
               label = "Download data")
 ),



          

          

## Modelling other ----
tabPanel("Hazard ratios for prior medications and comorbidities",

    tags$h3("Hazard ratios for prior medications and comorbidities"),
    tags$h5("Hazard ratios for prior medications and comorbidities.
            Select the database,
            the study population, the study period,
            whether a year of prior history was required for individuals
            to be included into the study, the outcome of interest, the
            type of model, and the modelling adjustment."),
    tags$hr(),

     div(style="display: inline-block;vertical-align:top; width: 150px;",
     pickerInput(inputId = "RfDbSelector",
  label = "Database",
  choices = unique(Network.Model.estimates$db),
  selected = unique(Network.Model.estimates$db)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),
  
  div(style="display: inline-block;vertical-align:top; width: 250px;",
      pickerInput(inputId = "RfStudyCohortSelector",
  label = "Study cohort",
  choices = unique(Network.Model.estimates$working.study.cohort),
  selected = unique(Network.Model.estimates$working.study.cohort)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

div(style="display: inline-block;vertical-align:top; width: 250px;",
      pickerInput(inputId = "RfPopulationSelector",
  label = "Study period",
  choices = unique(Network.Model.estimates$pop.type),
  selected = unique(Network.Model.estimates$pop.type)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 250px;",
        pickerInput(inputId = "RfStudyPopulationSelector",
  label = "Year of prior history required",
  choices = rev(unique(Network.Model.estimates$prior.obs.required)),
  selected = rev(unique(Network.Model.estimates$prior.obs.required))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfOutcomeSelector",
  label = "Outcome",
  choices = sort(unique(Network.Model.estimates$working.outcome.name)) ,
  selected = sort(unique(Network.Model.estimates$working.outcome.name))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),


  div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfExposureSelector",
  label = "Outcome",
  choices = sort(unique(Network.Model.estimates$var)) ,
  selected = sort(unique(Network.Model.estimates$var))[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = TRUE)),

    div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfModelTypeSelector",
  label = "Type",
  choices = unique(Network.Model.estimates$model.type) ,
  selected = unique(Network.Model.estimates$model.type)[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

    div(style="display: inline-block;vertical-align:top; width: 150px;",
      pickerInput(inputId = "RfModelSelector",
  label = "Type",
  choices = unique(Network.Model.estimates$model[!is.na(Network.Model.estimates$model)]) ,
  selected = unique(Network.Model.estimates$model[!is.na(Network.Model.estimates$model)])[1],
  options = list(
    `actions-box` = TRUE,
    size = 10,
    `selected-text-format` = "count > 3"),
  multiple = FALSE)),

  plotlyOutput("Estimates.plot.overall"),
tags$hr(),
downloadButton("downloadOther",
               label = "Download data")
 )



          

          
# close -----
                                                   ))

#### SERVER ------
server <-	function(input, output, session) {
  
get.patient.characteristcis<-reactive({
          # browser()
    table<-Network.patient.characteristcis %>% 
      filter(db %in% input$CohortProfileDatabaseSelector) %>% 
      filter(pop %in% input$CohortProfileStudyPopulationTypeSelector) %>% 
      filter(pop.type %in% input$CohortProfileStudyPopulationSelector |
             is.na(pop.type)) %>% 
      filter(prior.obs.required%in%input$CohortProfilePriorHistorySelector) %>% 
      select(-"id") %>% 
      mutate(dbpop=paste0("[",db, "] ",pop)) %>% 
      select(var, dbpop, input$CohortProfileOutcomeSelector)
  
  table<-table  %>%
  pivot_wider(names_from = dbpop,
             names_glue = "{dbpop}: {.value}",
              values_from = input$CohortProfileOutcomeSelector)
 
  
 table<-bind_rows(
     table[c(1:12),],
    table[c(1),]  %>%
      mutate_at(vars(names(table)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Comorbidities"),
     table[c(13:51),],
    table[c(1),]  %>%
      mutate_at(vars(names(table)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Medication use (183 days prior to four days prior)"),
    table[c(52:61),],
    table[c(1),]  %>%
      mutate_at(vars(names(table)), ~ replace(., !is.na(.), NA)) %>%
      mutate(var="Medication use (on index date)"),
    table[c(62:94),]
    )   %>%
     mutate(var=ifelse(var=="Copd", "COPD", var)) %>%
     mutate(var=ifelse(var=="Antiinflamatory and antirheumatic", "Non-steroidal anti-inflammatory drugs ", var)) %>%
     mutate(var=ifelse(var=="Coxibs", "Cox2 inhibitors ", var)) %>%
     mutate(var=ifelse(var=="Corticosteroids", "Systemic corticosteroids ", var)) %>%
     mutate(var=ifelse(var=="Antithrombotic", "Antithrombotic and anticoagulant therapies", var)) %>%
     mutate(var=ifelse(var=="Lipid modifying", "Lipid modifying agents ", var)) %>%
     mutate(var=ifelse(var=="Antineoplastic immunomodulating", "Antineoplastic and immunomodulating agents ", var)) %>%
     mutate(var=ifelse(var=="Hormonal contraceptives", "Hormonal contraceptives for systemic use ", var)) %>%
     mutate(var=ifelse(var=="Sex hormones modulators", "Sex hormones and modulators of the genital system", var))%>%
     mutate(var=ifelse(var=="Age.under.20", "Age: Under 20", var))  %>%
     mutate(var=ifelse(var=="Age.20 29", "Age: 20 to 29", var))  %>%
     mutate(var=ifelse(var=="Age.30 39", "Age: 30 to 39", var))  %>%
     mutate(var=ifelse(var=="Age.40 49", "Age: 40 to 49", var))  %>%
     mutate(var=ifelse(var=="Age.50 59", "Age: 50 to 59", var))  %>%
     mutate(var=ifelse(var=="Age.60 69", "Age: 60 to 69", var))  %>%
     mutate(var=ifelse(var=="Age.70 79", "Age: 70 to 79", var))  %>%
     mutate(var=ifelse(var=="Age.80u", "Age: 80 or older", var))  %>%
     mutate(var=str_replace(var, ".all.history", " (Over all prior history)")) %>%
     mutate(var=str_replace(var, ".one.year", " (Over prior year)")) %>%
     mutate(var=str_replace(var, ".30.days", " (Over prior 30 days)")) %>%
     mutate(var=str_replace(var, ".on.index: new user", ": new user"))%>%
     mutate(var=str_replace(var, ".on.index: prevalent user", ": prevalent user"))%>%
     mutate(var=str_replace(var, ".on.index: non-user", ": non-user"))%>%
     mutate(var=str_replace(var, "Copd", "COPD"))
 
 
 
table<-apply(table, 2,
                     function(x)
                       ifelse(str_sub(x, 1, 8) %in%  c("0 (0.0%)"),
             "<5",x))
table<-data.frame(table,check.names = FALSE)

   # browser()
  table  <-table[,!is.na(table[1,]), drop = F]
  table  <-table[,(table[1,])!="0", drop = F]
  table  <-table[,(table[1,])!="1", drop = F]
  table  <-table[,(table[1,])!="2", drop = F]
  table  <-table[,(table[1,])!="3", drop = F]
  table  <-table[,(table[1,])!="4", drop = F]
  # table  <-table[,(table[1,])!="5", drop = F]
  


    table
  }) 
 
output$tbl<-  renderDataTable({
  validate(need(length(input$CohortProfileDatabaseSelector)>0,
                "No results for selected inputs"))
  table<-get.patient.characteristcis() 
  table<-table%>% select_if(~!is.na(.[1L]))
  table<-table[,  which(!table[1,] %in% c("1","2", "3", "4", "5"))]
 
  
  validate(need(ncol(table)>1, 
                "No results for selected inputs"))
  
   datatable(table
    ,rownames= FALSE,
    colnames = c('Variable' = 1),
     extensions = 'Buttons',
    # options = list(lengthChange = FALSE,
    #                pageLength = 40,
    #                buttons = c('copy', 'csv', 'excel')))
    options = list(lengthChange = FALSE,
                    pageLength = 94,
                                dom = 'tB',
                               buttons = list(list(extend = "excel", 
                                             text = "Download table as excel",
                                             filename = "PatientProfiles.csv"))
                            ))
    } )
 
get.inc.summary<-reactive({
    
  table<-Network.Survival.summary %>% 
  filter(db %in% input$IncDbSelector)  %>% 
  filter(surv.type %in% input$IncSurvTypeSelector)  %>% 
  filter(pop %in% input$IncStudyPopulationTypeSelector) %>% 
  filter(pop.type %in% input$IncPopulationSelector) %>% 
  filter(prior.obs.required%in% input$IncStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IncOutcomeSelector)
  
  if(input$IncDaysSelector!="All"){
  table<-table %>% 
   filter(time %in%  input$IncDaysSelector)
  }
  
  
 
  if(input$IncStrataSelector=="Overall"){
 table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("overall")) %>%  
   select(db, pop, time, "Number at risk", "Events","Cumulative incidence") 
 }
  
      if(input$IncStrataSelector=="Sex"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("gender")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
      }
  
        if(input$IncStrataSelector=="Condition of interest"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("cond.comp")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
        }
  
          if(input$IncStrataSelector=="Medication of interest"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("drug.comp")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
          if(input$IncStrataSelector=="Condition or medication of interest"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("cond.drug.comp")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
  
    if(input$IncStrataSelector=="Age group (<=44, 45-64, >=65) and sex"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr2_gender")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
      if(input$IncStrataSelector=="Age group (<=44, 45-64, >=65)"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr2")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
      if(input$IncStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr_gender")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
      }
  
        if(input$IncStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  
  
      if(input$IncStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr3_gender")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
      }
  
        if(input$IncStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)"){
     table<-table %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr3")) %>%  
   select(db, pop, time, Strata, "Number at risk", "Events","Cumulative incidence") 
    }
  

  table
  })
  
output$inc.tbl<- renderDataTable({
      table<-get.inc.summary() 

 
  
   datatable(table
    ,rownames= FALSE,
    colnames = c("Database"=1,
                 'Study population' = 2,
                 'Days since index date' = 3),
     extensions = 'Buttons',
    # options = list(lengthChange = FALSE,
    #                pageLength = 40,
    #                buttons = c('copy', 'csv', 'excel')))
    options = list(lengthChange = FALSE,
                    pageLength = 40,
                                dom = 'tB',
                               buttons = list(list(extend = "excel", 
                                             text = "Download table as excel",
                                             filename = "Inc.csv"))
                            ))
    
  } )

output$cumulative.incidence.plot<- renderPlotly({

    
plot.data<-Network.Survival.summary %>% 
  filter(db %in% input$IncDbSelector)  %>% 
  filter(surv.type %in% input$IncSurvTypeSelector)  %>% 
  filter(pop %in% input$IncStudyPopulationTypeSelector) %>% 
  filter(pop.type %in% input$IncPopulationSelector) %>% 
  filter(prior.obs.required%in% input$IncStudyPopulationSelector)%>% 
  filter(outcome.name %in%  input$IncOutcomeSelector)

 
  if(input$IncStrataSelector=="Overall"){
 plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("overall"))
 }
  
      if(input$IncStrataSelector=="Sex"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("gender"))
      }
  
        if(input$IncStrataSelector=="Condition of interest"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("cond.comp")) 
        }
  
          if(input$IncStrataSelector=="Medication of interest"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("drug.comp"))
    }
  
          if(input$IncStrataSelector=="Condition or medication of interest"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("cond.drug.comp"))
    }
  
    if(input$IncStrataSelector=="Age group (<=44, 45-64, >=65) and sex"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr2_gender")) 
    }
  
      if(input$IncStrataSelector=="Age group (<=44, 45-64, >=65)"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr2")) 
    }
  
      if(input$IncStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85) and sex"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr_gender")) 
      }
  
        if(input$IncStrataSelector=="Age group (30-44, 45-54, 55-64, 65-74, 75-84, >=85)"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr")) 
    }
  
  
      if(input$IncStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80) and sex"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr3_gender")) 
      }
  
        if(input$IncStrataSelector=="Age group (30-39, 40-49, 50-59, 60-69, 70-79, >=80)"){
     plot.data<-plot.data %>% 
   ungroup() %>% 
  filter(strata %in% c("age_gr3")) 
    }

validate(
  need(nrow(plot.data)>0, "No results for selected inputs")
)
 # browser()

if(input$IncStrataSelector=="Overall"){
  plot<-  plot.data %>%
  ggplot()+
  geom_step(aes(x=time, surv)) +
  geom_step(aes(x=time, y=lower), linetype=2)+
  geom_step(aes(x=time, y=upper), linetype=2)+
  geom_ribbon(aes(x=time,
                  ymin = lower,
                  ymax = upper),
              stat="stepribbon",
                alpha = .2)+
 xlab("Days since index date")+
    ylab("Cumulative incidence")+
    scale_y_continuous(labels = scales::percent)
} else {

    plot<-  plot.data %>%
  ggplot()+
  geom_step(aes(x=time, surv, colour=Strata)) +
  geom_step(aes(x=time, y=lower, colour=Strata), linetype=2)+
  geom_step(aes(x=time, y=upper, colour=Strata), linetype=2)+
  geom_ribbon(aes(x=time,
                  ymin = lower,
                  ymax = upper, fill=Strata),
              stat="stepribbon",
                alpha = .2)+
 xlab("Days since index date")+
    ylab("Cumulative incidence")+
    scale_y_continuous(labels = scales::percent)


}


# browser()
plot<-  ggplotly(plot +    
  theme_bw()+
  # scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
  #                                "Vaccinated with BNT162b2" = "#377eb8",
  #                              "General population (index date: 1st December)" = "#4daf4a",
  #                              "General population (index date: first visit/ contact)" = "#984ea3",
  #                              "SARS-CoV-2 PCR positive test"="#ff7f00"))+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12)) , 
  tooltip = "text")%>%
  config(displaylogo = FALSE)%>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                    "pan2d", "zoom2d",
                                    "select2d",
                                    "lasso2d",
                                    "drawclosedpath",
                                    "drawopenpath",
                                    "drawrect",
                                    "drawcircle",
                                    "eraseshape",
                                    "autoScale2d",
                                    "resetScale2d",
                                    "hoverClosestCartesian",
                                    "hoverCompareCartesian",
                                    "hoverClosestGeo",
                                    "hoverClosestGl2d",
                                    "toggleSpikelines")) 
  # %>% 
  #   layout(legend = list(orientation = "h", y=-2, itemsizing='constant'))

for (i in 1:length(plot$x$data)){
    if (!is.null(plot$x$data[[i]]$name)){
        plot$x$data[[i]]$name =  gsub("\\(","",str_split(plot$x$data[[i]]$name,",")[[1]][1])
    }}

plot
    })

# age    
get.age.est.plot.data<- reactive({
 
 Network.Model.estimates  %>% 
   filter(!is.na(rel.age)) %>% 
  # filter(time.window%in% input$RfAgeTimeWindowSelector|
  #            is.na(time.window)) %>%  
  # filter(pop %in% input$RfAgeStudyPopulationTypeSelector) %>% 
  filter(pop.type %in% input$RfAgePopulationSelector) %>% 
  filter(prior.obs.required%in% input$RfAgeStudyPopulationSelector)%>% 
  filter(working.outcome.name %in%  input$RfAgeOutcomeSelector)%>% 
  filter(db %in%  input$RfAgeDbSelector) %>% 
  filter(model.type %in% input$RfAgeModelTypeSelector) %>% 
  filter(working.study.cohort %in% input$RfAgeStudyCohortSelector)

    
  }) 

output$Estimates.plot.age<- renderPlotly({
  
#   validate(
#   need(input$IRsDatabaseSelector != "", "Please select a database")
# )
  
working.data<-get.age.est.plot.data() 
# 
validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)


 
 
 plot<-  working.data %>%
    filter(rel.age<=90) %>% 
  ggplot()+
  geom_line(aes(rel.age, hr, colour=gender))+
  geom_line(aes(rel.age, hr.low, colour=gender), linetype="dashed")+
  geom_line(aes(rel.age, hr.high, colour=gender), linetype="dashed")+
  ylab("Relative\nhazard ratio\n")+
  xlab("Age")+
  scale_y_continuous(position = "right")+
  scale_colour_manual(values=c("#b2182b","#2166ac"))+
  theme_bw()+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=16),
        axis.title=element_text(size=16,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"), 
        legend.text=element_text(size=16),
        legend.position = "top")




# browser()
  ggplotly(plot , 
  tooltip = "text")%>%
  config(displaylogo = FALSE)%>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                    "pan2d", "zoom2d",
                                    "select2d",
                                    "lasso2d",
                                    "drawclosedpath",
                                    "drawopenpath",
                                    "drawrect",
                                    "drawcircle",
                                    "eraseshape",
                                    "autoScale2d",
                                    "resetScale2d",
                                    "hoverClosestCartesian",
                                    "hoverCompareCartesian",
                                    "hoverClosestGeo",
                                    "hoverClosestGl2d",
                                    "toggleSpikelines")) 
  # %>% 
  #   layout(legend = list(orientation = "h", y=-2, itemsizing='constant'))

    })

output$downloadAge <- downloadHandler(
  filename = function() {
    paste0("AgeHRs", ".csv")
  },
  content = function(file) {
    write.csv( Network.Model.estimates  %>% 
   filter(!is.na(rel.age))%>% 
      select(-working.outcome) %>% 
      select(-var) %>% 
      select(-model), file) 
  }
)
# sex    
get.sex.est.plot.data<- reactive({
 Network.Model.estimates  %>% 
   filter(is.na(rel.age)) %>% 
  # filter(time.window%in% input$RfAgeTimeWindowSelector|
  #            is.na(time.window)) %>%  
  # filter(pop %in% input$RfAgeStudyPopulationTypeSelector) %>% 
  filter(pop.type %in% input$RfSexPopulationSelector) %>% 
  filter(prior.obs.required%in% input$RfSexStudyPopulationSelector)%>% 
  filter(working.outcome.name %in%  input$RfSexOutcomeSelector)%>% 
  filter(db %in%  input$RfSexDbSelector) %>% 
  filter(model.type %in% input$RfSexModelTypeSelector) %>% 
  filter(working.study.cohort %in% input$RfSexStudyCohortSelector) %>% 
  filter(model  %in% input$RfSexModelSelector) %>% 
  filter(var=="Sex (male:female)")
    
  }) 

output$Estimates.plot.sex<- renderPlotly({
  
#   validate(
#   need(input$IRsDatabaseSelector != "", "Please select a database")
# )
  
working.data<-get.sex.est.plot.data() 
# 
validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
# browser()

 
 
 plot<-  working.data %>%
  ggplot()+
  geom_point(aes(db, hr))+
  geom_errorbar(aes(db, ymin=hr.low, ymax=hr.high), width=0)+
    xlab("")+
    ylab("Hazard ratio")+
   coord_flip()+
     ylim(c(0,NA))+
    geom_hline(yintercept = 1, colour="grey")



# browser()
  ggplotly(plot +    
  theme_bw()+
  # scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
  #                                "Vaccinated with BNT162b2" = "#377eb8",
  #                              "General population (index date: 1st December)" = "#4daf4a",
  #                              "General population (index date: first visit/ contact)" = "#984ea3",
  #                              "SARS-CoV-2 PCR positive test"="#ff7f00"))+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12)) , 
  tooltip = "text")%>%
  config(displaylogo = FALSE)%>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                    "pan2d", "zoom2d",
                                    "select2d",
                                    "lasso2d",
                                    "drawclosedpath",
                                    "drawopenpath",
                                    "drawrect",
                                    "drawcircle",
                                    "eraseshape",
                                    "autoScale2d",
                                    "resetScale2d",
                                    "hoverClosestCartesian",
                                    "hoverCompareCartesian",
                                    "hoverClosestGeo",
                                    "hoverClosestGl2d",
                                    "toggleSpikelines")) 
  # %>% 
  #   layout(legend = list(orientation = "h", y=-2, itemsizing='constant'))

    })

output$downloadSex <- downloadHandler(
  filename = function() {
    paste0("SexHRs", ".csv")
  },
  content = function(file) {
    write.csv( Network.Model.estimates  %>% 
   filter(is.na(rel.age))%>% 
  filter(var=="Sex (Male:Female)") %>% 
      select(-working.outcome) %>% 
      select(-gender) , file)
  }
)

# cmorbs and meds
get.est.plot.data<- reactive({

 Network.Model.estimates  %>% 
   filter(is.na(rel.age)) %>%  
  filter(pop.type %in% input$RfPopulationSelector) %>% 
  filter(prior.obs.required%in% input$RfStudyPopulationSelector)%>% 
  filter(working.outcome.name %in%  input$RfOutcomeSelector)%>% 
  filter(db %in%  input$RfDbSelector) %>% 
  filter(model.type %in% input$RfModelTypeSelector) %>% 
  filter(working.study.cohort %in% input$RfStudyCohortSelector)  %>% 
  filter(var %in% input$RfExposureSelector)  %>% 
  filter(model %in% input$RfModelSelector) 
    
  }) 

output$Estimates.plot.overall<- renderPlotly({
 working.data<-get.est.plot.data() 
validate(
  need(nrow(working.data)>0, "No results for selected inputs")
)
 # browser()

  plot<-  working.data %>%
  ggplot()+
  geom_point(aes(var, hr))+
  geom_errorbar(aes(var, ymin=hr.low, ymax=hr.high), width=0)+
    xlab("")+
    ylab("Hazard ratio")+
   coord_flip()+
     ylim(c(0,NA))+
    geom_hline(yintercept = 1, colour="grey")


# browser()
  ggplotly(plot +    
  theme_bw()+
  # scale_color_manual(values=c("Vaccinated with ChAdOx1" = "#e41a1c",
  #                                "Vaccinated with BNT162b2" = "#377eb8",
  #                              "General population (index date: 1st December)" = "#4daf4a",
  #                              "General population (index date: first visit/ contact)" = "#984ea3",
  #                              "SARS-CoV-2 PCR positive test"="#ff7f00"))+
  theme(legend.title = element_blank(),
        axis.text=element_text(size=12),
        axis.title=element_text(size=12,face="bold"),
        strip.text = element_text(size=16, face="bold"),
        strip.text.y.left = element_text(angle = 0),
        strip.background = element_rect( fill="#f7f7f7"),
      #  axis.title.y.right =  element_text(angle = 0),
        legend.text=element_text(size=12)) , 
  tooltip = "text")%>%
  config(displaylogo = FALSE)%>%
  config(modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d",
                                    "pan2d", "zoom2d",
                                    "select2d",
                                    "lasso2d",
                                    "drawclosedpath",
                                    "drawopenpath",
                                    "drawrect",
                                    "drawcircle",
                                    "eraseshape",
                                    "autoScale2d",
                                    "resetScale2d",
                                    "hoverClosestCartesian",
                                    "hoverCompareCartesian",
                                    "hoverClosestGeo",
                                    "hoverClosestGl2d",
                                    "toggleSpikelines")) 
  # %>% 
  #   layout(legend = list(orientation = "h", y=-2, itemsizing='constant'))

    })

output$downloadOther <- downloadHandler(
  filename = function() {
    paste0("ComrbidityMeicationHRs", ".csv")
  },
  content = function(file) {
    write.csv( Network.Model.estimates  %>% 
   filter(is.na(rel.age))%>% 
  filter(var!="Sex (Male:Female)") %>% 
      select(-working.outcome) %>% 
      select(-gender) , file)
  }
)
  

 }
  


#### RUN APP ----
shinyApp(ui = ui, server = server)


