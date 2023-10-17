#### SERVER ------
server <-	function(input, output, session) {
  # prevalence
  get_prevalence_estimates<-reactive({
    
    table<-prevalence_estimates %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                denominator_days_prior_history,
                analysis_complete_database_intervals,
                denominator_start_date,
                result_id,
                denominator_end_date,
                analysis_type, analysis_full_contribution)) %>% 
      filter(database_name %in% input$prevalence_database_name_selector)  %>% 
      filter(as.character(prevalence_start_date) %in% input$prevalence_start_date_selector)  %>% 
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector)     %>% 
      #filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector)   %>% 
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector)  

    table
  }) 
  output$tbl_prevalence_estimates<-  DT::renderDataTable({

    table<-get_prevalence_estimates() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(prevalence=paste0(nice.num3(prevalence*100), "%")) %>% 
      mutate(prevalence_95CI_lower=paste0(nice.num3(prevalence_95CI_lower*100), "%")) %>% 
      mutate(prevalence_95CI_upper=paste0(nice.num3(prevalence_95CI_upper*100), "%")) %>% 
      mutate(prevalence= ifelse(!is.na(prevalence),
               paste0(prevalence, " (",
                                prevalence_95CI_lower," to ", 
                                prevalence_95CI_upper, ")"))) %>% 
      select(!c("prevalence_95CI_lower", "prevalence_95CI_upper",
                "cohort_obscured", "result_obscured",
                "analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id","prevalence",
                "denominator_strata_cohort_definition_id",
                "denominator_strata_cohort_name")) %>%
     # relocate(result_id, .after = prevalence_end_date) %>%
    relocate(`Prevalence (95% CI)`, .before = outcome_cohort_name) %>% 
      rename(`Start Date` = prevalence_start_date,
             `End Date` = prevalence_end_date,
             `Population (n)` = n_population,
             `Cases (n)` = n_cases,
             Cancer = outcome_cohort_name,
             Age = denominator_age_group,
             Sex = denominator_sex,
             Database = database_name)

    
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "prevalence_estimates"))
              ))
  } )
  output$plot_prevalence_estimates<- renderPlotly({ 
    
    table<-get_prevalence_estimates() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))

    if(is.null(input$prevalence_plot_group)){
      if(!is.null(input$prevalence_plot_facet)){
      p<-table %>% 
        unite("facet_var", 
              c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>% 
        ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                          ymin = "prevalence_95CI_lower",
                          ymax = "prevalence_95CI_upper")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width=0) +
        facet_wrap(vars(facet_var),ncol = 2)+
        scale_y_continuous(
          labels = scales::percent,
          limits = c(0, NA)
        ) +
        theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$prevalence_plot_group) ){ 
      
      if(is.null(input$prevalence_plot_facet) ){ 
      p<-table %>% 
        unite("Group", 
              c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>% 
        ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                          ymin = "prevalence_95CI_lower",
                          ymax = "prevalence_95CI_upper",
                          group="Group",
                          colour="Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width=0, position=position_dodge(width=1)) +
        theme_bw()
      }
      
      if(!is.null(input$prevalence_plot_facet) ){
        if(!is.null(input$prevalence_plot_group) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$prevalence_plot_group)), remove = FALSE, sep = "; ") %>% 
          unite("facet_var", 
                c(all_of(input$prevalence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$prevalence_x_axis, y="prevalence",
                            ymin = "prevalence_95CI_lower",
                            ymax = "prevalence_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          facet_wrap(vars(facet_var),ncol = 2)+  
          scale_y_continuous(
            labels = scales::percent,
            limits = c(0, NA)
          )  +
          theme_bw()
        }
      }
      
    }
    
    p
    
    })
  get_prevalence_attrition<-reactive({
    
    table<-prevalence_attrition %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, analysis_outcome_lookback_days,
                analysis_interval,
                analysis_time_point,
                result_id,
                denominator_strata_cohort_definition_id,
                denominator_days_prior_history,
                denominator_strata_cohort_name,
                analysis_complete_database_intervals,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$prevalence_database_name_selector)    %>% 
      filter(denominator_age_group %in% input$prevalence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$prevalence_denominator_sex_selector)     %>% 
      #filter(denominator_days_prior_history %in% input$prevalence_denominator_days_prior_history_selector)     %>% 
      filter(outcome_cohort_name %in% input$prevalence_outcome_cohort_name_selector)  
    
    table
  }) 
  output$tbl_prevalence_attrition<-  DT::renderDataTable({
    
    table<-get_prevalence_attrition() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_full_contribution",
                "analysis_min_cell_count",
                "denominator_cohort_id", 
                "outcome_cohort_id")) %>% 
      rename(n = current_n,
             `Excluded (n)`= excluded,
             Reason = reason,
             Cancer = outcome_cohort_name,
             Age = denominator_age_group,
             Sex = denominator_sex,
             Database = database_name,
             `Analysis Type` = analysis_type,
             Step = step )
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "prevalence_attrition"))
              ))
  })
 
  # incidence
  get_incidence_estimates<-reactive({
    
    table<-incidence_estimates %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                analysis_complete_database_intervals,
                denominator_start_date,
                result_id,
                denominator_days_prior_history,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector)  %>% 
      filter(as.character(incidence_start_date) %in% input$incidence_start_date_selector)  %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>% 
      #filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector)   %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector)  %>%
      filter(analysis_interval %in% input$incidence_denominator_analysis_interval_selector) 

    table
  }) 
  output$tbl_incidence_estimates<-  DT::renderDataTable({

    table<-get_incidence_estimates() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      mutate(incidence_100000_pys=nice.num2(incidence_100000_pys)) %>% 
      mutate(incidence_100000_pys_95CI_lower=nice.num2(incidence_100000_pys_95CI_lower)) %>% 
      mutate(incidence_100000_pys_95CI_upper=nice.num2(incidence_100000_pys_95CI_upper)) %>% 
      mutate(incidence_100000_pys= ifelse(!is.na(incidence_100000_pys),
               paste0(incidence_100000_pys, " (",
                                incidence_100000_pys_95CI_lower," to ", 
                                incidence_100000_pys_95CI_upper, ")"))) %>% 
      select(!c("incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper",
                "cohort_obscured", "result_obscured","person_days",
                "analysis_min_cell_count","analysis_repeated_events",
                "analysis_outcome_washout",
                "denominator_cohort_id", "outcome_cohort_id",
                "denominator_strata_cohort_definition_id",
                "denominator_strata_cohort_name")) %>% 
      mutate(n_persons=nice.num.count(n_persons)) %>% 
      mutate(n_events=nice.num.count(n_events)) %>% 
      mutate(person_years=nice.num.count(person_years)) %>% 
      relocate(incidence_start_date) %>% 
      relocate(incidence_end_date, .after = incidence_start_date) %>% 
      relocate(person_years, .after = n_persons) %>% 
    rename(`Start Date` = incidence_start_date,
           `End Date` = incidence_end_date,
           `Persons (n)` = n_persons,
           `Person Years`= person_years,
           `Events (n)` = n_events,
           `Incidence (100000 pys)` = incidence_100000_pys,
           Cancer = outcome_cohort_name,
           `Time Interval` = analysis_interval,
           Age = denominator_age_group,
           Sex = denominator_sex,
           Database = database_name)
    
    
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "incidence_estimates"))
              ))
  } )
  output$plot_incidence_estimates<- renderPlotly({ 
    
    table<-get_incidence_estimates() 
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))

    if(is.null(input$incidence_plot_group)){
      if(!is.null(input$incidence_plot_facet)){
      p<-table %>% 
        unite("facet_var", 
              c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
        ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width=0) +
        facet_wrap(vars(facet_var),ncol = 2)+
        scale_y_continuous(
          limits = c(0, NA)
        ) +
        theme_bw()
      } else{
        p<-table %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()        
      }
    } 
    
    
    if(!is.null(input$incidence_plot_group) ){ 
      
      if(is.null(input$incidence_plot_facet) ){ 
      p<-table %>% 
        unite("Group", 
              c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
        ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                          ymin = "incidence_100000_pys_95CI_lower",
                          ymax = "incidence_100000_pys_95CI_upper",
                          group="Group",
                          colour="Group")) +
        geom_point(position=position_dodge(width=1))+
        geom_errorbar(width=0, position=position_dodge(width=1)) +
        theme_bw()
      }
      
      if(!is.null(input$incidence_plot_facet) ){
        if(!is.null(input$incidence_plot_group) ){ 
        p<-table %>% 
          unite("Group", 
                c(all_of(input$incidence_plot_group)), remove = FALSE, sep = "; ") %>% 
          unite("facet_var", 
                c(all_of(input$incidence_plot_facet)), remove = FALSE, sep = "; ") %>% 
          ggplot(aes_string(x=input$incidence_x_axis, y="incidence_100000_pys",
                            ymin = "incidence_100000_pys_95CI_lower",
                            ymax = "incidence_100000_pys_95CI_upper",
                            group="Group",
                            colour="Group")) +
          geom_point(position=position_dodge(width=1))+
          geom_errorbar(width=0, position=position_dodge(width=1)) +
          facet_wrap(vars(facet_var),ncol = 2)+  
          scale_y_continuous(
            limits = c(0, NA)
          )  +
          theme_bw()
        }
      }
      
    }
    
    p
    
    })
  get_incidence_attrition<-reactive({
    
    table<-incidence_attrition %>% 
      # first deselect settings which did not vary for this study
      select(!c(analysis_id, 
                #analysis_interval,
                analysis_repeated_events,
                analysis_outcome_washout,
                analysis_complete_database_intervals,
                result_id,
                denominator_days_prior_history,
                denominator_strata_cohort_definition_id,
                denominator_strata_cohort_name,
                denominator_start_date,
                denominator_end_date)) %>% 
      filter(database_name %in% input$incidence_database_name_selector)    %>% 
      filter(denominator_age_group %in% input$incidence_denominator_age_group_selector)     %>% 
      filter(denominator_sex %in% input$incidence_denominator_sex_selector)     %>% 
      #filter(denominator_days_prior_history %in% input$incidence_denominator_days_prior_history_selector)     %>% 
      filter(outcome_cohort_name %in% input$incidence_outcome_cohort_name_selector) %>%
    filter(analysis_interval %in% input$incidence_denominator_analysis_interval_selector) 
    table
  }) 
  output$tbl_incidence_attrition<-  DT::renderDataTable({
    
    table<-get_incidence_attrition() 
    
    validate(need(ncol(table)>1, 
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis_min_cell_count",
                "denominator_cohort_id", "outcome_cohort_id"))  %>% 
      rename(n = current_n,
             `Excluded (n)`= excluded,
             Reason = reason,
             Cancer = outcome_cohort_name,
             Age = denominator_age_group,
             Sex = denominator_sex,
             `Time Interval` = analysis_interval,
             Database = database_name,
             Step = step )
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "attrition_attrition"))
              ))
  })  
  
  # Survival
  # survival estimates: whole population
  get_survival_estimates<-reactive({
    
    table<-survival_estimates_whole %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector)  %>% 
      filter(Age %in% input$survival_age_group_selector)     %>% 
      #rename(Sex = Gender) %>% 
      filter(Sex %in% input$survival_sex_selector)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) 
    #%>%
      #filter(CalendarYearGp %in% input$calendar_year_selector) 
    
    table
  }) 
  output$tbl_survival_estimates<-  DT::renderDataTable({

    table<-get_survival_estimates()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    table <- table %>%
      mutate(time=nice.num3(time)) %>%
      mutate(est=nice.num3(est)) %>%
      mutate(ucl=nice.num3(ucl)) %>%
      mutate(lcl=nice.num3(lcl)) %>%
      select(!c("Stratification", "std.error")) %>%
      relocate(Cancer) %>%
      relocate(Estimate = est, .after = time) %>%
      relocate(upperCI = ucl, .after = Estimate) %>%
      relocate(lowerCI = lcl, .after = upperCI) 
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_estimates"))
              ))
  } )

  # KM plots for whole population
  output$plot_survival_estimates<- renderPlotly({

    table<-get_survival_estimates()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    if(is.null(input$survival_plot_group)){
      if(!is.null(input$survival_plot_facet)){
        p<-table %>%
          unite("facet_var",
                c(all_of(input$survival_plot_facet)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$time, y="est",
                            ymin = "lcl",
                            ymax = "ucl")) +
          geom_line() +
          #geom_point(position=position_dodge(width=1))+
          #geom_errorbar(width=0) +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      } else{
        p<-table %>%
          ggplot(aes_string(x=input$time, y="est",
                            ymin = "lcl",
                            ymax = "ucl")) +
          #geom_point(position=position_dodge(width=1))+
          #geom_errorbar(width=0) +
          scale_y_continuous(
            limits = c(0, NA)
          ) +
          theme_bw()
      }
    }


    if(!is.null(input$survival_plot_group) ){

      if(is.null(input$survival_plot_facet) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$survival_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$time, y="est",
                            ymin = "lcl",
                            ymax = "ucl",
                            group="Group",
                            colour="Group")) +
          geom_line() +
          #geom_point(position=position_dodge(width=1))+
          #geom_errorbar(width=0, position=position_dodge(width=1)) +
          theme_bw()
      }

      if(!is.null(input$survival_plot_facet) ){
        if(!is.null(input$survival_plot_group) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$survival_plot_group)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$survival_plot_facet)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$time, y="est",
                              ymin = "lcl",
                              ymax = "ucl",
                              group="Group",
                              colour="Group")) +
            #geom_point(position=position_dodge(width=1))+
            #geom_errorbar(width=0, position=position_dodge(width=1)) +
            geom_line() +
            facet_wrap(vars(facet_var),ncol = 2)+
            scale_y_continuous(
              limits = c(0, NA)
            )  +
            theme_bw()
        }
      }

    }

    p

  })
  
# survival estimates: calendar year population
  get_survival_estimates_cy<-reactive({
    
    table<-survival_estimates_calendar %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector_cy)  %>% 
      filter(Age %in% input$survival_age_group_selector_cy)     %>% 
      filter(Sex %in% input$survival_sex_selector_cy)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector_cy) %>%
      filter(CalendarYearGp %in% input$calendar_year_selector_cy) 
    
    table
  }) 
  output$tbl_survival_estimates_cy<-  DT::renderDataTable({
    
    table<-get_survival_estimates_cy()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      mutate(time=nice.num3(time)) %>%
      mutate(est=nice.num3(est)) %>%
      mutate(ucl=nice.num3(ucl)) %>%
      mutate(lcl=nice.num3(lcl)) %>%
      select(!c("Stratification", "std.error")) %>%
      relocate(Cancer) %>%
      relocate(Estimate = est, .after = time) %>%
      relocate(upperCI = ucl, .after = Estimate) %>%
      relocate(lowerCI = lcl, .after = upperCI) 
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_estimates_calendarYears"))
              ))
  } )
  
# KM plots for calendar year population
  output$plot_survival_estimates_cy<- renderPlotly({
    
    table<-get_survival_estimates_cy()
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    if(is.null(input$survival_plot_group_cy)){
      if(!is.null(input$survival_plot_facet_cy)){
        p<-table %>%
          unite("facet_var",
                c(all_of(input$survival_plot_facet_cy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$time, y="est", colour="CalendarYearGp")) +
          geom_line() +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(limits = c(NA, 1) ) +
          theme_bw()
      } else{
        p<-table %>%
          ggplot(aes_string(x=input$time, y="est", colour="CalendarYearGp")) +
          scale_y_continuous(
            limits = c(NA, 1)
          ) +
          theme_bw()
      }
    }
    
    
    if(!is.null(input$survival_plot_group_cy) ){
      
      if(is.null(input$survival_plot_facet_cy) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$survival_plot_group_cy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x=input$time, y="est",
                            group="Group",
                            colour="Group")) +
          geom_line() +
          theme_bw()
      }
      
      if(!is.null(input$survival_plot_facet_cy) ){
        if(!is.null(input$survival_plot_group_cy) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$survival_plot_group_cy)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$survival_plot_facet_cy)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x=input$time, y="est",
                              group="Group",
                              colour="Group")) +
            geom_line() +
            facet_wrap(vars(facet_var_cy),ncol = 2)+
            scale_y_continuous(limits = c(NA, 1) )  +
            theme_bw()
        }
      }
      
    }
    
    p
    
  })    
  
# risk table whole population 
  get_survival_risktable<-reactive({
    
    table<-survival_risk_table %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector)  %>% 
      filter(Age %in% input$survival_age_group_selector)     %>% 
      filter(Gender %in% input$survival_sex_selector)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) %>%
      relocate(`20`, .after = `18`)
     # filter(CalendarYearGp %in% input$calendar_year_selector) 
    
    table
  }) 
  output$tbl_survival_risk_table<-  DT::renderDataTable({
    
    table<-get_survival_risktable()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification")) %>% 
      rename(Sex = Gender, `Calendar Year` = CalendarYearGp)

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_risk_table"))
              ))
  } )
  
# risk table calendar years  
  get_survival_risktable_cy<-reactive({
    
    table<- survival_risk_table_cy %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector_cy)  %>% 
      filter(Age %in% input$survival_age_group_selector_cy)     %>% 
      filter(Gender %in% input$survival_sex_selector_cy)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector_cy) %>%
      filter(CalendarYearGp %in% input$calendar_year_selector_cy) 
    
    table
  }) 
  output$tbl_survival_risk_table_cy <-  DT::renderDataTable({
    
    table<-get_survival_risktable_cy()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification")) %>% 
      rename(Sex = Gender, 
             `Calendar Year` = CalendarYearGp )
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_risk_table_by_calendaryr"))
              ))
  } )

# median survival calendar years
  get_survival_median_table_cy<-reactive({

    table<- survival_median_table_cy %>%
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>%
      filter(Database %in% input$survival_database_name_selector_cy)  %>%
      filter(Age %in% input$survival_age_group_selector_cy)     %>%
      filter(Gender %in% input$survival_sex_selector_cy)     %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector_cy) %>%
      filter(CalendarYearGp %in% input$calendar_year_selector_cy)

    table
  })
  output$tbl_survival_median_table_cy <-  DT::renderDataTable({

    table<-get_survival_median_table_cy()

    validate(need(ncol(table)>1,
                  "No results for selected inputs"))

    table <- table %>%
      select(!c("Stratification", "n.max", "n.start")) %>%
    relocate(Cancer) %>% 
      rename(Sex = Gender) %>% 
      rename(`Records (n)` = records) %>% 
      mutate(median= ifelse(!is.na(median),paste0(median, " (",`0.95LCL`," - ",  `0.95UCL`, ")"))) %>% 
      rename(`Median Survival in Years (95% CI)` = median) %>% 
      mutate(rmean= ifelse(!is.na(rmean),paste0(rmean, " (",`se(rmean)`,")"))) %>% 
      rename(`rmean in Years (SE)` = rmean,
             `Events (n)` = events,
             `Calendar Year` = CalendarYearGp) %>% 
      select(!c("0.95LCL", "0.95UCL", "se(rmean)")) 
      
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survival_median_by_calendaryr"))
              ))
  } )

# median survival whole population
  get_survival_median_table<-reactive({
    
    table<-survival_median_table %>% 
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector)  %>% 
      filter(Age %in% input$survival_age_group_selector)     %>% 
      filter(Gender %in% input$survival_sex_selector)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) 
    
    table
  }) 
  output$tbl_survival_median_table<-  DT::renderDataTable({
    
    table<-get_survival_median_table()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification", "n.max", "n.start")) %>%
      relocate(Cancer) %>% 
      rename(Sex = Gender) %>% 
      rename(`Records (n)` = records) %>% 
      mutate(median= ifelse(!is.na(median),paste0(median, " (",`0.95LCL`," - ",  `0.95UCL`, ")"))) %>% 
      rename(`Median Survival in Years (95% CI)` = median) %>% 
      mutate(rmean= ifelse(!is.na(rmean),paste0(rmean, " (",`se(rmean)`,")"))) %>% 
      rename(`rmean in Years (SE)` = rmean,
             `Events (n)` = events,
             `Calendar Year` = CalendarYearGp) %>% 
      select(!c("0.95LCL", "0.95UCL", "se(rmean)")) 
      
    # 
    # table <- table %>%
    #   mutate(across(everything(), as.character)) %>%
    #   mutate(across(everything(), ~replace_na(.x, " ")))
    # 
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_median_survival"))
              ))
  } )

# survival risk rates calendar years
  get_survival_rates_table_cy<-reactive({
    
    table<- survival_rates_table_cy %>%
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>%
      filter(Database %in% input$survival_database_name_selector_cy)  %>%
      filter(Age %in% input$survival_age_group_selector_cy)     %>%
      filter(Sex %in% input$survival_sex_selector_cy)     %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector_cy) %>%
      filter(CalendarYearGp %in% input$calendar_year_selector_cy)
    
    table
  })
  output$tbl_survival_rates_table_cy <-  DT::renderDataTable({
    
    table<-get_survival_rates_table_cy()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification",
                "type",
                "logse",
                "conf.int",
                "conf.type",
                "n.risk",
                "n.event",
                "n.censor"
                
      )) %>%
      mutate(surv = surv * 100) %>% 
      mutate(lower = lower * 100) %>% 
      mutate(upper = upper * 100) %>% 
      mutate(surv=nice.num2(surv)) %>%
      mutate(std.err=nice.num3(std.err)) %>%
      mutate(cumhaz=nice.num3(cumhaz)) %>%
      mutate(std.chaz=nice.num3(std.chaz)) %>%      
      mutate(lower=nice.num2(lower)) %>%
      mutate(upper=nice.num2(upper)) %>%
      relocate(Cancer) %>%
      relocate(surv, .after = time) %>%
      relocate(lower, .after = surv) %>%
      relocate(upper, .after = lower) %>% 
      rename(`Time (years)` = time) %>% 
      mutate(surv= ifelse(!is.na(surv),paste0(surv, " (",lower," - ",  upper, ")"))) %>% 
      rename(`% Survival (95% CI)` = surv,
             `Calendar Year` = CalendarYearGp) %>% 
      select(!c("lower", "upper",
                "std.err",
                "cumhaz",
                "std.chaz"))
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survival_rates_by_calendaryr"))
              ))
  } )
  
# survival risk rates whole population
  get_survival_rates_table<-reactive({
    
    table<- survival_rates_table %>%
      # first deselect settings which did not vary for this study
      select(!c(GenderAge, Method)) %>%
      filter(Database %in% input$survival_database_name_selector)  %>%
      filter(Age %in% input$survival_age_group_selector)     %>%
      filter(Gender %in% input$survival_sex_selector)     %>%
      filter(Cancer %in% input$survival_outcome_cohort_name_selector)  
    
    table
  })
  output$tbl_survival_rates_table <-  DT::renderDataTable({
    
    table<-get_survival_rates_table()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification",
                "type",
                "logse",
                "conf.int",
                "conf.type",
                "n.risk",
                "n.event",
                "n.censor"
                
                )) %>%
      mutate(surv = surv * 100) %>% 
      mutate(lower = lower * 100) %>% 
      mutate(upper = upper * 100) %>% 
      mutate(surv=nice.num2(surv)) %>%
      mutate(std.err=nice.num3(std.err)) %>%
      mutate(cumhaz=nice.num3(cumhaz)) %>%
      mutate(std.chaz=nice.num3(std.chaz)) %>%      
      mutate(lower=nice.num2(lower)) %>%
      mutate(upper=nice.num2(upper)) %>%
      relocate(Cancer) %>%
      relocate(surv, .after = time) %>%
      relocate(lower, .after = surv) %>%
      relocate(upper, .after = lower) %>% 
      rename(Sex = Gender) %>% 
      rename(`Time (years)` = time) %>% 
      mutate(surv= ifelse(!is.na(surv),paste0(surv, " (",lower," - ",  upper, ")"))) %>% 
      rename(`% Survival (95% CI)` = surv,
             `Calendar Year` = CalendarYearGp) %>% 
      select(!c("lower", "upper",
                "std.err",
                "cumhaz",
                "std.chaz"))
                
                
    
    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv",
                                                 text = "Download results as csv",
                                                 filename = "survival_rates_whole_pop"))
              ))
  } )

# table 1
  get_table_one <-reactive({
    
    table<-table_one_results %>% 
      filter(Cancer %in% input$table1_outcome_cohort_name_selector) %>% 
      filter(Sex %in% input$table1_sex_selector) 

    table
  }) 
  
  
  output$tbl_table_one<-  DT::renderDataTable({
    
    table<-get_table_one()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>% 
      select(!c("analysis" )) %>% 
      relocate(Cancer, .after = `CPRD GOLD`) %>% 
      rename(Database = var)

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "tableOne"))
              ))
  } )

# plot for survival probabilities over calender year
  output$plot_survival_probs_cy<- renderPlotly({
    
    table<-get_survival_rates_table_cy()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    

    if(is.null(input$survival_rate_plot_group)){
      if(!is.null(input$survival_rate_facet_cy)){
        p <-table %>%
          unite("facet_var",
                c(all_of(input$survival_rate_facet_cy)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="CalendarYearGp", y="surv", colour = input$time)) +
          geom_point() +
          geom_line() +
          facet_wrap(vars(facet_var),ncol = 2)+
          scale_y_continuous(limits = c(NA, 1) ) +
          theme_bw()
      } else{
        p <-table %>%
          ggplot(aes_string(x="CalendarYearGp", y="surv",  colour = input$time)) +
          geom_point() +
          geom_line() +
          scale_y_continuous(
            limits = c(NA, 1)
          ) +
          theme_bw()
      }
    }
    
    
    if(!is.null(input$survival_rate_plot_group) ){
      
      if(is.null(input$survival_rate_facet_cy) ){
        p<-table %>%
          unite("Group",
                c(all_of(input$survival_rate_plot_group)), remove = FALSE, sep = "; ") %>%
          ggplot(aes_string(x="CalendarYearGp", y="surv",
                            group="Group",
                            colour="Group")) +
          geom_line() +
          theme_bw()
      }
      
      if(!is.null(input$survival_rate_facet_cy) ){
        if(!is.null(input$survival_rate_plot_group) ){
          p<-table %>%
            unite("Group",
                  c(all_of(input$survival_rate_plot_group)), remove = FALSE, sep = "; ") %>%
            unite("facet_var",
                  c(all_of(input$survival_rate_facet_cy)), remove = FALSE, sep = "; ") %>%
            ggplot(aes_string(x="CalendarYearGp", y="surv",
                              group="Group",
                              colour="Group")) +
            geom_line() +
            facet_wrap(vars(facet_var),ncol = 2)+
            scale_y_continuous(limits = c(NA, 1) )  +
            theme_bw()
        }
      }
      
    } 
    
    
    
    
    
    
      # if(!is.null(input$survival_rate_facet_cy)){
      #   p <-table %>%
      #     unite("facet_var",
      #           c(all_of(input$survival_rate_facet_cy)), remove = FALSE, sep = "; ") %>%
      #     ggplot(aes_string(x="CalendarYearGp", y="surv", colour = input$time)) +
      #     geom_point() +
      #     geom_line() +
      #     facet_wrap(vars(facet_var),ncol = 2)+
      #     scale_y_continuous(limits = c(NA, 1) ) +
      #     theme_bw()
      # } else {
      #   p <-table %>%
      #     ggplot(aes_string(x="CalendarYearGp", y="surv",  colour = input$time)) +
      #     geom_point() +
      #     geom_line() +
      #     scale_y_continuous(
      #       limits = c(NA, 1)
      #     ) +
      #     theme_bw()
      # }

    
    p
    
  })  
  
# table for median and mean follow up
  get_survival_followup_table<-reactive({
    
    table<-survival_followup_table %>% 
      # first deselect settings which did not vary for this study
      #select(!c(CalendarYearGp)) %>% 
      #select(!c(GenderAge, Method)) %>% 
      filter(Database %in% input$survival_database_name_selector)  %>% 
      filter(Age %in% input$survival_age_group_selector)     %>% 
      filter(Gender %in% input$survival_sex_selector)     %>% 
      filter(Cancer %in% input$survival_outcome_cohort_name_selector) 
    
    table
  }) 
  output$tbl_survival_followup_table<-  DT::renderDataTable({
    
    table<-get_survival_followup_table()
    
    validate(need(ncol(table)>1,
                  "No results for selected inputs"))
    
    table <- table %>%
      select(!c("Stratification")) %>% 
      mutate(median_followup=nice.num3(median_followup)) %>%
      mutate(lower_IQR=nice.num3(lower_IQR)) %>%
      mutate(upper_IQR=nice.num3(upper_IQR)) %>%
      mutate(mean_followup=nice.num3(mean_followup)) %>%
      mutate(sd_followup=nice.num3(sd_followup)) %>%
      relocate(Cancer) %>%
      relocate(Age, .after = sd_followup) %>%
      relocate(Gender, .after = Age) %>%
      relocate(Database, .after = Gender) %>% 
      mutate(median_followup= ifelse(!is.na(median_followup),paste0(median_followup, " (",lower_IQR," - ",  upper_IQR, ")"))) %>%
      mutate(mean_followup= ifelse(!is.na(mean_followup),paste0(mean_followup, " (",sd_followup, ")"))) %>% 
      rename(Sex = Gender,
             `Median Follow up in Years (IQR)` = median_followup,
             `Mean Follow up in Years (SD)` = mean_followup) %>% 
      select(!c("lower_IQR", "upper_IQR",
                "sd_followup"))

    datatable(table,
              rownames= FALSE,
              extensions = 'Buttons',
              options = list(lengthChange = FALSE,
                             dom = 'tB',
                             pageLength = 100000000,
                             buttons = list(list(extend = "csv", 
                                                 text = "Download results as csv",
                                                 filename = "survival_followup_survival"))
              ))
  } )
  
  
  
  
  
   
}