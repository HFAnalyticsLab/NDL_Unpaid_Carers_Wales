#' script contains functions that prepare demographic tables, including those used in supplementary plots '#

# function creates standardised dataframes that can be used in other demographic functions 
demog_dataprep <- function(unpaidcarer_df){

  names(unpaidcarer_df) <- tolower(names(unpaidcarer_df))
  
  # Add age group column
  unpaidcarer_df <- unpaidcarer_df %>%
    mutate(age_group = cut(age,
                           breaks = c(17, 39, 49, 59, 69, 79, Inf),
                           labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))
  
  # Add age group 20 year bands column
  unpaidcarer_df <- unpaidcarer_df %>%
    mutate(age_group_20 = cut(age,
                              breaks = c(17, 39, 59, 79, Inf),
                              labels = c("Under 39", "40-59", "60-79", "80+")))
  
  # Add column with year of assess.
  unpaidcarer_df <- unpaidcarer_df %>%
    mutate(index_yr = format(first_identified_date, "%Y"))
  
  # Add column with month of assess.
  unpaidcarer_df <- unpaidcarer_df %>%
    mutate(index_mth = format(first_identified_date, "%m"))
  
  # Add column with financial year of assess.
  unpaidcarer_df <- unpaidcarer_df %>%
    mutate(financial_yr = str_c(get_fy(first_identified_date, offset_period = -1), "/", get_fy(first_identified_date)))
  
  # Add column for quarter of assess.
  unpaidcarer_df <- unpaidcarer_df %>%
    mutate(index_quarter = cut(as.numeric(index_mth),
                               breaks = c(00, 03, 06, 09, 12),
                               labels = c("Q1", "Q2", "Q3", "Q4")))
  
  # create factor levels
  unpaidcarer_df <- unpaidcarer_df %>% 
    mutate(Sex = factor(sex, levels = rev(c("1", "2")), labels = rev(c("Male", "Female"))),
           WIMD_Quintile_2019 = factor(wimd, 
                                       levels = c("1", "2", "3", "4", "5"), 
                                       labels = c("1 (most)", "2", "3", "4", "5 (least)")),
           Sex_by_WIMD = paste0(Sex, ", ", WIMD_Quintile_2019),
           Sex_by_WIMD = factor(Sex_by_WIMD,
                                levels = c( "Female, 1 (most)"
                                            ,"Female, 2"
                                            ,"Female, 3"
                                            ,"Female, 4"        
                                            ,"Female, 5 (least)"
                                            ,"Male, 1 (most)"
                                            ,"Male, 2"
                                            ,"Male, 3"          
                                            ,"Male, 4"
                                            ,"Male, 5 (least)")))
  
  return(unpaidcarer_df)
}

# function create counts and percentages of unpaid carers by first identified method and various demographic factors
demog_overall_counts <- function(unpaidcarer_df){
  
  # Total
  unpaidcarer_df_overall_total <- unpaidcarer_df %>%
    group_by(identifiedby) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "overall", level = "overall")
  
  # Sex
  unpaidcarer_df_overall_sex <- unpaidcarer_df %>%
    group_by(identifiedby, sex) %>%
    count() %>%
    rename(count = n, level = sex) %>%
    mutate(variable = "sex")
  
  # Age group
  unpaidcarer_df_overall_age <- unpaidcarer_df %>%
    group_by(identifiedby, age_group) %>%
    count() %>%
    rename(count = n, level = age_group) %>%
    mutate(variable = "age")
  
  # WIMD
  unpaidcarer_df_overall_wimd <- unpaidcarer_df %>%
    group_by(identifiedby, wimd) %>%
    count() %>%
    rename(count = n, level = wimd) %>%
    mutate(variable = "wimd")
  
  # RUC
  unpaidcarer_df_overall_ruc <- unpaidcarer_df %>%
    group_by(identifiedby, ruc11_desc) %>%
    count() %>%
    rename(count = n, level = ruc11_desc) %>%
    mutate(variable = "ruc")
  
  # Age band by sex
  unpaidcarer_df_overall_age_sex <- unpaidcarer_df %>%
    group_by(identifiedby, age_group, sex) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "age_sex") 
  
  # Age band by wimd
  unpaidcarer_df_overall_age_wimd <- unpaidcarer_df %>%
    group_by(identifiedby, age_group, wimd) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "age_wimd") 
  
  # Age band 20 years by sex
  unpaidcarer_df_overall_age20_sex <- unpaidcarer_df %>%
    group_by(identifiedby, age_group_20, sex) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "age20_sex") 
  
  # Age band 20 years by wimd
  unpaidcarer_df_overall_age20_wimd <- unpaidcarer_df %>%
    group_by(identifiedby, age_group_20, wimd) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "age20_wimd") 
  
  # Sex by WIMD
  unpaidcarer_df_overall_sex_wimd <- unpaidcarer_df %>%
    group_by(identifiedby, sex, wimd) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "sex_wimd")
  
  # Merge
  unpaidcarer_df_overall <- unpaidcarer_df_overall_total %>%
    merge(unpaidcarer_df_overall_sex, all = TRUE) %>%
    merge(unpaidcarer_df_overall_age, all = TRUE) %>%
    merge(unpaidcarer_df_overall_wimd, all = TRUE) %>%
    merge(unpaidcarer_df_overall_sex_wimd, all = TRUE) %>%
    merge(unpaidcarer_df_overall_ruc, all = TRUE) %>%
    select(variable, level, everything()) %>%
    arrange(variable, level)
  
  # Add percentages
  unpaidcarer_df_overall <- unpaidcarer_df_overall %>%
    group_by(variable, identifiedby) %>%
    mutate(total = sum(count)) %>%
    ungroup() %>%
    mutate(percentage = calc_percentage(count, total)) 
  return(unpaidcarer_df_overall)
}


# function create counts and percentages of unpaid carers per calendar year, by first identified method and various demographic factors 
demog_calendaryear_counts <- function(unpaidcarer_df){
  ## Counts - yearly
  
  # Total
  unpaidcarer_df_yrly_total <- unpaidcarer_df %>%
    group_by(index_yr, identifiedby) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "yrly", level = "yrly")
  
  # Sex
  unpaidcarer_df_yrly_sex <- unpaidcarer_df %>%
    group_by(index_yr, identifiedby, sex) %>%
    count() %>%
    rename(count = n, level = sex) %>%
    mutate(variable = "sex")
  
  # Age group
  unpaidcarer_df_yrly_age <- unpaidcarer_df %>%
    group_by(index_yr, identifiedby, age_group) %>%
    count() %>%
    rename(count = n, level = age_group) %>%
    mutate(variable = "age")
  
  # Age group 20 years
  unpaidcarer_df_yrly_age_20 <- unpaidcarer_df %>%
    group_by(index_yr, identifiedby, age_group_20) %>%
    count() %>%
    rename(count = n, level = age_group_20) %>%
    mutate(variable = "age_20")
  
  # WIMD
  unpaidcarer_df_yrly_wimd <- unpaidcarer_df %>%
    group_by(index_yr, identifiedby, wimd) %>%
    count() %>%
    rename(count = n, level = wimd) %>%
    mutate(variable = "wimd")
  
  # RUC
  unpaidcarer_df_yrly_ruc <- unpaidcarer_df %>%
    group_by(index_yr, identifiedby, ruc11_desc) %>%
    count() %>%
    rename(count = n, level = ruc11_desc) %>%
    mutate(variable = "ruc")
  
  # Merge
  unpaidcarer_df_yrly <- unpaidcarer_df_yrly_total %>%
    merge(unpaidcarer_df_yrly_sex, all = TRUE) %>%
    merge(unpaidcarer_df_yrly_age, all = TRUE) %>%
    merge(unpaidcarer_df_yrly_age_20, all = TRUE) %>%
    merge(unpaidcarer_df_yrly_wimd, all = TRUE) %>%
    merge(unpaidcarer_df_yrly_ruc, all = TRUE) %>%
    select(variable, level, everything()) %>%
    arrange(variable, level)
  
  
  return(unpaidcarer_df_yrly)
  
}


# function create counts and percentages of unpaid carers per financial year, by first identified method and various demographic factors 
demog_financialyear_counts <- function(unpaidcarer_df){
  ## Counts - financial year
  
  # Total
  unpaidcarer_df_financial_yr_total <- unpaidcarer_df %>%
    group_by(financial_yr, identifiedby) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "financial_yr", level = "financial_yr")
  
  # Sex
  unpaidcarer_df_financial_yr_sex <- unpaidcarer_df %>%
    group_by(financial_yr, identifiedby, sex) %>%
    count() %>%
    rename(count = n, level = sex) %>%
    mutate(variable = "sex")
  
  # Age group
  unpaidcarer_df_financial_yr_age <- unpaidcarer_df %>%
    group_by(financial_yr, identifiedby, age_group) %>%
    count() %>%
    rename(count = n, level = age_group) %>%
    mutate(variable = "age")
  
  # WIMD
  unpaidcarer_df_financial_yr_wimd <- unpaidcarer_df %>%
    group_by(financial_yr, identifiedby, wimd) %>%
    count() %>%
    rename(count = n, level = wimd) %>%
    mutate(variable = "wimd")
  
  # RUC
  unpaidcarer_df_financial_yr_ruc <- unpaidcarer_df %>%
    group_by(financial_yr, identifiedby, ruc11_desc) %>%
    count() %>%
    rename(count = n, level = ruc11_desc) %>%
    mutate(variable = "ruc")
  
  # Merge
  unpaidcarer_df_financial_yr <- unpaidcarer_df_financial_yr_total %>%
    merge(unpaidcarer_df_financial_yr_sex, all = TRUE) %>%
    merge(unpaidcarer_df_financial_yr_age, all = TRUE) %>%
    merge(unpaidcarer_df_financial_yr_wimd, all = TRUE) %>%
    merge(unpaidcarer_df_financial_yr_ruc, all = TRUE) %>%
    select(variable, level, everything()) %>%
    arrange(variable, level)
  
  return(unpaidcarer_df_financial_yr)
  
}


# function creates various splits of demographic table to be used in plots.
# list of tables are saved into an excel file per LA (1 sheet per table)
demog_prep_plot_tables <- function(overall_df, la_name){
  
  overall_counts <- overall_df %>%  filter(variable == "overall") 
  
  #Sex
  overall_sex <- overall_df %>%
    filter(variable == "sex") %>% 
    mutate(factor_levels = factor(level, levels = rev(c("1", "2")), labels = rev(c("Male", "Female")))) 
  
  #Age_Group
  overall_age <- overall_df %>%
    filter(variable == "age") %>%
    mutate(factor_levels = factor(level, levels = rev(c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))) 
  
  #WIMD_Quintile_2019
  overall_wimd <- overall_df %>%
    filter(variable == "wimd") %>%
    mutate(factor_levels = factor(level, 
                                       levels = rev(c("1", "2", "3", "4", "5")), 
                                       labels = rev(c("1 (most)", "2", "3", "4", "5 (least)")))) 
  #Sex_by_WIMD
  overall_sexwimd <- overall_df %>%
    filter(variable == "sex_wimd") %>%
    group_by(identifiedby, sex) %>% 
    mutate(total = sum(count)) %>% ungroup() %>%
    mutate(percentage = calc_percentage(count, total)) %>% 
    mutate(sex = factor(sex, levels = rev(c("1", "2")), labels = rev(c("Male", "Female")))) %>%
    mutate(WIMD_Quintile_2019 = factor(wimd,
                                       levels = rev(c("1", "2", "3", "4", "5")),
                                       labels = rev(c("1 (most)", "2", "3", "4", "5 (least)")))) %>% 
    mutate(level = paste0(sex, ", ", WIMD_Quintile_2019)) %>% 
    mutate( factor_levels = factor(level,
                                 levels = rev(c( "Female, 5 (least)"
                                                 ,"Female, 4"
                                                 ,"Female, 3"
                                                 ,"Female, 2"        
                                                 ,"Female, 1 (most)"
                                                 ,"Male, 5 (least)"
                                                 ,"Male, 4"
                                                 ,"Male, 3"          
                                                 ,"Male, 2"
                                                 ,"Male, 1 (most)"))))
    
    
  #Rurality
  overall_ruc <- overall_df %>%
    filter(variable == "ruc") %>%
    mutate(factor_levels = factor(level,
                             levels = rev(c("Urban city and town", "Rural town and fringe","Rural town and fringe in a sparse setting", "Rural village and dispersed", "Rural village and dispersed in a sparse setting")))) 
  name_str <- "demog"
  
  df_list <- list()
  df_list[[paste0(name_str, "_cnt_",substr(la_name,1,7))]] <- overall_counts
  df_list[[paste0(name_str, "_cntperc_sex_",substr(la_name,1,7))]] <- overall_sex
  df_list[[paste0(name_str, "_cntperc_age_",substr(la_name,1,7))]] <- overall_age
  df_list[[paste0(name_str, "_cntperc_wimd_",substr(la_name,1,7))]] <- overall_wimd
  df_list[[paste0(name_str, "_cntperc_sexwimd_",substr(la_name,1,7))]] <- overall_sexwimd
  df_list[[paste0(name_str, "_cntperc_ruc_",substr(la_name,1,7))]] <- overall_ruc
  
  
  # write to excel book with multiple sheets
  demographics_path <- '' # add your path
  
  if(tolower(la_name) %in% c("swansea", "npt", "denbighshire")){
    write.xlsx(df_list, paste0(demographics_path,'data/processed/1429_',la_name,'_demographics_countsperc.xlsx'))
    
  }else(
    write.xlsx(df_list[[1]], paste0(demographics_path,'data/processed/1429_',la_name,'_demographics_countsperc.xlsx'))
  )
  
  
  return(df_list)
  
}


library(gtsummary)
library(openxlsx)
# function evaluates and formats chi-square tests for demographic factors.
# saved into an excel file per LA 
write_demog_stats <- function(la_df, la_name){
  
  # prepare data
  la_df2 <- la_df %>% 
    select(identifiedby, Sex, age_group, WIMD_Quintile_2019, ruc11_desc)
  
  
  names(la_df2) <- c("Identified By", "Sex", "Age Group", "WIMD Quintile (2019)", "Rurality") 
  
  # run chi-square test on sex and wimd
  stats_df <- la_df2 %>% 
    tbl_summary(by="Identified By") %>%  
    add_p(test= all_categorical() ~ "chisq.test", 
          # format p-value to 2dp
          pvalue_fun = ~ style_pvalue(.x,digits = 2)) %>% 
    # add header to statistics column which is hidden by default
    modify_header(statistic ~ "**Chi-sq Statistic**") %>% 
    # format chi-square results to 3dp
    modify_fmt_fun(statistic  ~ purrr::partial(style_sigfig, digits=3))  %>% 
    gtsummary::as_tibble()

 
  # create data for sex wimd (female) chi-sqare test
  sexwimd_f <- la_df %>% filter(sex == 2) %>% 
    select(identifiedby, Sex_by_WIMD) %>% 
    mutate( Sex_by_WIMD = factor(Sex_by_WIMD,
                                   levels = rev(c( "Female, 5 (least)"
                                  ,"Female, 4"
                                  ,"Female, 3"
                                  ,"Female, 2"
                                  ,"Female, 1 (most)"
                                  ))))
  
  
  names(sexwimd_f) <- c("Identified By","Sex by WIMD Quintile (Female)") 
  
  # run chi-square test
  sexwimd_f_stats <- sexwimd_f %>% 
    tbl_summary(by="Identified By") %>%  
    add_p(test= all_categorical() ~ "chisq.test", 
          pvalue_fun = ~ style_pvalue(.x,digits = 2)) %>% 
    modify_header(statistic ~ "**Chi-sq Statistic**") %>% 
    modify_fmt_fun(statistic  ~ purrr::partial(style_sigfig, digits=3)) %>% 
    gtsummary::as_tibble()
  
  
  
  # create data for sex-wimd (male) chi-sqare test
  sexwimd_m <- la_df %>% filter(sex == 1) %>% 
    select(identifiedby, Sex_by_WIMD) %>% 
    mutate( Sex_by_WIMD = factor(Sex_by_WIMD,
                                 levels = rev(c("Male, 5 (least)"
                                                ,"Male, 4"
                                                ,"Male, 3"
                                                ,"Male, 2"
                                                ,"Male, 1 (most)"))))
  
  
  names(sexwimd_m) <- c("Identified By","Sex by WIMD Quintile (Male)") 
  
  # run test
  sexwimd_m_stats <- sexwimd_m %>% 
    tbl_summary(by="Identified By") %>%  
    add_p(test= all_categorical() ~ "chisq.test", 
          pvalue_fun = ~ style_pvalue(.x,digits = 2)) %>% 
    # add header to statistics column which is hidden by default
    modify_header(statistic ~ "**Chi-sq Statistic**") %>% 
    modify_fmt_fun(statistic  ~ purrr::partial(style_sigfig, digits=3)) %>% 
    gtsummary::as_tibble()
  
  
  # save outputs
  wb <- createWorkbook()
  
  # write other stats table to xlsx
  sname <- paste0(la_name,"_demog")
  addWorksheet(wb, sname)
  writeData(wb, sheet = sname, x= stats_df)
  
  # add sexwimd female
  sname <- paste0(la_name,"_sexwimd_female")
  addWorksheet(wb, sname)
  writeData(wb, sheet = sname, x= sexwimd_f_stats)
  
  # add sexwimd Male
  sname <- paste0(la_name,"_sexwimd_male")
  addWorksheet(wb, sname)
  writeData(wb, sheet = sname, x= sexwimd_m_stats)
  
  # export excel
  saveWorkbook(wb, paste0(p, "data/processed/1429_", "demog_",la_name,"_carersOnly_stats", ".xlsx"), overwrite = TRUE)
  
  # return underlying table
  return(la_df2)
  
}


