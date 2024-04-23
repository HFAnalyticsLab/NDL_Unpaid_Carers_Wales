## This script contains function that support creating identification timelines for each LA using LA and GP data.

# function standardises format and structure of datasets used to create timelines
timeline_dataprep <- function(la_df){
  names(la_df) <- tolower(names(la_df))

  
  # Add age group column
  la_df <- la_df %>%
    mutate(age_group = cut(age,
                           breaks = c(17, 39, 49, 59, 69, 79, Inf),
                           labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))
  
  # Add column with financial year of assess.
  la_df <- la_df %>%
    filter(event_dt >= '2017-04-01' & event_dt <= '2022-03-31') %>% # NB: All data is now filtered to financial years
    mutate(financial_yr = str_c(get_fy(event_dt, offset_period = -1, opt_fy_start = "04-01"), "/", get_fy(event_dt, opt_fy_start = "04-01")))
  
  # Add column with year of assess.
  la_df <- la_df %>%
    mutate(index_yr = format(event_dt, "%Y"))
  
  # Add column with month of assess.
  la_df <- la_df %>%
    mutate(index_mth = format(event_dt, "%m"))
  
  # Add column for quarter of assess.
  la_df <- la_df %>%
    mutate(index_quarter = cut(as.numeric(index_mth),
                               breaks = c(00, 03, 06, 09, 12),
                               labels = c("Q1", "Q2", "Q3", "Q4")))

  return(la_df)
}

# function create counts in financial years overall and split by various demographic factors 
timeline_countsby_financialyr <- function(la_df, cohort_name){
  # Count by year
  la_df_financial_yr <- la_df %>%
    group_by(financial_yr) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "year", level = "year", cohort = cohort_name) %>%
    arrange(financial_yr)
  
  # Count by year and sex
  la_df_financial_yr_sex <- la_df %>%
    group_by(sex, financial_yr) %>%
    count() %>%
    rename(count = n, level = sex) %>%
    mutate(variable = "sex", cohort = cohort_name) %>%
    arrange(level, financial_yr)
  
  # Count by year and age group
  la_df_financial_yr_age <- la_df %>%
    group_by(age_group, financial_yr) %>%
    count() %>%
    rename(count = n, level = age_group) %>%
    mutate(variable = "age_group", cohort = cohort_name) %>%
    arrange(level, financial_yr)
  
  # count by year and WIMD
  la_df_financial_yr_wimd <- la_df %>%
    group_by(wimd_desc, financial_yr) %>%
    count() %>%
    rename(count = n, level = wimd_desc) %>%
    mutate(variable = "wimd", cohort = cohort_name) %>%
    arrange(level, financial_yr)
  
  # count by year and RUC
  la_df_financial_yr_ruc <- la_df %>%
    group_by(ruc_desc, financial_yr) %>%
    count() %>%
    rename(count = n, level = ruc_desc) %>%
    mutate(variable = "ruc", cohort = cohort_name) %>%
    arrange(level, financial_yr)
  
  # merge 
  la_df_yr <- la_df_financial_yr %>%
    merge(la_df_financial_yr_sex, all = TRUE) %>%
    merge(la_df_financial_yr_age, all = TRUE) %>%
    merge(la_df_financial_yr_wimd, all = TRUE) %>%
    merge(la_df_financial_yr_ruc, all = TRUE) %>%
    select(variable, level, everything()) %>%
    arrange(variable, level)
  
  return(la_df_yr)
}

# function create counts by index quarter and year overall and split by various demographic factors 
timeline_countsby_quarter <- function(la_df, cohort_name){
  # count by index quarter and year
  la_df_index_quarter <- la_df %>%
    group_by(index_yr, index_quarter) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "year", level = "year", cohort = cohort_name) %>%
    arrange(index_yr, index_quarter)
  
  # count by index quarter, year and sex
  la_df_index_quarter_sex <- la_df %>%
    group_by(index_yr, index_quarter, sex) %>%
    count() %>%
    rename(count = n, level = sex) %>%
    mutate(variable = "sex", cohort = cohort_name) %>%
    arrange(level, index_yr, index_quarter)
  
  # count by index quarter, year and age
  la_df_index_quarter_age <- la_df %>%
    group_by(index_yr, index_quarter, age_group) %>%
    count() %>%
    rename(count = n, level = age_group) %>%
    mutate(variable = "age_group", cohort = cohort_name) %>%
    arrange(level, index_yr, index_quarter)
  
  # count by index quarter, year and wimd
  la_df_index_quarter_wimd <- la_df %>%
    group_by(index_yr, index_quarter, wimd_desc) %>%
    count() %>%
    rename(count = n, level = wimd_desc) %>%
    mutate(variable = "wimd", cohort = cohort_name) %>%
    arrange(level, index_yr, index_quarter)
  
  # count by index quarter, year and ruc
  la_df_index_quarter_ruc <- la_df %>%
    group_by(index_yr, index_quarter, ruc_desc) %>%
    count() %>%
    rename(count = n, level = ruc_desc) %>%
    mutate(variable = "ruc", cohort = cohort_name) %>%
    arrange(level, index_yr, index_quarter)
  
  # Merge
  la_df_quarter <- la_df_index_quarter %>%
    merge(la_df_index_quarter_sex, all = TRUE) %>%
    merge(la_df_index_quarter_age, all = TRUE) %>%
    merge(la_df_index_quarter_wimd, all = TRUE) %>%
    merge(la_df_index_quarter_ruc, all = TRUE) %>%
    select(variable, level, everything()) %>%
    arrange(variable, level)
  return(la_df_quarter)
}



# function counts total identified by index year and  quarters (does not include demographic breakdown)
counts_quarterly <- function(la_df, la_name){
  
  la_df2 <- la_df %>%
    group_by(index_yr, index_quarter) %>%
    count() %>%
    rename(count = n) %>%
    mutate(variable = "year", level = "year", cohort = la_name) %>%
    arrange(index_yr, index_quarter)
  
  return(la_df2)
  
}

# create timeline plots for identification counts by demographic factor level and financial year
timeline_plots_yrly_demog <- function(df_demog){
  out_df <- df_demog %>%  ggplot() +
    geom_line(aes(x = financial_yr, 
                  y = count, 
                  colour = cohort,
                  group = cohort)) +
    
    facet_grid(~factor_levels) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    ) 
  return(out_df)
  
}

# create timeline plots for identification counts by demographic factor level and index year and quarter
timeline_plots_qtrly_demog <- function(df_demog, if_bylagp){
  out_df <- df_demog %>%  ggplot() +
    geom_line(aes(x = interaction(index_quarter, index_yr),
                  y = count,
                  group = level,
                  colour = level)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
      )
  return(out_df)
  
}

# create timeline plots for identification counts by index year and quarter and by cohort
timeline_plots_la_qtrly <- function(df_la){
  
  out_df <-  df_la %>%
   ggplot() +
    geom_line(aes(x = interaction(index_quarter, index_yr),
                  y = count,
                  group = cohort)) +
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
    )
  return(out_df)
}
