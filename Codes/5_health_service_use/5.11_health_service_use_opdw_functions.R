##' 11/11/2022
##' *Functions for Outpatient Data Analysis*
##' 

rate_multiplier <- 1000 # define rate denominator (i.e per 1000 population)

# Function creates standardised dataframe with pre-defined field formats for further processing by other functions.
outpatients_dataprep <- function(matched_df){
  
  # change column names to lower case
  names(matched_df) <- tolower(names(matched_df))
  
  # add age group column
  matched_df1 <- matched_df %>%
    mutate(age_group = cut(age,
                           breaks = c(17, 39, 49, 59, 69, 79, Inf),
                           labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))
 
  # attended appointment category (codes 5 and 6)
  # patient cancelled appointment (code 2)
  # patient missed appointment (codes 3, 7, and 8)
  # provider cancelled/postponed appointment (code 4)
  # no attendance (null and code 9, which is invalid)
  
  # add attendance descriptions 
  matched_df1 <- matched_df1 %>%
    mutate(attend_type_desc = case_when(
      attend_cd == 5 | attend_cd == 6 ~ 'Patient Attended',
      attend_cd == 2 ~ 'Patient Cancelled',
      attend_cd == 3 | attend_cd == 7 | attend_cd == 8 ~ 'Patient Missed',
      attend_cd == 4 ~ 'Provider Cancelled',
      TRUE ~ 'No Appointment'
    ))
  
  # add carer description
  matched_df1 <- matched_df1 %>%
    mutate(carer_desc = ifelse(carer_flag == 1, 'Unpaid carers', 'Non-carers'))
  
  # add column with total by carer type
  df_opdw_tot_id <- matched_df1 %>%
    group_by(carer_flag) %>%
    summarise(total_identified = n_distinct(alf_pe)) %>%
    ungroup()
  
  # add column with total by carer type and GP/LA identified 
  df_opdw_tot_id_lagp <- matched_df1 %>%
    group_by(carer_flag, first_identified_by) %>%
    summarise(total_identified_lagp = n_distinct(alf_pe)) %>%
    ungroup()
  

  
  # add column with total by age and sex by carer type and lagp (age sex standardisation denominator for 0,1,2+)
  df_opdw_tot_agesex_lagp <- matched_df1 %>%
    group_by(carer_flag, first_identified_by, age_group, sex) %>%
    summarise(total_agesex_identified_lagp = n_distinct(alf_pe)) %>%
    ungroup()
  

  # add totals to main df
  matched_df2 <- matched_df1 %>%
    merge(df_opdw_tot_id, all = TRUE)
  
  matched_df2 <- matched_df2 %>%
    merge(df_opdw_tot_id_lagp, all = TRUE)
  

  matched_df2 <- matched_df2 %>%
    merge(df_opdw_tot_agesex_lagp, all = TRUE)
  
  return(matched_df2)
  
}



# function counts individuals who had an outpatient event by carer status
outpatients_indv_count_table <- function(cleaned_df){
  
  matched_opdw_counts <- cleaned_df %>%
    
    #flag rows with a OPDW event with a 1, no OPDW event as a 0 
    mutate(opdw_event_flag = ifelse(!is.na(attend_cd) & attend_cd !=9, 1, 0)) %>%
    
    # group by identification cohort, count of total identified, admission type, and alf to count number of each admission type per cohort per person
    group_by(carer_flag, total_identified, alf_pe) %>%
    summarise(total_opdw_events = sum(opdw_event_flag)) %>%
    
    # group by identification cohort, count of total identified, admission type, and number of admissions to count number of carers per number of admissions by admission type
    group_by(carer_flag, total_identified, total_opdw_events) %>%
    summarise(count = n()) 
    
    # adding carer description back in to carer flagged data
    matched_opdw_counts <- matched_opdw_counts %>%
      mutate(carer_desc = factor(ifelse(carer_flag == 1, "Unpaid carer", "Non-carer"), 
                                 levels = c("Unpaid carer", "Non-carer")))

    return(matched_opdw_counts)
}

# function counts individuals who had an outpatient event by carer status and lagp
outpatients_indv_count_table_lagp <- function(cleaned_df){
  
  matched_opdw_counts_lagp <- cleaned_df %>%
    # flag rows with an OPDW event with a 1 and no OPDW event as a 0
    mutate(opdw_event_flag =  ifelse(!is.na(attend_cd) & attend_cd !=9, 1, 0)) %>%
    
    # group as above by carer flag
    group_by(carer_flag, first_identified_by, total_identified_lagp, alf_pe) %>%
    summarise(total_opdw_events_lagp = sum(opdw_event_flag)) %>%
    
    # group as above by carer flag
    group_by(carer_flag, first_identified_by, total_identified_lagp, total_opdw_events_lagp) %>%
    summarise(count = n()) 
  
  # adding carer description back in to carer flagged data
  matched_opdw_counts_lagp <- matched_opdw_counts_lagp %>%
    mutate(carer_desc = factor(ifelse(carer_flag == 1, "Unpaid carer", "Non-carer"), 
                               levels = c("Unpaid carer", "Non-carer")))

  return(matched_opdw_counts_lagp)
}


# Functions gets counts and percentages of number of individuals per bin 0/1/2+ by carer status
# E.g. how many individuals had 1 event and how mnay individuals had 2+ events. 
# the grouping '2+' was chosen to avoid issues with statistical disclosure control.
outpatients_indv_bins_table <- function (df_byCF){
  
  matched_counts_carerflag <- df_byCF%>%
    ungroup() %>%
    mutate(events_grp = cut(total_opdw_events,
                            breaks = c(-0.1, 0.1, 1.1, Inf),
                            labels = c('0', '1', '2+'), include.lowest = TRUE)) %>%
    
    group_by(carer_desc, events_grp) %>%
    mutate(count_grp = sum(count)) %>%
    
    group_by(carer_desc,  events_grp) %>%
    mutate(
           events_percentage_grp = count_grp/total_identified*100)

  # drop unneeded cols and add distinct_all()
  matched_counts_carerflag <- matched_counts_carerflag %>% 
    select(-total_opdw_events, -count) %>% 
    distinct_all()
  
  # view
  return(matched_counts_carerflag)
}

# Functions gets counts and percentages of number of individuals per bin 0/1/2+ by carer status and lagp
outpatients_indv_bins_table_lagp <- function (df_byCF_lagp){
  
  matched_counts_cf_lagp <- df_byCF_lagp%>%
    ungroup() %>%
    mutate(events_grp = cut(total_opdw_events_lagp,
                            breaks = c(-0.1, 0.1, 1.1, Inf),
                            labels = c('0', '1', '2+'), include.lowest = TRUE)) %>%
    
    group_by(carer_flag, first_identified_by, events_grp) %>%
    mutate(count_grp = sum(count)) %>%
    group_by(carer_flag, first_identified_by, events_grp) %>%
    mutate(events_percentage_grp_lagp = (count_grp / total_identified_lagp) * 100)
  # create a column for carer type
  matched_counts_cf_lagp <- matched_counts_cf_lagp %>%
    mutate(carer_desc = factor(ifelse(carer_flag == 1, "Unpaid carer", "Non-carer"), levels = c("Unpaid carer", "Non-carer")))
  
  # drop unneeded cols and add distinct_all()
  matched_counts_cf_lagp <- matched_counts_cf_lagp %>% 
    select(-total_opdw_events_lagp, -count) %>% 
    distinct_all()
  
  
  return(matched_counts_cf_lagp)
}





# Function counts the total number of each outpatient appointment event per person by carer status
# and group them by total events
outpatients_app_count_table <- function(cleaned_df){
  
  matched_opdw_app_counts_byCarerFlag <- cleaned_df %>%
    # flag rows with an OPDW event with a 1 and no OPDW event as a 0
    mutate(opdw_event_flag =  ifelse(!is.na(attend_cd) & attend_cd !=9, 1, 0)) %>%
    
    # group carer status, alf and attendance type
    group_by(carer_desc, total_identified, alf_pe, attend_type_desc) %>%
    summarise(total_opdw_events = sum(opdw_event_flag)) %>%
    
    # group above by carer status, attendance type and total event events
    group_by(carer_desc,  total_identified, attend_type_desc, total_opdw_events) %>%
    summarise(count = n()) 

    return(matched_opdw_app_counts_byCarerFlag)
}


# Function counts the total number of each outpatient appointment event per person by carer status and lagp
# and group them by total events
outpatients_app_count_table_lagp <- function(cleaned_df){
    
  matched_opdw_app_counts_lagp <- cleaned_df %>%
    mutate(opdw_event_flag =  ifelse(!is.na(attend_cd) & attend_cd !=9, 1, 0)) %>%
      
    # group carer status, first identified by, alf and attendance type
    group_by(carer_desc, first_identified_by, total_identified_lagp, alf_pe, attend_type_desc) %>%
    summarise(total_opdw_events_lagp = sum(opdw_event_flag)) %>%
    
    # group above by carer status, , first identified by, attendance type and total event events
    group_by(carer_desc, first_identified_by, total_identified_lagp, attend_type_desc, total_opdw_events_lagp) %>%
    summarise(count = n()) 
  
    return(matched_opdw_app_counts_lagp)
}


# Function adds percentages of the events 'attended', 'missed', 
# or 'cancelled' as percentages are more desriable than showing the raw counts by
# - by carer status
# - by carer status and lagp
outpatient_subset_proportions <- function(subset_df, subsetlagp_df){
  
  ### carer status
  # Patient attended, patient cancelled, patient missed, provider cancelled
  opdw_app_subset <- subset_df %>%
    filter(attend_type_desc == 'Patient Attended'
           | attend_type_desc == 'Patient Cancelled'
           | attend_type_desc == 'Patient Missed'
           | attend_type_desc == 'Provider Cancelled')
  
  # mutating to add new totals of just the four subsets interested in and percentage of that
  opdw_app_subset <- opdw_app_subset %>%
    # new column for total occurrences of attendances by attendance count
    mutate(apps_count = (total_opdw_events * count)) %>%
    
    # new total column from just the four subsets
    group_by(carer_desc) %>%
      mutate(apps_total = sum(total_opdw_events * count)) %>%
   
    # new columns for the counts grp by attend_type_desc and then percentage
    group_by(carer_desc, attend_type_desc) %>%
      mutate(apps_count_grp = sum(total_opdw_events * count),
           apps_percentage = ((total_opdw_events * count) / apps_total) * 100,
           apps_percentage_grp = sum((total_opdw_events * count) / apps_total) * 100) %>%
    
    # new column for event group bins 
    mutate(events_grp = cut(total_opdw_events,
                            breaks = c(-0.1, 0.1, 1.1, Inf),
                            labels = c('0', '1', '2+'), include.lowest = TRUE)) %>%
    
    # new column for count group
    group_by(carer_desc, events_grp) %>%
    mutate(count_grp = sum(count)) %>% 
    ungroup() %>% 
    # select subset binned columns
    select(carer_desc,  attend_type_desc, apps_total, apps_count_grp, apps_percentage_grp,count_grp, events_grp) %>% 
    distinct_all()

  # No Appointment
  opdw_no_app <- subset_df %>%
    filter(attend_type_desc == 'No Appointment')

   
  ### by carer status and lagp
  # Patient attended, patient cancelled, patient missed, provider cancelled
  opdw_app_subset_lagp <- subsetlagp_df %>%
    filter(attend_type_desc == 'Patient Attended'
           | attend_type_desc == 'Patient Cancelled'
           | attend_type_desc == 'Patient Missed'
           | attend_type_desc == 'Provider Cancelled')

  # mutating to add new totals of just the four subsets interested in and percentage of that
  opdw_app_subset_lagp <- opdw_app_subset_lagp %>%
    # new column for total occurrences of attendances by attendance count
    mutate(apps_count_cf = (total_opdw_events_lagp * count)) %>%
    
    # new total column from just the four subsets
    group_by(first_identified_by, carer_desc) %>%
      mutate(apps_total_lagp = sum(total_opdw_events_lagp * count)) %>%
    
    # new columns for the counts by attend_type_desc and then percentage
    group_by(first_identified_by, carer_desc, attend_type_desc) %>%
     mutate(apps_count_grp_lagp = sum(total_opdw_events_lagp * count),
           apps_percentage_lagp = ((total_opdw_events_lagp * count) / apps_total_lagp) * 100,
           apps_percentage_grp_lagp = sum((total_opdw_events_lagp * count) / apps_total_lagp) * 100) %>%
    
    # new column for event group by bins
    mutate(events_grp = cut(total_opdw_events_lagp,
                            breaks = c(-0.1, 0.1, 1.1, Inf),
                            labels = c('0', '1', '2+'), include.lowest = TRUE)) %>%
    
    # new column for count group
    group_by(carer_desc, first_identified_by, events_grp) %>%
    mutate(count_grp = sum(count)) %>% 
    ungroup() %>% 
    # select subset binned columns
    select(carer_desc, first_identified_by, attend_type_desc, apps_total_lagp, apps_count_grp_lagp, apps_percentage_grp_lagp,count_grp, events_grp) %>% 
    distinct_all()
   
  # No Appointment
  opdw_no_app_lagp <- subsetlagp_df %>%
    filter(attend_type_desc == 'No Appointment')
  
  # create list of tables to be returned
  df_list <- list(opdw_app_subset, opdw_no_app, opdw_app_subset_lagp, opdw_no_app_lagp)

  return(df_list)
}





## Rate functions
# function creates dummy dataframe for age-sex standardisation containing all combinations of required variables
create_dummy_table <- function(analysis_type){
  
  
  first_identified_by <- c("GP", "LA")
  sex <- c(1, 2)
  age_group <- c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")
  
  if(analysis_type == "overall"){
    events_grp <- c("Has outpatient appointment")
    
  } else if(analysis_type == "attended_alf" ){
    events_grp <- c( "Total attended appointments") #
    
  } else if(analysis_type == "attended_bin"){
        events_grp <- c('1', '2+')
  }
  
  # creating a dummy table containing all combinations of stratifying variables by admis_type
  dummy <- as.data.frame(expand.grid(first_identified_by, sex, age_group, events_grp))
  
  # assign column names
  names(dummy) <- c("first_identified_by", "sex", "age_group", "events_grp")
  
  return(dummy)
}


# function formats direct standardisation function (op_dsr) output
op_reformat_std_df <- function(std_df){
  # remove unneeded cols
  sub_std <- std_df %>% select(total_count, total_pop, first_identified_by, events_grp, value, lowercl, uppercl)
  
  sub_std <- sub_std %>%
    mutate(crude_prevrate = total_count/total_pop*rate_multiplier) %>%
    select(-total_count, -total_pop)
  
  
  # format crude rate table to have crude rates of unpaid carer and non-carers side in a wide table rather than long
  wider_df <- tidyr::pivot_wider(sub_std,
                                 id_cols = events_grp,
                                 names_from = first_identified_by,
                                 values_from = c("crude_prevrate", "value", "lowercl", "uppercl"))
  
  
  
  # calculate diff between gp vsla (gpvsla); lavsgp
  wider_df2 <- wider_df %>% 
    mutate(lavsgp_rateratio = value_LA/value_GP,
           lavsgp_lowercl = lowercl_LA/uppercl_GP,
           lavsgp_uppercl = uppercl_LA/lowercl_GP) %>% 
    # round all to 3dp
    mutate_if(is.numeric, ~round(., 3)) %>% 
    as.data.frame() %>% 
    # specify order of columns
    select( events_grp,
            crude_prevrate_LA, value_LA, lowercl_LA, uppercl_LA, 
            crude_prevrate_GP, value_GP, lowercl_GP, uppercl_GP, 
            lavsgp_rateratio, lavsgp_lowercl, lavsgp_uppercl)
  
  return(wider_df2)
  
}


# function performed direct standardisation
op_dsr <- function(dummy, count_df, denom_df, std_pop, la_name){
  
  # add local total to dummy
  prep_df1 <- dummy %>% left_join(denom_df)
  
  # add local count to above
  prep_df2 <- prep_df1 %>% left_join(count_df) 
  
  
  prep_df2$name <- la_name # create name variable for joining to stnd pop
  
  prep_df3 <- prep_df2 %>% 
    # left join with standard pop denominator
    left_join(std_pop, by = c("age_group" = "age", "sex" = "sex_code", "name" = "name"))
  

  # replace NA count group values with 0s
  prep_df3$count_grp[is.na(prep_df3$count_grp)] <- 0 
  
  # Using PHEindicatormethods package
  # x = numerator (stratified counts); n= denominator (statified totals in df); stdpop = standard population counts; 
  # stdpoptype = "field" indicates that standard population data is part of input data, otherwise it is "vector"(self-explanatory); 
  # multiplier = per how many (i.e put 1000 for per 1000)
  std_df <- prep_df3 %>%
    group_by(first_identified_by, events_grp) %>%
    phe_dsr(
      x = count_grp,
      n = total_agesex_identified_lagp,
      stdpop = pop,
      stdpoptype = "field",
      multiplier = rate_multiplier)
  
  std_df_neat <- op_reformat_std_df(std_df)
  
  return(std_df_neat)
  
}


# function calculates crude rate by carer status and carer status + lagp (depending on by_carertype parameter value)
op_cruderate_alf <- function(la_df, by_carertype){
  
  
  la_df <- la_df %>%
    #rename column carer_desc as Cater Type for chisq output
    mutate("Carer Type" = ifelse(carer_flag == 0, "non_carers", "unpaid_carers")) 
  
  cols <- c("event_grp",
            "total_identified_unpaid_carers", "count_grp_unpaid_carers", "crude_rate_unpaid_carers", "lowercl_unpaid_carers", "uppercl_unpaid_carers",           
            "total_identified_non_carers", "count_grp_non_carers", "crude_rate_non_carers", "lowercl_non_carers", "uppercl_non_carers",
            "ratio", "ratio_lowercl", "ratio_uppercl")
  
  # if comparing lagp unpaid carers
  if(by_carertype == 0){
    la_df <- la_df %>%
      filter(carer_flag == 1) %>% 
      #rename column carer_desc as Cater Type for chisq output
      mutate("Carer Type" = first_identified_by,
             total_identified = total_identified_lagp) 
    
    # update columns to select after crude rate calculation
    cols <- str_replace_all(cols, "unpaid_carers", "LA")
    cols <- str_replace_all(cols, "non_carers", "GP")
  }
  
  
  # create table with one row per alf for attendance 
  la_df1 <- la_df %>%
    # flag rows with an OPDW attendance (5 or 6) with a 1 and no OPDW event as a 0
    mutate(opdw_att_flag =  ifelse(!is.na(attend_dt) & attend_cd %in% c(5, 6), 1, 0)) %>%
    filter(opdw_att_flag == 1) %>% 
    #select needed columns
    select(alf_pe, `Carer Type`,  total_identified, opdw_att_flag) %>% 
    # distinct to one row per alf
    distinct_all() %>%
    # group variables for summarising counts
    group_by(`Carer Type`, total_identified) %>% 
    summarise(count_grp = n_distinct(alf_pe))  %>% 
    mutate(event_grp = "Total attended appointments")
    

  cruderate_df <- la_df1   %>% 
    #calculate crude rate
    mutate(crude_rate = round((count_grp/total_identified*rate_multiplier), digits = 3),
           lowercl = (rate_multiplier/total_identified) * (count_grp - (1.96 * sqrt(count_grp))),
           uppercl = (rate_multiplier/total_identified) * (count_grp + (1.96 * sqrt(count_grp))))
  
  # format crude rate table to have crude rates of unpaid carer and non-carers side in a wide table rather than long
  wider_cruderate <- tidyr::pivot_wider(cruderate_df,
                                        id_cols = event_grp,
                                        names_from =`Carer Type`,
                                        values_from = c("total_identified", "count_grp", "crude_rate", "lowercl", "uppercl")) %>% 
    as.data.frame()
  
  
  # ratio and upper/lower confidence level calculations

  wider_cruderate$ratio <- wider_cruderate[,7]/wider_cruderate[,6]
  wider_cruderate$ratio_lowercl <- wider_cruderate[,9]/wider_cruderate[,10]
  wider_cruderate$ratio_uppercl <- wider_cruderate[,11]/wider_cruderate[,8]
  
  
  wider_cruderate <- wider_cruderate %>% 
    #round to 3dp
    mutate_if(is.numeric, ~round(., 3)) %>% 
    # select columns at wanted order
    select(cols) %>%  
    filter(event_grp != 0)

  
  return(wider_cruderate)
  
  
}




## OP health service timeline functions ##
# function adds time group column for all alfs and relevant flags of having an appointment and having attended the appointment
op_add_timeline_cols <- function(cleaned_df){
  # create time group for appointments made
  #by timeline 3m/6m/1yr
  appt_timegrp <- cleaned_df %>% 
    select(alf_pe, first_identified_date, attend_dt) %>% 
    # select only those with appointment attendance
    filter(!is.na(attend_dt)) %>% 
    #create time intervals leading to index date
    mutate(appt_time_period =interval( attend_dt, first_identified_date) %/% months(1),
         appt_time_grp = case_when(
           appt_time_period >=0 & appt_time_period <= 3 ~ '3-0 months',
           appt_time_period >= 4 & appt_time_period <= 6 ~ '6-4 months',
           appt_time_period >=7 & appt_time_period <= 9 ~ '9-7 months',
           appt_time_period > 9   ~ '12-10 months'
           
         ), 
         appt_time_grp = factor(appt_time_grp,levels = c('12-10 months', '9-7 months','6-4 months', '3-0 months'  )))
  
  
  
  attended_timegrp <- cleaned_df %>% 
    select(alf_pe, carer_desc, first_identified_by, first_identified_date, attend_dt, attend_cd) %>% 
    
    mutate(
      # create flag for appointments (i.e attend_dt not null)
      appt_flag = ifelse(!is.na(attend_dt),1,0),
      # create flag for attended appointmetns i.e attend_cd is 5 or 6
      att_flag = ifelse(!is.na(attend_dt) & attend_cd %in% c(5, 6),1,0)) %>% 
   
    # create time intervals leading to index date
    mutate(attended_time_period = interval( attend_dt, first_identified_date) %/% months(1),
         attended_time_grp = case_when(
           attended_time_period >=0 & attended_time_period <= 3 ~ '3-0 months',
           attended_time_period >= 4 & attended_time_period <= 6 ~ '6-4 months',
           attended_time_period >=7 & attended_time_period <= 9 ~ '9-7 months',
           attended_time_period > 9   ~ '12-10 months'
           
         ), 
         attended_time_grp = factor(attended_time_grp,levels = c('12-10 months', '9-7 months','6-4 months', '3-0 months'  ))) %>% 
    group_by(carer_desc, first_identified_by, attended_time_grp) %>% 
    # get total appointments and attended appts by carer type, lagp and time group
    mutate(total_appt_lagp = sum(appt_flag),
           total_att_appts_lagp = sum(att_flag)) %>% 
    ungroup() %>% 
    # get total appointments and attended appts by carer type and time group
    group_by(carer_desc, attended_time_grp) %>% 
    mutate(total_appt = sum(appt_flag),
           total_att_appts = sum(att_flag)) %>% 
    ungroup() 
  
  
  # join tables to form timeline subset table
  timegrp_df <- cleaned_df %>% 
    filter(!is.na(attend_dt)) %>%
    select(alf_pe, carer_desc, first_identified_date, first_identified_by, attend_dt, attend_type_desc,total_identified, total_identified_lagp) %>% 
    left_join(appt_timegrp) %>% 
    left_join(attended_timegrp) 
  
  return(timegrp_df)
  
}


# function creates aggregated table for timeline for appointments attended over total cohort
op_timeline_appt_tables <- function(timegrp_df, by_lagp){
  # wouldnt add up to 100 as each alf can have multiple oupatient attendances in the year prior to their index date
  
  if(by_lagp == 1){
    timegrp_df1 <- timegrp_df %>% 
      filter(carer_desc == "Unpaid carers") %>% 
      mutate(cohort = paste0(first_identified_by, "-identified"),
             cohort = factor(cohort, levels=c('LA-identified', 'GP-identified')))
  } else{
    timegrp_df1 <- timegrp_df %>% 
      mutate(cohort = factor(carer_desc, levels=c('Unpaid carers', 'Non-carers')))
  }
  
  

    # % of individuals who attended appointments over total cohort
    if(by_lagp == 1){
      timeline_df <- timegrp_df1 %>%
        group_by(cohort, total_identified_lagp, attended_time_grp) %>% 
        summarise(count_grp = n_distinct(alf_pe))%>% 
        mutate(perc_grp = calc_percentage(count_grp, total_identified_lagp)) %>% 
        select(cohort, total_identified_lagp, attended_time_grp, count_grp, perc_grp) %>% 
        rename(time_grp = attended_time_grp) %>% 
        distinct_all() %>% ungroup() 
      
    } else{
     
      timeline_df <- timegrp_df1 %>%
        group_by(cohort, total_identified, attended_time_grp) %>% 
        summarise(count_grp = n_distinct(alf_pe))%>% 
        mutate(perc_grp = calc_percentage(count_grp, total_identified)) %>% 
        select(cohort, total_identified, attended_time_grp, count_grp, perc_grp) %>% 
        rename(time_grp = attended_time_grp) %>% 
        distinct_all() %>% ungroup()
      
    } 
    
    
  # remove those who did not have an appointment
  timeline_df <- timeline_df %>% filter(!is.na(time_grp)) 
  

  return(as.data.frame(timeline_df))
  
}


# function creates plot of the above function by carer status/carer status + lagp
op_timeline_perc_plots <- function(df, la_name, by_lagp){
  
  # pre-define labels and titles
  y_lab <- "Percentage (%)"
  x_lab <- "Month groups prior to index date"
  plot_type <- "dodge"
  
  context <- "individuals who attended outpatient appointments"
  
  
  if(by_lagp == 1){
    plot_title <- paste0(la_name, " percentage of ", context, " in months prior to \nindex assessment date for unpaid carers identified by GP and LA" )
    plot_palette <- lagpcarers_palette
    legend_title <- "Unpaid carers cohort"
    
  } else {
    plot_title <- paste0(la_name, " percentage of ", context, "  in months prior to \nindex assessment date by carer type" )
    plot_palette <- carertype_palette
    legend_title <- "Cohort"
  }
  

  df_plot <- df  %>% 
    ggplot() + 
    scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
    geom_col(aes(x = time_grp, 
                 y = perc_grp,
                 fill = cohort), 
             position = plot_type) +
    scale_fill_manual(values = plot_palette, 
                      guide = guide_legend(reverse = FALSE, title = legend_title)) 
  

  df_plot <- df_plot + 
    labs(
      title = plot_title,
      x = x_lab,
      y = y_lab
    ) + 
    geom_text(aes(x = time_grp,  
                  y = perc_grp, 
                  label = sprintf("%.1f", perc_grp),
                  group = cohort), 
              position= position_dodge(width = 1), 
              vjust=-.25,
              size = 3.5) + 
    theme(panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    panel.background = element_rect(fill = "white", color = "grey50"),
    strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"))
  
  
  return(df_plot)
  
}

