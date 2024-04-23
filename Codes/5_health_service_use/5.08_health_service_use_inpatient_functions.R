# Functions for Inpatient data analysis
rate_multiplier <- 1000 # define multiplier for direct standardisation method

# function creates standardised (cleaned) dataframe in a structure that can be used by other functions.
inpatients_dataprep <- function(raw_la_df){
  
  
  # change column names to lower case
  names(raw_la_df) <- tolower(names(raw_la_df))
  
  # Add age group column
  la_df <- raw_la_df %>%
    mutate(age_group = cut(age,
                           breaks = c(17,39, 49, 59, 69, 79, Inf),
                           labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))
  
  # Add admis_type description
  la_df <- la_df %>%
    mutate(admis_type_desc = ifelse(is.na(admis_type), 'No admission', 
                                    ifelse(admis_type == 0, 'Planned',
                                           ifelse(admis_type == 1, 'Emergency', 'NA'))))
  # Add levels to identified by
  la_df <- la_df %>%
    mutate(first_identified_by = factor(first_identified_by, 
         levels = c('GP', 'LA')))
  
  # Add carer description
  la_df <- la_df %>% mutate(carer_desc = ifelse(carer_flag == 1,'Unpaid carers', 'Non-carers')) %>% 
    mutate(carer_desc = factor(carer_desc, 
                                        levels = c('Unpaid carers', 'Non-carers')))
  
  

  # Add column tota for carer type
  df_pedw_tot_id <- la_df %>%
    group_by(carer_flag) %>%
    summarise(total_identified = n_distinct(alf_pe)) %>%
    ungroup()
  
  # Add column with total carer type and GP/LA identified
  df_pedw_lagp_tot_id <- la_df %>%
    group_by(carer_flag, first_identified_by) %>%
    summarise(total_identified_lagp = n_distinct(alf_pe)) %>%
    ungroup()
  
  
  # Add by timeline 3m/6m/1yr
  timeline <- la_df %>% 
    select(alf_pe, admis_dt, first_identified_date) %>% 
    # select only those with ed attendance
    filter(!is.na(admis_dt)) %>% 
    #create time intervals leading to index date
    mutate(adm_time_period =interval( admis_dt, first_identified_date) %/% months(1),
           adm_time_grp = case_when(
             adm_time_period >=0 & adm_time_period <= 3 ~ '3-0 months',
             adm_time_period >= 4 & adm_time_period <= 6 ~ '6-4 months',
             adm_time_period >=7 & adm_time_period <= 9 ~ '9-7 months',
             adm_time_period > 9   ~ '12-10 months'
             
           ), 
           adm_time_grp = factor(adm_time_grp,levels = c('12-10 months', '9-7 months','6-4 months', '3-0 months'  )))
  
  
  # Add these totals to the main df
  cleaned_df <- la_df %>%
    merge(df_pedw_tot_id, all = TRUE)
  
  cleaned_df <- cleaned_df %>%
    merge(df_pedw_lagp_tot_id, all = TRUE)
  
  cleaned_df <- cleaned_df %>%
    merge(timeline, all = TRUE)
  
  return(data.frame(cleaned_df))
  
}



# function creates a df with counts and percentages of the total cohort with each admission type by carer status 
inpatients_countperc_table <-function(cleaned_df){
  matched_pedw_counts <- cleaned_df %>%
    # Flag rows with a PEDW a event with a 1, no PEDW event as a 0
    mutate(pedw_event_flag = ifelse(!is.na(admis_dt), 1, 0)) %>%
    
    # group by  identification cohort, count of total identified, admission type and alf to count number of each admission type per cohort per person.
    group_by( carer_desc,total_identified, alf_pe, admis_type_desc) %>%
    summarise(total_pedw_events = sum(pedw_event_flag)) %>% 
    
    # group by identification cohort, count of total identified, admission type and number of admissions to count number of carers per number of admissions by admission type
    group_by( carer_desc, total_identified, admis_type_desc, total_pedw_events) %>%
    summarise(count = n()) %>%
    mutate(percentage = calc_percentage(count, total_identified)) 
  
  
  return(matched_pedw_counts)
  
}

# function creates the following outputs :
# 1. df with counts and percentages of the total cohort with each admission type by carer status and lagp identified
# 2. df with counts by age sex for unpaid carers only by lagp to be used in direct standardisation function as numerator
#   2a. by overall (any type of admission)
#   2b. by planned and emergency admission types
# 3. df with age sex totals for unpaid carers only by lagp to be used in direct standardisation function as denominator
inpatients_countperc_table_lagp<- function(cleaned_df, la_name){
  
  # 1. df with counts and percentages of the total cohort with each admission type
  matched_pedw_counts_lagp <- cleaned_df %>%
    # Flag rows with a PEDW a event with a 1, no PEDW event as a 0
    mutate(pedw_event_flag = ifelse(!is.na(admis_dt), 1, 0)) %>%
    
    # group by carer_flag, identification cohort, count of total identified, admission type and alf to count number of each admission type per cohort per person.
    group_by( carer_desc,first_identified_by, total_identified_lagp, alf_pe, admis_type_desc) %>%
    summarise(total_pedw_events_lagp = sum(pedw_event_flag)) %>%
    
    # group by identification cohort, count of total identified, admission type and number of admissions to count number of carers per number of admissions by admission type
    group_by(carer_desc, first_identified_by, total_identified_lagp, admis_type_desc, total_pedw_events_lagp) %>%
    summarise(count = n()) %>%
    mutate(percentage = calc_percentage(count, total_identified_lagp))
  
  
  # 2a.counts by age sex overall for unpaid carers only by lagp to be used in direct standardisation function
  agesex_pedw_counts_overall_lagp <- cleaned_df %>% 
    #only unpaid carers are used in direct standardisation
    filter(carer_flag ==1) %>% 
  
    # Flag rows with a PEDW a event with a 1, no PEDW event as a 0
    mutate(pedw_event_flag = ifelse(!is.na(admis_dt), 1, 0)) %>%
    
    # standardise admis_type to "Has hospitalisation"
    mutate(admis_type_desc = ifelse(pedw_event_flag == 1, "Has hospitalisation", admis_type_desc)) %>% 
    
    # group by carer_flag, identification cohort, count of total identified, admission type and alf to count number of each admission type per cohort per person.
    group_by(first_identified_by, age_group, sex, alf_pe, admis_type_desc) %>%
    summarise(total_pedw_events_lagp = sum(pedw_event_flag)) %>%
    
    # group by identification cohort, count of total identified, admission type and number of admissions to count number of carers per number of admissions by admission type
    group_by(first_identified_by, age_group, sex,  admis_type_desc, total_pedw_events_lagp) %>%
    summarise(count = n()) %>% 
    
    # split into 0/1 bins
    mutate(events_grp = cut(total_pedw_events_lagp,
                            breaks = c(-0.1, 0.1,  Inf),
                            labels = c('0', '1+'), include.lowest = TRUE)) %>%
    group_by( first_identified_by, age_group, sex,   events_grp) %>%
    mutate(count_grp = sum(count)) %>% 
    select(first_identified_by, age_group, sex,  admis_type_desc, events_grp, count_grp) %>% 
    distinct_all() %>%
    ungroup()
  
  
  # 2b.counts by age sex for each admission type for unpaid carers only by lagp to be used in direct standardisation function
  agesex_pedw_counts_admistype_lagp <- cleaned_df %>% 
    filter(carer_flag ==1) %>% 
    # Flag rows with a PEDW a event with a 1, no PEDW event as a 0
    mutate(pedw_event_flag = ifelse(!is.na(admis_dt), 1, 0)) %>%
    # remove those witn no admissions
    filter(admis_type_desc != "No admission") %>% 
    # group by carer_flag, identification cohort, count of total identified, admission type and alf to count number of each admission type per cohort per person.
    group_by( first_identified_by, age_group, sex, alf_pe, admis_type_desc) %>%
    summarise(total_pedw_events_lagp = sum(pedw_event_flag)) %>%
    
    # group by identification cohort, count of total identified, admission type and number of admissions to count number of carers per number of admissions by admission type
    group_by(first_identified_by, age_group, sex, admis_type_desc, total_pedw_events_lagp) %>%
    summarise(count = n())  %>% 
    # split into 0/1 bins
    mutate(events_grp = cut(total_pedw_events_lagp,
                            breaks = c(-0.1, 0.1,  Inf),
                            labels = c('0', '1+'), include.lowest = TRUE)) %>%
    group_by(first_identified_by, age_group, sex, admis_type_desc, events_grp) %>%
    mutate(count_grp = sum(count)) %>% 
    select(first_identified_by, age_group, sex,  admis_type_desc, events_grp, count_grp) %>% 
    distinct_all() %>%
    ungroup()
  
  # combine overall has admissions and by admission types
  matched_agesex_pedw_counts_lagp <- rbind(agesex_pedw_counts_overall_lagp, agesex_pedw_counts_admistype_lagp)

  # remove unneeded cols and no admissions
  matched_agesex_pedw_counts_lagp <- matched_agesex_pedw_counts_lagp %>% 
    filter(admis_type_desc != "No admission") %>% 
    select(first_identified_by, age_group, sex,  admis_type_desc,  count_grp) 
  
  # 3. Count age sex totals for direct standardisation denominator
  agesex_pedw_counts_denom <-cleaned_df %>% 
    filter(carer_flag ==1) %>% 
    group_by(first_identified_by, age_group, sex) %>%
    summarise(total_identified_agesex = n_distinct(alf_pe)) %>%
    ungroup()
  
  # place dataframes into a list temporarily to be converted into environment variables
  countperc_list <- list()
  countperc_list[[paste0(la_name, "_matched_pedw_counts_lagp")]] <- matched_pedw_counts_lagp
  countperc_list[[paste0(la_name, "_agesex_pedw_counts_hf")]] <- matched_agesex_pedw_counts_lagp
  countperc_list[[paste0(la_name, "_agesex_pedw_totals_hf")]] <- agesex_pedw_counts_denom
  
  list2env(countperc_list, envir = .GlobalEnv) # convert list items into environment variables

}


# function creates percentage of individuals with 0/1+ admissions for each admission type over total cohort by carer status
inpatients_binsperc_table <- function(df_byGPLA){

  matched_counts_grp <- df_byGPLA %>%
    ungroup() %>%
    mutate(events_grp = cut(total_pedw_events,
                            breaks = c(-0.1, 0.1,   Inf),
                            labels = c('0', '1+'), include.lowest = TRUE)) %>%
    group_by(carer_desc,  admis_type_desc, events_grp) %>%
    mutate(count_grp = sum(count),
           perc_grp = sum(percentage)) %>% 
    select(carer_desc, admis_type_desc, total_identified, events_grp, count_grp, perc_grp) %>% 
    distinct_all() %>% 
    mutate(identifier = carer_desc) # create additional column for plotting
  
  
  
  return(matched_counts_grp)
}

# function creates percentage of individuals with 0/1+ admissions for each admission type over total cohort for unpaid carers by lagp
inpatients_binsperc_table_lagp <- function(df_lagp, la_name){

  matched_counts_lagp <- df_lagp %>%
    ungroup() %>%
    # unpaid carers only
    filter(carer_desc == "Unpaid carers") %>% 
    mutate(events_grp = cut(total_pedw_events_lagp,
                            breaks = c(-0.1, 0.1,  Inf),
                            labels = c('0', '1+'), include.lowest = TRUE)) %>%
    group_by(carer_desc, first_identified_by, admis_type_desc, events_grp) %>%
    mutate(count_grp = sum(count),
           perc_grp = sum(percentage)) %>% 
    select(carer_desc, first_identified_by, admis_type_desc, total_identified_lagp, events_grp, count_grp, perc_grp) %>% 
    distinct_all() %>% 
    mutate(identifier = paste0(first_identified_by, "-identified"),
           identifier = factor(identifier, levels=c('LA-identified', 'GP-identified'))) # create additional column for plotting
  
  return(matched_counts_lagp)
}



# function creates percentage of individuals with hospital admissions over total cohort by carer status or for unpaid carers by lagp 
inpatient_overall_countsperc <- function(carerdesc_df,  if_bylagp , la_name){
  
  df_ls <- list()
  idvar_list <- c("identifier", "total_identified", "events_grp")
  suffix <- ""
  
  # update variable list for lagp dfs
  if(if_bylagp == 1){
    suffix <- "_lagp"
  }
  
  
  
  # rename column in lagp datasets total_identified_lagp to total_identified
  names(carerdesc_df)[grep("_lagp", colnames(carerdesc_df))] <- "total_identified"
  
  # get binned counts individuals without hospitalisation 
  adm_type_counts <- carerdesc_df %>% ungroup() %>% 
    filter(admis_type_desc == "No admission") %>% 
    mutate(Has_hospital_adm_count = total_identified - count_grp) %>% 
    select(identifier, total_identified, events_grp, count_grp, Has_hospital_adm_count) %>%
    distinct_all() 
  
  # by carer type
  adm_type_counts2 <- adm_type_counts %>% 
    # pivot wide to long
    melt(id.vars = idvar_list, variable.name = "count_grp_name", value.name = "count_grp" )
  # rename columns to "no admission" and "has hospital admission(s)"
  adm_type_counts2 <- adm_type_counts2 %>%  
    mutate(events_grp = ifelse(count_grp_name == "Has_hospital_adm_count", "1+","0")) %>% 
    mutate(events_grp = factor(events_grp , levels = c("0","1+") )) %>% 
    mutate(count_grp_name = ifelse(count_grp_name == "count_grp", "No admissions", "Has hospital admission(s)")) %>% 
    rename(grp_name = count_grp_name)
  
  

  
  ## As above, but using percentage
  adm_type_perc <- carerdesc_df %>% ungroup() %>%
    filter(admis_type_desc == "No admission" ) %>% 
    mutate(Has_hospital_adm_perc = 100 - perc_grp)  %>% 
    select(identifier, total_identified, events_grp, perc_grp, Has_hospital_adm_perc) %>%
    distinct_all() 
  
  adm_type_perc2 <- adm_type_perc %>% 
    melt(id.vars = idvar_list, variable.name = "perc_grp_name", value.name = "perc_grp" )
  
  
  adm_type_perc2 <- adm_type_perc2 %>%  
    mutate(events_grp = ifelse(perc_grp_name == "Has_hospital_adm_perc", "1+","0")) %>% 
    mutate(events_grp = factor(events_grp , levels = c("0","1+") )) %>% 
    mutate(perc_grp_name = ifelse(perc_grp_name == "perc_grp", "No admissions", "Has hospital admission(s)")) %>% 
    rename(grp_name = perc_grp_name)
  
  
  
  # merge count and percentage
  merged_df <- merge(adm_type_counts2, adm_type_perc2)
  
  # select only those with hospital admission
  merged_df <- merged_df %>% filter(events_grp == "1+")
  
  # create relevant df name
  fn <- paste0(la_name,'_inpatient_overall_countsperc')
  if(if_bylagp == 1){ fn <- paste0(fn,"_lagp")}
  
 
  # write to excel 
  pedw_path <- ''
  write.xlsx(merged_df, paste0(pedw_path,'data/processed/1429_',fn,'.xlsx'))
  
  return(merged_df)
}


library(maditr)
# function creates percentage of individuals with planned and emergency hospital admissions over total cohort by carer status or for unpaid carers by lagp 
inpatient_admtype_countsperc <- function(carerdesc_df,  if_bylagp , la_name){
  
  la_name <- substr(la_name,1,7) # shortened denbighshire to denbigh for character count limit when exporting df
  
  #initialise  admission type list and empty lists for storing processed count and binned df
  adm_list <- c("Planned","Emergency")
  df_ls <- list()
  
  idvar_list <- c("identifier", "admis_type_desc",  "total_identified", "events_grp")
  suffix <- ""
  
  # update variable list for lagp dfs
  if(if_bylagp == 1){
    idvar_list <- c("identifier", "admis_type_desc", "total_identified", "events_grp") 
    suffix <- "_lagp"
  }
  
  # rename column in lagp datasets total_identified_lagp to total_identified
  names(carerdesc_df)[grep("_lagp", colnames(carerdesc_df))] <- "total_identified"
  
  # for planned and emergency admission types,
  # - 1. filter admission type
  # - 2. count number of individuals without type of admission
  # - 3. select required columns in order
  # - 4. convert to long dataframe
  # - 5. create and rename columns to has/no admission type
  for(adm_type in adm_list){
    adm_type_counts <- carerdesc_df %>%
      # - 1. filter admission type
      filter(admis_type_desc == adm_type ) %>% 
      # - 2. count number of individuals without type of admission
      mutate(no_adm_type_count = total_identified - count_grp) %>% 
      ungroup() %>% 
      # - 3. select required columns in order
      select(identifier, admis_type_desc, total_identified, events_grp, count_grp, no_adm_type_count) %>%
      distinct_all() 
    
    # - 4. convert to long dataframe
    adm_type_counts2 <- adm_type_counts %>% 
      melt(id.vars = idvar_list, variable.name = "count_grp_name", value.name = "count_grp" )
    
    
    adm_type_counts2 <- adm_type_counts2 %>%  
      # - 5. create and rename columns to has/no admission type
      mutate(events_grp = ifelse(count_grp_name == "no_adm_type_count", "0","1+")) %>% 
      mutate(events_grp = factor(events_grp , levels = c("0","1+") )) %>% 
      mutate(count_grp_name = ifelse(count_grp_name == "count_grp", paste0("Has ", adm_type, " admission(s)"), paste0("No ", adm_type, " admissions"))) %>% 
      rename(grp_name = count_grp_name)

    
    # repeat steps 1-5 above but calculating percentage
    adm_type_perc <- carerdesc_df %>% ungroup() %>%
      filter(admis_type_desc == adm_type ) %>% 
      mutate(no_adm_type_perc = 100 - perc_grp)  %>% 
      ungroup() %>% 
      select(identifier, admis_type_desc, total_identified, events_grp, perc_grp, no_adm_type_perc) %>%
      distinct_all() 
    
    adm_type_perc2 <- adm_type_perc %>% 
      melt(id.vars = idvar_list, variable.name = "perc_grp_name", value.name = "perc_grp" )
    
    
    adm_type_perc2 <- adm_type_perc2 %>%  
      mutate(events_grp = ifelse(perc_grp_name == "no_adm_type_perc", "0","1+")) %>% 
      mutate(events_grp = factor(events_grp , levels = c("0","1+") )) %>% 
      mutate(perc_grp_name = ifelse(perc_grp_name == "perc_grp", paste0("Has ", adm_type, " admission(s)"), paste0("No ", adm_type, " admissions"))) %>% 
      rename(grp_name = perc_grp_name)
    
    fn <- paste0(la_name,"_",adm_type, "_adm_perc")
    if(if_bylagp == 1){ fn <- paste0(fn,"_lagp")}
    
    
    # merge counts and percentages
    merged_df <- merge(adm_type_counts2, adm_type_perc2)
    
    # select only 1+
    merged_df <- merged_df %>% filter(events_grp == "1+")
    
    # create relevant df name
    fn <- paste0(la_name,"_", adm_type, "_adm_cnt")
    if(if_bylagp == 1){ fn <- paste0(fn,"_lagp")}
    
    df_ls[[fn]] <- merged_df # place dataframes into list to be shown as environmental variables outside for-loop
    
  }
  
  
  # create relevant df name
  fn <- paste0(la_name,'_inpatient_admistype_countsperc')
  if(if_bylagp == 1){ fn <- paste0(fn,"_lagp")}
  
  
  # write to excel book with multiple sheets
  pedw_path <- ''
  write.xlsx(df_ls, paste0(pedw_path,'data/processed/1429_',fn,'_peje.xlsx'))
  
  # split list output into environment variables
  list2env(df_ls, envir = .GlobalEnv)
 
}



# function creates count and percentage plots of individuals with hospitalisation (overall) and by admission type (planned or emergency)
# by carer status and for LA and GP unpaid carers  
inpatient_bins_plot <- function(plot_df, la_name, adm_type ,if_count, if_bylagp){
  y_lab <- ifelse(if_count==1,"Count", "Percentage (%)")
  plot_type <- ifelse(if_count==1,"dodge", "stack")

  
  if(if_count == 1){
    # dodged plots for count plots
     
    plot_title <- paste0(la_name," count of individuals with ",adm_type," hospitalisations by carer type" )   
    plot_palette <- carertype_palette
    legend_title <- "Cohort"
    
    if(if_bylagp == 1){
      ## perc by carer flag + lagp
      plot_title <- paste0(la_name ," count of individuals with ", adm_type, " hospitalisations by carer type identified by LA and GP")
      plot_palette <- lagpcarers_palette
      legend_title <- "Unpaid carers cohort"
      
    }
    
    out_plot <- plot_df %>% 
      ggplot() +
      scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
      geom_col(aes(x = events_grp, 
                   y = count_grp,
                   fill = identifier), 
               position = plot_type) +
      geom_text(aes(x = events_grp,  
                    y = count_grp, 
                    label = count_grp, 
                    group = events_grp), 
                position= position_dodge(width = 1), 
                vjust=-.25,
                size = 3.5) 
    
    out_plot <- out_plot + 
      labs(
        title = plot_title,
        x = legend_title,
        y = y_lab
      ) + facet_wrap(~ identifier, ncol = 2)
    
    
  }else {
    
    # stacked plots for percentage 
    plot_title <- paste0(la_name," percentage of individuals with ",adm_type, "hospitalisations by carer type" )
    plot_palette <- carertype_palette
    legend_title <- "Cohort"
    
    if(if_bylagp == 1){
      ## perc by carer flag + lagp
      plot_title <- paste0(la_name ," percentage of individuals with ", adm_type, " hospitalisations by carer type identified by LA and GP")
      plot_palette <- lagpcarer_palette
      legend_title <- "Unpaid carers cohort"
    }
    
    
    out_plot <- plot_df  %>% ggplot() + 
      # remove margin between bar plots and x-axis
      scale_y_continuous(expand = expansion(mult = c(0,0.05))) + 
      geom_col(aes(x = identifier, 
                   y = perc_grp,
                   fill = identifier), position = plot_type) +
      labs(
        title = plot_title,
        x = legend_title,
        y = y_lab
      ) + 
      geom_text(aes(x = identifier,  
                    y = perc_grp, 
                    label = sprintf("%.1f", perc_grp),
                    group = identifier), 
                position= position_dodge(width = 1), 
                vjust=-.25,
                size = 3.5)

    
  }
  
  out_plot <- out_plot +
    scale_fill_manual(values = plot_palette, 
                      guide = guide_legend(reverse = FALSE, title = legend_title))+ 
    theme(
    panel.background = element_rect(fill = "white", color = "grey50"),
    strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"),
    axis.line = element_line(colour = "black"),
    legend.position = "none")
  
  return(out_plot)
}



# function creates tables by carer status and lagp unpaid carers to create health service use timeline 
inpatient_timeline_tables <- function(df, if_lagp){
  # select required columns
  df1 <- df %>% 
    select(alf_pe, carer_desc, first_identified_by, first_identified_date, adm_time_grp,  total_identified, total_identified_lagp)
  
  
  
  #counts and perc 
  # - by carer status (wouldnt add up to 100 as individuals multiple hospital admissions in the year prior to their index dates)
  df2 <- df1 %>%
    group_by(carer_desc, total_identified, adm_time_grp) %>% 
    # count number of alfs with admission in each time group
    summarise(count_grp = n())%>% 
    # calculate count as a percentage of the total cohort
    mutate(perc_grp = calc_percentage(count_grp, total_identified)) %>% 
    select(carer_desc, total_identified, adm_time_grp, count_grp, perc_grp) %>% 
    distinct_all() %>% ungroup() %>% 
    mutate(identifier = carer_desc) # create additional column for plotting
  
  # - by lagp unpaid carers
  if(if_lagp == 1){
    
    df2 <- df1 %>%
      filter(carer_desc == "Unpaid carers") %>% 
      group_by(carer_desc, first_identified_by, total_identified_lagp, adm_time_grp) %>% 
      summarise(count_grp = n())%>% 
      mutate(perc_grp = calc_percentage(count_grp, total_identified_lagp)) %>% 
      select(carer_desc,first_identified_by, total_identified_lagp, adm_time_grp, count_grp, perc_grp) %>% 
      distinct_all() %>% ungroup() %>% 
      mutate(identifier = paste0(first_identified_by, "-identified"),
             identifier = factor(identifier, levels=c('LA-identified', 'GP-identified')))
    
  }
  
  #remove those without admissions
  df2 <- df2 %>%  filter(!is.na(adm_time_grp))
  
  return(df2)
  
}

# function create health service timeline using output from inpatient_timeline_tables
inpatient_timeline_perc_plots <- function(df, la_name, if_lagp, adm_name){
  library(lubridate)
  #pre-define labels and titles
  y_lab <- "Percentage (%)"
  x_lab <- "Month groups prior to index date"
  plot_type <- "dodge"

  adm_name <- ifelse(adm_name== "", adm_name, paste0(adm_name, " "))
  plot_title <- paste0(la_name, " percentage of individuals with ", adm_name," hospitalisations in months prior to \nindex assessment date by carer type" )
  plot_palette <- carertype_palette
  legend_title <- "Cohort"
  
  if(if_lagp == 1){
    ## perc by carer flag + lagp
    plot_title <- paste0(la_name, " percentage of individuals with ", adm_name," hospitalisations in months prior to \nindex assessment date by unpaid carers identified by LA and GP" )
    plot_palette <- lagpcarers_palette
    legend_title <- "Unpaid carers cohort"
    df <- df %>% mutate(identifier = factor(identifier, levels = c("LA-identified", "GP-identified")))
    
  } else{
    df <- df %>% mutate(identifier = factor(identifier, levels = c("Unpaid carers", "Non-carers")))
  }
  
  # add new rule - if count_grp is "*" (i.e masked value), use * as label, otherwise perc_grp as label
  df <- df %>% mutate(ylabels = ifelse(count_grp == "*", "*", sprintf("%.1f", perc_grp)),
                      # order to adm_time_grp
                      adm_time_grp = factor(adm_time_grp,levels = c('12-10 months', '9-7 months','6-4 months', '3-0 months'  ))
                      )
  
  # set x,y and fill that applies to both carer desc and carer desc + lagp plots
  df_plot <- df  %>% ggplot() + 
    scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
    geom_col(aes(x = adm_time_grp, 
                 y = perc_grp,
                 fill = identifier), 
             position = plot_type) +
    scale_fill_manual(values = plot_palette, 
                      guide = guide_legend(reverse = FALSE, title = legend_title)) 
  
  df_plot <- df_plot + 
    labs(
      title = plot_title,
      x = x_lab,
      y = y_lab
    ) + 
    geom_text(aes(x = adm_time_grp,  
                  y = perc_grp, 
                  label = ylabels,
                  group = identifier), 
              position= position_dodge(width = 1), 
              vjust=-.25,
              size = 3.5)
  
  # add theme
  df_plot <- df_plot + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             axis.line = element_line(colour = "black"),
                             panel.background = element_rect(fill = "white", color = "grey50"),
                             strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"),
                             )
  
  
  return(df_plot)
  
}

# function creates template dataframe with all combinations of unpaid carer type, sex, age group and admission type.
# Used in direct standardisation function (inpatient_standardisation)
inpatient_create_dummytbl <- function(){
  # create values for stratifying variable
  first_identified_by <- c("GP", "LA")
  sex <- c(1, 2)
  age_group <- c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")
  admis_type_desc <- c("Has hospitalisation", "Planned", "Emergency")
  
  # create dummy table containing all combinations of stratifying variables by admis_type
  dummy <- as.data.frame(expand.grid(first_identified_by, sex, age_group, admis_type_desc))
  names(dummy) <- c( "first_identified_by", "sex", "age_group", "admis_type_desc") # rename columns
  
  

  return(dummy)
}


library(PHEindicatormethods)
# function prepares required dataframes and runs the direct standardisation function using the prepared dfs.
inpatient_standardisation <- function(carers_lagp, agesex_totals, la_name, df_standardpop_lkup){
  # create dummy table containing all combinations of stratifying variables by admis_type
  dummy <- inpatient_create_dummytbl()
  
  
  # join totals table to dummy table to get one row per admis type by stratifying variables
  admistotals_dummy <-  left_join(dummy, agesex_totals, by=c("first_identified_by", "sex", "age_group")) 
  
  admistotals_dummy$name <- la_name # create name variable for joining to stnd pop
  
  df_standardpop_lkup <- df_standardpop_lkup[df_standardpop_lkup$name == la_name,]
  # join admistotals_dummy (dummy table with study pop totals) to standard population totals. This provides all the strata needed.
  populations <- left_join(df_standardpop_lkup, admistotals_dummy, by=c("age" = "age_group", "sex_code" = "sex", "name" = "name")) 
  
  
  # join populations to carers_lagp df with admis_type
  dsr_input <- left_join(populations, carers_lagp, by=c("first_identified_by", "admis_type_desc", "age" = "age_group", "sex_code" = "sex"))
  
  # replace NA count group values with 0s
  dsr_input$count_grp[is.na(dsr_input$count_grp)] <- 0 
  
  # save age sex counts in excel
  pedw_path <- ''
  initials <- ''
  outfn <- paste0(pedw_path,'data/processed/1429_pedw_',la_name,'_agesex_counts.xlsx')
  xlsx::write.xlsx(data.frame(dsr_input), file=outfn, sheetName = "lagp", row.names = FALSE)
  
  
  # Using PHEindicatormethods package
  # x = numerator (stratified counts); n= denominator (statified totals in df); stdpop = standard population counts; 
  # stdpoptype = "field" indicates that standard population data is part of input data, otherwise it is "vector"(self-explanatory); 
  # multiplier = per how many (i,e put 1000 for per 1000)
  std <- dsr_input %>%
    group_by(first_identified_by, admis_type_desc) %>%
    phe_dsr(
      x = count_grp,
      n = total_identified_agesex,
      stdpop = pop,
      stdpoptype = "field",
      multiplier = rate_multiplier
    )
  
  return(std)
}



# function formats output of inpatient_standardisation function into desired format.
inpatient_reformat_std_table <- function(std_df){
  # select needed cols
  sub_std <- std_df %>% select(total_count, total_pop, first_identified_by, admis_type_desc, value, lowercl, uppercl)
  
  # calculate crude prevalence rate
  sub_std <- sub_std %>%
    mutate(crude_prevrate = total_count/total_pop*rate_multiplier) %>%
    select(-total_count, -total_pop) %>% ungroup()
  
  
  # format crude rate table to have crude rates of unpaid carer and non-carers side in a wide table rather than long
  wider_df <- tidyr::pivot_wider(sub_std,
                                 id_cols = admis_type_desc,
                                 names_from = first_identified_by,
                                 values_from = c("crude_prevrate", "value", "lowercl", "uppercl"))
  
  
  
  # calculate la vs gp unpaid carers rate ratio and corresponding lower and upper confidence levels
  wider_df2 <- wider_df %>% 
    mutate(lavsgp_rateratio = value_LA/value_GP,
           lavsgp_lowercl = lowercl_LA/uppercl_GP,
           lavsgp_uppercl = uppercl_LA/lowercl_GP) %>% 
    # round all to 3dp
    mutate_if(is.numeric, ~round(., 3)) %>% 
    as.data.frame() %>% 
    # specify order of columns
    select(admis_type_desc, crude_prevrate_LA, value_LA, lowercl_LA, uppercl_LA, 
           crude_prevrate_GP, value_GP, lowercl_GP, uppercl_GP, 
           lavsgp_rateratio, lavsgp_lowercl, lavsgp_uppercl)
   
  return(wider_df2)
  
}



library(stringr)
# function calculates crude rates by carer status and lagp unpaid carers for hospitalisations (overall) and by admission type. 
inpatient_calc_cruderate <- function(la_df, by_admistype, by_carertype){
  
  la_df <- la_df %>% 
    # create generic column to indicate if alf has hospitalisation 
    mutate("Has inpatient admission" = ifelse(admis_type_desc == "No admission", "No" , "Yes")) %>% 
    #rename column carer_desc as Cater Type for chisq output
    mutate("Carer Type" = ifelse(carer_flag == 0, "non_carers", "unpaid_carers")) 
  

  if(by_carertype == 0){
    # filter by first_identified_by value
    la_df <- la_df %>% 
      filter(carer_flag == 1) %>% 
      mutate(total_identified = total_identified_lagp,
             "Carer Type" = first_identified_by)
  }
  
  # planned/emergency admissions
  if(by_admistype == 1){
    
    cols <- c("admis_type_desc",
              "total_identified_unpaid_carers", "count_unpaid_carers", "crude_rate_unpaid_carers", "lowercl_unpaid_carers", "uppercl_unpaid_carers",           
              "total_identified_non_carers", "count_non_carers", "crude_rate_non_carers", "lowercl_non_carers", "uppercl_non_carers",
              "ratio", "ratio_lowercl", "ratio_uppercl" )
    
    if(by_carertype == 0){
      # update columns to select after crude rate calculation
      cols <- str_replace_all(cols, "unpaid_carers", "LA")
      cols <- str_replace_all(cols, "non_carers", "GP")
    }
    
    # by admission type
    cruderate_df <- la_df  %>% 
      # select only those with either emergency/planned admissions
      filter(`Has inpatient admission` == "Yes") %>% 

      # select needed columns
      select(alf_pe, `Carer Type`,  total_identified, admis_type_desc, `Has inpatient admission`) %>% 
      # distinct to one row per alf
      distinct_all() %>%
      # group variables for summarising counts
      group_by(`Carer Type`, total_identified, admis_type_desc, `Has inpatient admission`) %>% 
      summarise(count = n()) %>% 
      # calculate crude rate
      mutate(crude_rate = count/total_identified*rate_multiplier,
             lowercl = (rate_multiplier/total_identified) * (count - (1.96 * sqrt(count))),
             uppercl = (rate_multiplier/total_identified) * (count + (1.96 * sqrt(count))))
    
    # format crude rate table to have crude rates of unpaid carer and non-carers side in a wide table rather than long
    wider_cruderate <- tidyr::pivot_wider(cruderate_df,
                                          id_cols = admis_type_desc,
                                          names_from =`Carer Type`,
                                          values_from = c("total_identified", "count", "crude_rate", "lowercl", "uppercl")) %>%  
      as.data.frame()
    
    # ratio and upper/lower confidence level calculations 
    wider_cruderate$ratio <- wider_cruderate[,7]/wider_cruderate[,6]
    wider_cruderate$ratio_lowercl <- wider_cruderate[,9]/wider_cruderate[,10]
    wider_cruderate$ratio_uppercl <- wider_cruderate[,11]/wider_cruderate[,8]
    
    
    wider_cruderate <- wider_cruderate %>% 
      # round to 3dp
      mutate_if(is.numeric, ~round(., 3)) %>% 
      # select columns at wanted order
      select(cols)
    
    
  
    
    
  } else{
    # overall admissions
    cols <- c("Has inpatient admission",
              "total_identified_unpaid_carers", "count_unpaid_carers", "crude_rate_unpaid_carers", "lowercl_unpaid_carers", "uppercl_unpaid_carers",           
              "total_identified_non_carers", "count_non_carers", "crude_rate_non_carers", "lowercl_non_carers", "uppercl_non_carers",
              "ratio", "ratio_lowercl", "ratio_uppercl" )
    
    if(by_carertype == 0){
      # update columns to select after crude rate calculation
      cols <- str_replace_all(cols, "unpaid_carers", "LA")
      cols <- str_replace_all(cols, "non_carers", "GP")
    }
    
    
    # overall
    cruderate_df <- la_df  %>% 
      # select needed columns
      select(alf_pe, `Carer Type`,  total_identified, `Has inpatient admission`) %>% 
      # distinct to one row per alf
      distinct_all() %>%
      # group variables for summarising counts
      group_by(`Carer Type`, total_identified, `Has inpatient admission`) %>% 
      summarise(count = n()) %>% 
      # calculate crude rate
      mutate(crude_rate = round((count/total_identified*rate_multiplier), digits = 3),
             lowercl = (rate_multiplier/total_identified) * (count - (1.96 * sqrt(count))),
             uppercl = (rate_multiplier/total_identified) * (count + (1.96 * sqrt(count))))
    
    # format crude rate table to have crude rates of unpaid carer and non-carers side in a wide table rather than long
    wider_cruderate <- tidyr::pivot_wider(cruderate_df,
                                          id_cols = `Has inpatient admission`,
                                          names_from =`Carer Type`,
                                          values_from = c("total_identified", "count", "crude_rate", "lowercl", "uppercl")) %>% 
      as.data.frame()
    
    
    # ratio and upper/lower confidence level calculations
    wider_cruderate$ratio <- wider_cruderate[,7]/wider_cruderate[,6]
    wider_cruderate$ratio_lowercl <- wider_cruderate[,9]/wider_cruderate[,10]
    wider_cruderate$ratio_uppercl <- wider_cruderate[,11]/wider_cruderate[,8]
    
    wider_cruderate <- wider_cruderate %>% 
      # round to 3dp
      mutate_if(is.numeric, ~round(., 3)) %>% 
      # select columns at desired order
      select(cols)
    
    
  }
   
  return(wider_cruderate)
  
  
}


