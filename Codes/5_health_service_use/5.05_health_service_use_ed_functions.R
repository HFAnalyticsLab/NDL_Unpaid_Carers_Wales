# Functions for analysis of Emergency department (ED) attendances 
rate_multiplier <- 1000 # predefine rate (i.e per x population)

# function creates standardised dataframe to be used by other script functions.
edatt_dataprep <- function(matched_df){
  
  
  # change column names to lower case
  names(matched_df) <- tolower(names(matched_df))
  
  # Add age group column
  matched_df1 <- matched_df %>%
    mutate(age_group = cut(age,
                           breaks = c(17,39, 49, 59, 69, 79, Inf),
                           labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))
  
  # Add carer description
  matched_df1 <- matched_df1 %>% mutate(carer_desc = ifelse(carer_flag == 1,'Unpaid carers', 'Non-carers'),
                                        carer_desc = factor(carer_desc, levels = c('Unpaid carers', 'Non-carers')))
  
  

  # Add column with total carer type
  df_ed_tot_id <- matched_df1 %>%
    group_by(carer_desc) %>%
    summarise(total_identified = n_distinct(alf_pe)) %>%
    ungroup()
  
  
  # Add column with total carer type and GP/LA identified
  df_ed_tot_id_lagp <- matched_df1 %>%
    group_by(carer_flag, first_identified_by) %>%
    summarise(total_identified_lagp = n_distinct(alf_pe)) %>%
    ungroup()
  
  # by timeline 3m/6m/1yr for health service use timeline
  timeline <- matched_df1 %>% 
    select(alf_pe, admin_arr_dt, first_identified_date) %>% 
    # select only those with ed attendance
    filter(!is.na(admin_arr_dt)) %>% 
    # create time intervals leading to index date
    mutate(adm_time_period =interval( admin_arr_dt, first_identified_date) %/% months(1),
         adm_time_grp = case_when(
           adm_time_period >=0 & adm_time_period <= 3 ~ '3-0 months',
           adm_time_period >= 4 & adm_time_period <= 6 ~ '6-4 months',
           adm_time_period >=7 & adm_time_period <= 9 ~ '9-7 months',
           adm_time_period > 9   ~ '12-10 months'
           
         ), 
         # reorder groups
         adm_time_grp = factor(adm_time_grp,levels = c('12-10 months', '9-7 months','6-4 months', '3-0 months'  )))
  
  # Add these totals to the main df
  matched_df2 <- matched_df1 %>%
    merge(df_ed_tot_id, all = TRUE)
  
  matched_df2 <- matched_df2 %>%
    merge(df_ed_tot_id_lagp, all = TRUE)
  
  matched_df2 <- matched_df2 %>%
    merge(timeline, all = TRUE)
  
  return(matched_df2)
  
}



# function creates counts and percentages for those with  ED attendances out of total cohort by carer status
edatt_countperc_table <-function(cleaned_df){
  matched_ed_counts <- cleaned_df %>%
    # Flag rows with a ed a event with a 1, no ed event as a 0
    mutate(ed_event_flag = ifelse(!is.na(admin_arr_dt), 1, 0)) %>%
    
    # group by  identification cohort, count of total identified, admission type and alf to count number of each admission type per cohort per person.
    group_by( carer_desc,total_identified, alf_pe) %>%
    summarise(total_ed_events = sum(ed_event_flag)) %>% 
    
    # group by identification cohort, count of total identified, admission type and number of admissions to count number of carers per number of admissions by admission type
    group_by( carer_desc, total_identified, total_ed_events) %>%
    summarise(count = n()) %>%
    mutate(percentage = calc_percentage(count, total_identified)) 
  
  # View(matched_ed_counts)
  
  return(matched_ed_counts)
  
}

# function has multiple outputs:
# 1. counts and percentages of those with and without ED attendances out of total cohort by lagp
# 2. counts of those with and without ED attendances out of total cohort by lagp, age and sex for standardisation function
# 3. counts by age and sex for lagp unpaid carers to be used as denominator for standardisation function
edatt_countperc_table_lagp <- function(cleaned_df, la_name){
  
  # 1. total counts and percentage by lagp
  matched_edatt_counts_lagp <- cleaned_df %>%
    # Flag rows with a ed a event with a 1, no ed event as a 0
    mutate(ed_event_flag = ifelse(!is.na(admin_arr_dt), 1, 0)) %>%
    
    # group by carer_flag, identification cohort, count of total identified, admission type and alf to count number of each admission type per cohort per person.
    group_by( carer_desc,first_identified_by, total_identified_lagp, alf_pe) %>%
    summarise(total_ed_events_lagp = sum(ed_event_flag)) %>% 
    
    # group by identification cohort, count of total identified, admission type and number of admissions to count number of carers per number of admissions by admission type
    group_by(carer_desc, first_identified_by, total_identified_lagp, total_ed_events_lagp) %>%
    summarise(count = n()) %>%
    mutate(percentage = calc_percentage(count, total_identified_lagp)) 
  
  
  # 2. counts by age sex for carers by lagp
  agesex_edatt_counts_overall_lagp <- cleaned_df %>% 
    #only unpaid carers are used in direct standardisation
    filter(carer_flag ==1) %>% 
    # Flag rows with admin_arr_dt with a 1, no admin_arr_dt  as a 0
    mutate(ed_event_flag = ifelse(!is.na(admin_arr_dt), 1, 0)) %>%

    # group by carer_flag, identification cohort, count of total identified, admission type and alf to count number of each admission type per cohort per person.
    group_by( first_identified_by,  age_group, sex, alf_pe) %>%
    summarise(total_ed_events_lagp = sum(ed_event_flag)) %>% 
    
    # group by identification cohort, count of total identified, admission type and number of admissions to count number of carers per number of admissions by admission type
    group_by( first_identified_by, age_group, sex,  total_ed_events_lagp) %>%
    summarise(count = n()) %>% 
    
    # split into 0/1 bins
    mutate(events_grp = cut(total_ed_events_lagp,
                            breaks = c(-0.1, 0.1,  Inf),
                            labels = c('0', '1+'), include.lowest = TRUE)) %>%
    group_by( first_identified_by, age_group, sex, events_grp) %>%
    mutate(count_grp = sum(count)) %>% 
    ungroup() %>% 
    filter(events_grp == '1+') %>% 
    select( first_identified_by, age_group, sex, count_grp) %>% 
    distinct_all() 


  
  
  
  # 3. calculate total unique individuals by age and sex for direct standardisation denominator
  agesex_edatt_counts_denom <- cleaned_df %>% 
    filter(carer_flag ==1) %>% 
    group_by(first_identified_by, age_group, sex) %>%
    summarise(total_identified_agesex = n_distinct(alf_pe)) %>%
    ungroup()
  
  
  # save dataframes in a list and make them available in environment
  countperc_list <- list()
  countperc_list[[paste0(la_name, "_edatt_lagp_counts")]] <- matched_edatt_counts_lagp
  countperc_list[[paste0(la_name, "_agesex_edatt_counts_hf")]] <- agesex_edatt_counts_overall_lagp
  countperc_list[[paste0(la_name, "_agesex_edatt_totals_hf")]] <- agesex_edatt_counts_denom
  
  list2env(countperc_list, envir = .GlobalEnv)
  
  
}

# function get total count and percectage of ED attendance bins by carer status
edatt_binsperc_table <- function(df){
  
  matched_counts_grp <- df %>%
    ungroup() %>%
    mutate(events_grp = cut(total_ed_events,
                            breaks = c(-0.1, 0.1,  Inf),
                            labels = c('0', '1+'), include.lowest = TRUE)) %>%
    group_by(carer_desc, events_grp) %>%
    mutate(count_grp = sum(count),
           perc_grp = sum(percentage)) %>% 
    select(carer_desc, total_identified, events_grp, count_grp, perc_grp) %>% 
    distinct_all() %>% 
    # create new column
    mutate(identifier = carer_desc) 
  
  
  return(matched_counts_grp)
}

# function get total count and percectage of ED attendance bins by lagp
edatt_binsperc_table_lagp <- function(df){

  
  matched_counts_lagp <- df %>%
    # unpaid carers only
    filter(carer_desc == "Unpaid carers") %>% 
    ungroup() %>%
    mutate(events_grp = cut(total_ed_events_lagp,
                            breaks = c(-0.1, 0.1,  Inf),
                            labels = c('0', '1+'), include.lowest = TRUE)) %>%
    group_by(carer_desc, first_identified_by, events_grp) %>%
    mutate(count_grp = sum(count),
           perc_grp = sum(percentage)) %>% 
    select(carer_desc, first_identified_by, total_identified_lagp, events_grp, count_grp, perc_grp) %>% 
    distinct_all() %>% 
    # create new column and order levels
    mutate(identifier = paste0(first_identified_by, "-identified"),
           identifier = factor(identifier, levels = c("LA-identified", "GP-identified")))
  
  return(matched_counts_lagp)
  
}


# function creates ED attendance a table with counts of individuals with an attendance as a proportion of the total cohort
# in each 3 month period (adm_time_grp) prior to the index assessment date. This is done by carer status and lagp. 
# Output of the function is used to for creating corresponding visual.
edatt_timeline_tables <- function(df, if_lagp){
  
  df1 <- df %>% 
    select(alf_pe, carer_desc, first_identified_by, first_identified_date, adm_time_grp,  total_identified, total_identified_lagp)
  

  
  # counts and perc (by carer desc)
  # wouldnt add up to 100 as each alf can have multiple ED admissions in the year prior to their index dates
  df2 <- df1 %>%
    group_by(carer_desc, total_identified, adm_time_grp) %>% 
    summarise(count_grp = n())%>% 
    mutate(perc_grp = calc_percentage(count_grp, total_identified)) %>% 
    select(carer_desc, total_identified, adm_time_grp, count_grp, perc_grp) %>% 
    distinct_all() %>% ungroup() %>% 
    mutate(identifier = carer_desc) # create additional column for plotting
  
  # by carer desc and lagp
  if(if_lagp == 1){
    # wouldnt add up to 100 as each alf can have multiple ED admissions in the year prior to their index dates
    df2 <- df1 %>%
      # unpaid carers only
      filter(carer_desc == "Unpaid carers") %>% 
        group_by(carer_desc, first_identified_by, total_identified_lagp, adm_time_grp) %>% 
        summarise(count_grp = n())%>% 
        mutate(perc_grp = calc_percentage(count_grp, total_identified_lagp)) %>% 
        select(carer_desc,first_identified_by, total_identified_lagp, adm_time_grp, count_grp, perc_grp) %>% 
        distinct_all() %>% ungroup() %>% 
      mutate(identifier = paste0(first_identified_by, "-identified"),
             identifier = factor(identifier, levels = c("LA-identified", "GP-identified")))
    
  }
  

  
  return(df2)
  
}

# function creates bar plots of counts and percentages (depending on if_count flag value) of cohorts with an ED attendance for each LA
# la_name specifies the LA eg "Swansea; 
# if_count specifies if the plot type is using counts (if_count = 1) or percentages (if_count = 0);
# if_byLAGP determines comparison cohort type. if_byLAGP = 1 compares by carer status, if_byLAGP = 0 compares la vs gp unpaid carers. 
edatt_bins_plot <- function(plot_df, la_name, if_count, if_bylagp){
  # initialise plot variables
  y_lab <- ifelse(if_count==1,"Count", "Percentage (%)")
  plot_type <- ifelse(if_count==1,"dodge", "stack")
  
  # set default color palette to be carer type palette
  plot_palette <- carertype_palette
  
  # if if_bylagp flag set to 1, update  plot title variable and color palette to be lagp carers palette
  if(if_bylagp == 1) {
    
    plot_title <- paste0(la_name, " percentage of individuals with ED Department attendances for unpaid carers identified by LA and GP" )
    plot_palette <- lagpcarers_palette 
  } 
  
  
  if(if_count == 1){
    # dodged plots for count plots
    
    # common elements for count plot
    out_plot <- plot_df %>% 
      ggplot() +
      scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
      geom_col(aes(x = events_grp, 
                   y = count_grp,
                   fill = events_grp), position = plot_type) +
      geom_text(aes(x = events_grp,  
                    y = count_grp, 
                    label = count_grp, 
                    group = events_grp), 
                position= position_dodge(width = 1), 
                vjust=-.25,
                size = 3.5) 
    
    if(if_bylagp == 1){
      ## count by carer flag + lagp
      out_plot <- out_plot + 
        scale_fill_manual(values = plot_palette, guide = guide_legend(reverse = FALSE, title = "Total Attendances"))  + 
        labs(
        title = paste0("ED Department attendances by carer type ",la_name , " GP vs LA"),
        x = "",
        y = y_lab
      ) + facet_grid(vars(carer_desc), vars(first_identified_by)) 
        
      
    } else{
      ## count by carer type
      out_plot <- out_plot + 
        scale_fill_manual(values = plot_palette, guide = guide_legend(reverse = FALSE, title = "Total Attendances"))  + 
        labs(
          title = paste0("ED Department attendances in ", la_name , " unpaid carers"),
          x = "",
          y = y_lab
        ) + facet_wrap(~ carer_desc, ncol = 2)
      
    }
    
  }else {
    # stacked plots for percentage 
 
    plot_title <- paste0(la_name, " percentage of individuals with ED Department attendances by carer type" )
    
    
    
    out_plot <- plot_df  %>%
      # show only 1+ percentages
      filter(events_grp == "1+") %>% 
      ggplot() + 
      # remove gap between bar plot and x-axis
      scale_y_continuous(expand = expansion(mult = c(0,0.05))) 
    

    out_plot <- out_plot + 
      geom_col(aes(x = identifier, 
                   y = perc_grp,
                   fill = identifier), position = plot_type) +
      scale_fill_manual(values = plot_palette, guide = guide_legend(reverse = FALSE))+ 
      labs(
        title = plot_title,
        x = "",
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
  
  # basic theme for plot
  out_plot <- out_plot + theme(
    panel.background = element_rect(fill = "white", color = "grey50"),
    strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"),
    axis.line = element_line(colour = "black"),
    legend.position = "none")
  
  return(out_plot)
}

# function creates timeline plots by cohort type using output from edatt_timeline_tables function
edatt_timeline_perc_plots <- function(df, la_name, if_lagp){
  library(lubridate)
  #pre-define labels and titles
  y_lab <- "Percentage (%)"
  x_lab <- "Month groups prior to index date"
  plot_type <- "dodge"
  
 
  plot_title <- paste0(la_name, " Percentage of individuals with ED Department attendances in months prior to \nindex assessment date by carer type" )
  plot_palette <- carertype_palette
  legend_title <- "Cohort"
    
    
  if(if_lagp == 1) {
    
    plot_title <- paste0(la_name, " Percentage of individuals with ED Department attendances in months prior to \nindex assessment date unpaid carers identified by LA and GP" )
    plot_palette <- lagpcarers_palette
    legend_title <- "Unpaid carers cohort"
    df <- df %>% mutate(identifier = factor(identifier, levels = c("LA-identified", "GP-identified")))
  } 
  
  # set order of x-axis (month groups)
  df <- df %>% mutate(adm_time_grp = factor(adm_time_grp,levels = c('12-10 months', '9-7 months','6-4 months', '3-0 months'  )))
  
  # set x,y and fill that applies to both carer desc and carer desc + lagp plots
  df_plot <- df  %>% 
    # filter out those who did not have ed att
    filter(!is.na(adm_time_grp)) %>% 
    ggplot() + 
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
                  label = sprintf("%.1f", perc_grp),
                  group = identifier), 
              position= position_dodge(width = 1), 
              vjust=-.25,
              size = 3.5)
  
  
  # add theme
  df_plot <- df_plot + theme(panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             axis.line = element_line(colour = "black"),
                             panel.background = element_rect(fill = "white", color = "grey50"),
                             strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black"))
  
  
  return(df_plot)
    
}




# function creates a dataframe template containing combinations of all variables
# to be used to merge with the age-sex counts(numerator) and denominator 
edatt_create_dummytbl <- function(){
  # create values for stratifying variable
  first_identified_by <- c("GP", "LA")
  sex <- c(1, 2)
  age_group <- c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")
  
  
  # create dummy table containing all combinations of stratifying variables 
  dummy <- as.data.frame(expand.grid(first_identified_by, sex, age_group))
  names(dummy) <- c( "first_identified_by", "sex", "age_group") # rename columns
  
  
  return(dummy)
}

# function takes in parameters required for calculating age-sex standardised rates using the PHEindicatormethods package
# for ED attendance. It returns the standardised rate.
edatt_standardisation <- function(agesex_counts, agesex_totals, la_name, df_standardpop_lkup){
  # create dummy table containing all combinations of stratifying variables 
  dummy <- edatt_create_dummytbl()
  
  
  # join totals table to dummy table to get one row per admis type by stratifying variables
  totals_dummy <-  left_join(dummy, agesex_totals, by=c("first_identified_by", "sex", "age_group")) 
  
  totals_dummy$name <- la_name # create name variable for joining to stnd pop
  
  df_standardpop_lkup <- df_standardpop_lkup[df_standardpop_lkup$name == la_name,]
  # join admistotals_dummy (dummy table with study pop totals) to standard population totals. This provides all the strata needed.
  populations <- left_join(df_standardpop_lkup, totals_dummy, by=c("age" = "age_group", "sex_code" = "sex", "name" = "name")) 
  
  
  # join populations to agesex_counts df with admis_type
  dsr_input <- left_join(populations, agesex_counts, by=c("first_identified_by",  "age" = "age_group", "sex_code" = "sex"))
  
  # replace NA count group values with 0s
  dsr_input$count_grp[is.na(dsr_input$count_grp)] <- 0 
  
  initials <- ""# your initials here
  # binned counts and percentages by carer type and lagp
  ed_path <- '/'# your path here
  outfn <- paste0(ed_path,'/1429_ed_',la_name,'_agesex_counts.xlsx')
  xlsx::write.xlsx(data.frame(dsr_input), file=outfn, sheetName = "ed_lagp", row.names = FALSE)
  
  
  # Using PHEindicatormethods package
  # x = numerator (stratified counts); n= denominator (statified totals in df); stdpop = standard population counts; 
  # stdpoptype = "field" indicates that standard population data is part of input data, otherwise it is "vector"(self-explanatory); 
  # multiplier = per how many (i,e put 1000 for per 1000)
  std <- dsr_input %>%
    group_by(first_identified_by) %>%
    phe_dsr(
      x = count_grp,
      n = total_identified_agesex,
      stdpop = pop,
      stdpoptype = "field",
      multiplier = rate_multiplier
    )
  
  return(std)
}


# function takes in output of edatt_standardisation function and reformats it by:
# - removing unneeded columns
# - adding a column for crude prevalence rates 
# - changes dataframe from long to wide
# - adding columns for rate ratio, upper and lower CI and,
# - reordering columns
edatt_reformat_std_table <- function(std_df){
  #remove unneeded cols
  sub_std <- std_df %>% select(total_count, total_pop, first_identified_by,  value, lowercl, uppercl)
  
  sub_std <- sub_std %>%
    mutate(crude_prevrate = total_count/total_pop*rate_multiplier) 
  
  
  # format crude rate table to have crude rates of la and gp in a wide table rather than long
  wider_df <- tidyr::pivot_wider(sub_std,
                                 names_from = first_identified_by,
                                 values_from = c( "total_pop", "total_count", "crude_prevrate", "value", "lowercl", "uppercl"))
  
  
  
  # calculate diff between gp vsla (gpvsla); lavsgp
  wider_df2 <- wider_df %>% 
    mutate(lavsgp_rateratio = value_LA/value_GP,
           lavsgp_lowercl = lowercl_LA/uppercl_GP,
           lavsgp_uppercl = uppercl_LA/lowercl_GP) %>% 
    # round all to 3dp
    mutate_if(is.numeric, ~round(., 3)) %>% 
    as.data.frame() %>% 
    # specify order of columns
    select( total_pop_LA, crude_prevrate_LA, value_LA, lowercl_LA, uppercl_LA, 
           crude_prevrate_GP, value_GP, lowercl_GP, uppercl_GP, 
           lavsgp_rateratio, lavsgp_lowercl, lavsgp_uppercl)
  
  return(wider_df2)
  
}

# function calculates crude rates, ratios with upper and lower confidence levels using the input dataframe (la_df),
# by lagp unpaid carers or by carer status (depending on by_carertype value) 
edatt_calc_cruderate <- function(la_df, by_carertype){
  
  la_df <- la_df %>% 
    # create generic column to indicate if alf has ED attendance 
    mutate(has_ed_dept_attendance = ifelse(is.na(admin_arr_dt), "No" , "Yes")) %>% 
    # rename column carer_desc as carer Type 
    mutate("Carer Type" = ifelse(carer_flag == 0, "non_carers", "unpaid_carers")) 
  
  cnames <- c("has_ed_dept_attendance",
              "total_identified_unpaid_carers", "count_unpaid_carers", "crude_rate_unpaid_carers", "lowercl_unpaid_carers", "uppercl_unpaid_carers",           
              "total_identified_non_carers", "count_non_carers", "crude_rate_non_carers", "lowercl_non_carers", "uppercl_non_carers",
              "ratio", "ratio_lowercl", "ratio_uppercl" )
  
  if(by_carertype == 0){
    # filter by first_identified_by value
    la_df <- la_df %>% 
      filter(carer_flag == 1) %>% 
      mutate(total_identified = total_identified_lagp,
             "Carer Type" = first_identified_by)
    
    cnames <- str_replace_all(cnames, "unpaid_carers", "LA")
    cnames <- str_replace_all(cnames, "non_carers", "GP")
  }
  
  
  
  cruderate_df <- la_df  %>% 
    # select needed columns
    select(alf_pe, `Carer Type`,  total_identified, has_ed_dept_attendance) %>% 
    # distinct to one row per alf
    distinct_all() %>%
    # group variables for summarising counts
    group_by(`Carer Type`, total_identified, has_ed_dept_attendance) %>% 
    summarise(count = n()) %>% 
    # calculate crude rate and lower and upper confidence level
    mutate(crude_rate = round((count/total_identified*rate_multiplier), digits = 3),
           lowercl = (rate_multiplier/total_identified) * (count - (1.96 * sqrt(count))),
           uppercl = (rate_multiplier/total_identified) * (count + (1.96 * sqrt(count))))
  
  # format crude rate table to have crude rates of unpaid carer and non-carers side in a wide table rather than long
  wider_cruderate <- tidyr::pivot_wider(cruderate_df,
                                        id_cols = has_ed_dept_attendance,
                                        names_from =`Carer Type`,
                                        values_from = c("total_identified", "count", "crude_rate", "lowercl", "uppercl")) %>% 
    as.data.frame()
  
  
  # ratio and upper/lower confidence level calculations between comparison cohorts
  wider_cruderate$ratio <- wider_cruderate[,7]/wider_cruderate[,6]
  wider_cruderate$ratio_lowercl <- wider_cruderate[,9]/wider_cruderate[,10]
  wider_cruderate$ratio_uppercl <- wider_cruderate[,11]/wider_cruderate[,8]
  
  
  
  wider_cruderate <- wider_cruderate %>% 
    #round to 3dp
    mutate_if(is.numeric, ~round(., 3)) %>% 
    # select columns at wanted order
    select(cnames) %>% 
    # filter only with ed attendance
    filter(has_ed_dept_attendance == "Yes")
  
  
  return(wider_cruderate)
  
  
}


# chi-sq
library(gtsummary)
library(purrr)

# la_df <- npt_matched_edatt_raw2
# by_carertype = 1 
edatt_chisq <- function(la_df, by_carertype){
  
  la_df <- la_df  %>% 
    # create generic column to indicate if alf has ed att
    mutate(has_ed_dept_attendance = ifelse(is.na(admin_arr_dt), "No" , "Yes"),
           identifier = carer_desc)
  
  if(by_carertype == 0){
    la_df <- la_df %>% 
      filter(carer_flag == 1 ) %>% 
      mutate(identifier = paste0(first_identified_by," unpaid carers")) 
    
  } 
  
  
  la_df2 <- la_df  %>% 
    # select one row per alf and needed columns only
    select(alf_pe, identifier, has_ed_dept_attendance) %>% 
    distinct_all() %>%  
    select(-alf_pe)
  
  # xtabs( ~identifier+ has_ed_dept_attendance, data=la_df2) # uncomment for checking
  
  
  stats_out_df <- la_df2 %>% 
    # define to create the variable to group by for the analysis and decimal place. 
    # i.e counts to show value as: whole number(percentage with 1 dp)
    tbl_summary(by=identifier,
                digits = list( ~c(0,1))) %>%  
    # define statistic used for categorical variables as chi sq and p-value format to 2 dp
    add_p(test= all_categorical() ~ "chisq.test", 
          pvalue_fun = ~ style_pvalue(.x,digits = 2)) %>% 
    #add header to statistics column which is hidden by default
    modify_header(statistic ~ "**Chi-sq Statistic**") %>% 
    # format chi sq statistic to  3 dp
    modify_fmt_fun(statistic  ~ purrr::partial(style_sigfig, digits=3)) %>%  
    gtsummary::as_tibble() %>%  as.data.frame()
  
  # View(stats_out_df)
  
  
  return(stats_out_df)
}



