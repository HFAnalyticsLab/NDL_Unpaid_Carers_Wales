##' 06/12/2022
##' *Functions for GP Interactions Data Analysis*
##' 
#' Creating functions for use in Gp interactions data analysis to simplify analysis 
#' across all LAs.

rate_multiplier <- 1 # set to 1 as calculating event rate per person per year 

# Function creates standardised dataframe for variables to be used in other functions
gp_dataprep <- function(matched_df){
  
  # change column names to lower case
  names(matched_df) <- tolower(names(matched_df))
  
  # add age group column
  matched_df1 <- matched_df %>%
    mutate(age_group = cut(age,
                          breaks = c(17, 39, 49, 59, 69, 79, Inf),
                          labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))

  # add carer description
  matched_df1 <- matched_df1 %>%
    mutate(carer_desc = ifelse(carer_flag == 1, "Unpaid carers", "Non-carers"))
  
  # add column with total by carer type
  df_gp_tot_id <- matched_df1 %>%
    group_by(carer_flag) %>%
    summarise(total_identified = n_distinct(alf_pe)) %>%
    ungroup()
  
  # add column with total by carer type and GP/LA identified 
  df_gp_lagp_tot_id <- matched_df1 %>%
    group_by(carer_flag, first_identified_by) %>%
    summarise(total_identified_lagp = n_distinct(alf_pe)) %>%
    ungroup()
  
  # add column with total by age and sex by carer type and lagp ( age sex standardisation denominator for 0, 1, 2+)
  df_gp_tot_agesex_lagp <- matched_df1 %>%
    group_by(carer_flag, first_identified_by, age_group, sex) %>%
    summarise(total_agesex_identified_lagp = n_distinct(alf_pe)) %>%
    ungroup()
  
  
  # add totals to main df
  matched_df2 <- matched_df1 %>%
    merge(df_gp_tot_id, all = TRUE)
  
  matched_df2 <- matched_df2 %>%
    merge(df_gp_lagp_tot_id, all = TRUE)
  
  matched_df2 <- matched_df2 %>%
    merge(df_gp_tot_agesex_lagp, all = TRUE)
  
  return(matched_df2)
}


# function counts number of GP interactions for each individual grouped by carer status (carer vs non-carer)
# As this data is handling the raw counts, some individuals may have some very high numbers.
gp_indv_count_table <- function(cleaned_df){
 
   matched_gp_counts <- cleaned_df %>%
   
    # flag rows with a GP interacion with a 1, no interaction as a 0
    mutate(gp_event_flag = ifelse(!is.na(event_dt), 1, 0)) %>%
    
    # count number of interactions per cohort per person
    group_by(carer_desc, total_identified, alf_pe) %>%
      summarise(total_gp_events = sum(gp_event_flag)) %>%
    
    # get count of individuals with each total of GP interaction by carer status (carer_desc)
    group_by(carer_desc, total_identified, total_gp_events) %>%
      summarise(count = n()) 
  
    return(matched_gp_counts)
}


# As above, function counts number of GP interactions for each individual grouped by carer status and identification method. 
gp_indv_count_table_lagp <- function(cleaned_df){
  
  
  matched_gp_counts_lagp <- cleaned_df %>%
    
    # flag rows with a GP interaction with a 1 and no interaction as a 0
    mutate(gp_event_flag = ifelse(!is.na(event_dt), 1, 0)) %>%
    
    # count number of interactions per cohort per person
    group_by(carer_desc, first_identified_by, total_identified_lagp, alf_pe) %>%
      summarise(total_gp_events_lagp = sum(gp_event_flag)) %>%
    
    # get counts of individuals with each total of GP interaction by carer status (carer_desc) and first_identified_by
    group_by(carer_desc, first_identified_by, total_identified_lagp, total_gp_events_lagp) %>%
      summarise(count = n()) 

    return(matched_gp_counts_lagp)
}



# Function determines the counts of individuals per number of interactions, but in bins. 
# E.g. how many individuals had 0 interactions, and how many individuals had interactions in groups of 10, up until 40+. 
# The grouping '40+' was chosen as the upper limit to avoid statistical disclosure issues.
gp_indv_bins_table <- function(df_byCarerFlag){
  
  matched_counts_grp <- df_byCarerFlag %>%
    ungroup() %>%
    mutate(events_grp = cut(total_gp_events,
                            breaks = c(-0.1, 5.1, 10.1,15.1,20.1,25.1,30.1,35.1,40.1, Inf),
                            labels = c('0-5', '6-10', '11-15', '16-20', '21-25', '26-30', '31-35', '36-40', '41+'), 
                            include.lowest = TRUE)) %>% 
    
    group_by(carer_desc, events_grp) %>%
    mutate(count_grp = sum(count)) %>%
    
    group_by(carer_desc,  events_grp) %>%
    # percentage of total cohort within each bin
    mutate(events_percentage_grp = (count_grp / total_identified) * 100) %>% 
    select(-total_gp_events, -count,  -total_identified) %>% 
    distinct_all()

  
  return(matched_counts_grp)
}


# As above, but adding first identified by
gp_indv_bins_table_lagp <- function (df_byCF_lagp){
  
  matched_counts_grp_cf_lagp <- df_byCF_lagp%>%
    ungroup() %>%
     mutate(events_grp = cut(total_gp_events_lagp,
                            breaks = c(-0.1, 5.1, 10.1,15.1,20.1,25.1,30.1,35.1,40.1, Inf),
                            labels = c('0-5', '6-10', '11-15', '16-20', '21-25', '26-30', '31-35', '36-40', '41+'), 
                            include.lowest = TRUE)) %>% 
    
    group_by(carer_desc, first_identified_by, events_grp) %>%
      mutate(count_grp = sum(count)) %>%
    
    group_by(carer_desc, first_identified_by, events_grp) %>%
      mutate(events_percentage_grp_lagp = count_grp/total_identified *100) %>% 
      select(-count, -total_gp_events_lagp) %>% 
      distinct_all()
  
  return(matched_counts_grp_cf_lagp)
}

## RATE FUNCTIONS

# function get the numerator for the direct standardisation function by 
# counting the total number of individuals with an interaction by la/gp for each age group and sex combination.
gp_dsr_counts <- function(cleaned_df){
  
  cleaned_df <- cleaned_df %>% 
    filter(carer_desc == "Unpaid carers") %>%
    mutate(identifier = paste0(first_identified_by, "-identified"))
  
  
  gp_counts <- cleaned_df %>%
    
    # flag rows with a GP interaction with a 1 and no interaction as a 0
    mutate(gp_event_flag = ifelse(!is.na(event_dt), 1, 0)) %>%
    
    # group as above by carer flag
    group_by(identifier, age_group, sex) %>%
    summarise(total_gp_events_lagp = sum(gp_event_flag)) %>%
    distinct_all() 
  
  
  return(gp_counts)
}

# function creates a dummy dataframe for age-sex standardisation containing 
# all combinations of identifier, sex and age group
create_dummy_table <- function(){
  
  identifier <- c("GP-identified", "LA-identified")
  sex <- c(1, 2)
  age_group <- c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")

  # creating a dummy table containing all combinations of straifying variables 
  dummy <- as.data.frame(expand.grid(identifier, sex, age_group))
  
  # assign column names
  names(dummy) <- c("identifier", "sex", "age_group")
  
  return(dummy)
}


# function performs direct standardisation using a dummy table, numerator, denomoinator and standard population dataframes and name of LA.
gp_dsr <- function(dummy, count_df, denom_df, std_pop, la_name){
  
  # add local total to dummy
  prep_df1 <- dummy %>%
    left_join(denom_df)
  
  # add local count to above
  prep_df2 <- prep_df1 %>%
    left_join(count_df)
  
  prep_df2$name <- la_name #create name variable for joining to stnd pop
  
  prep_df3 <- prep_df2 %>%
    #left join with standard pop denominator 
    left_join(std_pop, by = c("age_group" = "age", "sex" = "sex_code", "name" = "name"))
  
  # replace NA count group values with 0s
  prep_df3$total_gp_events_lagp[is.na(prep_df3$total_gp_events_lagp)] <- 0
  
  # write age-sex num & denominator out
  initials <- "" # your initials here
  gp_folder <- ''# your path here

  la_name <- unique(prep_df3$name)
  outfn <- paste0(gp_folder,'data/processed/1429_',la_name,'_gp_agesex_counts.xlsx')
  xlsx::write.xlsx(data.frame(prep_df3), file=outfn, sheetName = "gp_lagp", row.names = FALSE) 
  
  
  
  # Using PHEindicatormethods package
  # x = numerator (stratified counts); 
  # n= denominator (statified totals in df); 
  # stdpop = standard population counts; 
  # stdpoptype = "field" indicates that standard population data is part of input data, otherwise it is "vector"(self-explanatory); 
  # multiplier = per how many (i.e put 1000 for per 1000)
  
  std_df <- prep_df3 %>%
    group_by(identifier) %>%
    phe_dsr(
      x = total_gp_events_lagp,
      n = total_agesex_identified_lagp,
      stdpop = pop,
      stdpoptype = "field", 
      multiplier = rate_multiplier)
  
  # calls function that reformats the standardisation output to into desired format
  std_df_neat = gp_reformat_std_df(std_df)
  
  return(std_df_neat)
}


# function reformats the standardisation output to into desired format
gp_reformat_std_df <- function(std_df){
  # remove unneeded cols
  sub_std <- std_df %>%
    select(total_count, total_pop, identifier,  value, lowercl, uppercl) %>% 
    mutate(identifier = substr(identifier,1,2)) %>% 
    select(-total_count, -total_pop)
  
  # format crude rate table to have crude rates of unpaid carer and non-carers side in a wide table rather than long
  wider_df <- tidyr::pivot_wider(sub_std,
                                 
                                 names_from = identifier,
                                 values_from = c( "value", "lowercl", "uppercl"))
  
  # calculate ratio between la vs gp with corresponding lower and upper confidence levels
  wider_df2 <- wider_df %>% 
    mutate(lavsgp_rateratio = value_LA/value_GP,
           lavsgp_lowercl = lowercl_LA/uppercl_GP,
           lavsgp_uppercl = uppercl_LA/lowercl_GP) %>% 
    # round all to 3dp
    mutate_if(is.numeric, ~round(., 3)) %>% 
    as.data.frame() %>% 
    # specify order of columns
    select( 
            value_LA, lowercl_LA, uppercl_LA, 
            value_GP, lowercl_GP, uppercl_GP, 
            lavsgp_rateratio, lavsgp_lowercl, lavsgp_uppercl)
  
  return(wider_df2)
  
}

# function initialises values needed by the gp_dsr function
gp_agesex_standardisation <- function(la_df, la_name){
  
  # read in needed rds (#in laura's rds - in her folder is the df_standardpop_lkup (read in in the main script)
  look_up_path <- ""# your path here
  paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")
  df_standardpop_lkup <- readRDS(paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")) # read in standard population look up
  
    # creating dummy table 
  gp_dummy <- create_dummy_table()
  
  # local age and sex denominator for unpaid carers
  gp_local_denom_carers <- la_df %>%
    filter(carer_desc == "Unpaid carers") %>%
    mutate(identifier = paste0(first_identified_by, "-identified")) %>% 
    # flag rows with a GP interaction with a 1 and no interaction as a 0
    mutate(gp_event_flag = ifelse(!is.na(event_dt), 1, 0)) %>%
    
    # GBH to note: total agesex identified - only count needs to be binned, not denominator.
    group_by(identifier, sex, age_group) %>%
    
    mutate(total_agesex_identified_lagp = n_distinct(alf_pe)) %>%
    select(identifier, sex, age_group, total_agesex_identified_lagp) %>%
    ungroup() %>%
    distinct_all() 
  
  # - local age and sex counts of attended appointments
  gp_bin_count <- gp_dsr_counts(la_df)
  
  # run standardisation function
  gp_overall_agesex_std <- gp_dsr(gp_dummy, gp_bin_count, gp_local_denom_carers, df_standardpop_lkup, la_name)
  
  return(gp_overall_agesex_std)
  
}

# function calculates the crude rates, ratios and upper/lower confidence interval for each cohort comparison determined by by_carertype value.
# If by_carertype = 1, compare unpaid carer vs non-carer. If by_carertype = 0, compare LA-identified vs GP-identified unpaid carers.
gp_cruderate_alf <- function(la_df, by_carertype){
  
  if(by_carertype == 1){
    la_df <- la_df %>% 
      mutate(identifier = ifelse(carer_flag == 0, "non_carers", "unpaid_carers"))
    
    # define columns needed in specified order.
    cnames <- c(
      "total_identified_unpaid_carers", "total_gp_events_unpaid_carers","crude_rate_unpaid_carers", "lowercl_unpaid_carers", "uppercl_unpaid_carers",
      "total_identified_non_carers", "total_gp_events_non_carers","crude_rate_non_carers", "lowercl_non_carers", "uppercl_non_carers",
      "ratio", "ratio_lowercl", "ratio_uppercl")
    
    
  }
  else {
    # for unpaid carers la vs gp comparison
    la_df <- la_df %>% 
      filter(carer_desc == "Unpaid carers") %>%
      mutate(identifier = paste0(first_identified_by),
             # for ease of analysis for each comparison type, create a new column "total_identified" containing values from "total_identified_lagp"
             total_identified = total_identified_lagp)
    
    cnames <- c(
      "total_identified_LA", "total_gp_events_LA", "crude_rate_LA", "lowercl_LA", "uppercl_LA",
      "total_identified_GP", "total_gp_events_GP","crude_rate_GP", "lowercl_GP", "uppercl_GP",
      "ratio", "ratio_lowercl", "ratio_uppercl")
  }
  
  
  # total counts of GP visits for numerator and total_identified for denominator
  la_df1 <- la_df %>%
    # flag rows with a GP interaction with a 1 and no interaction as a 0
    mutate(gp_event_flag = ifelse(!is.na(event_dt), 1, 0)) %>%
    # group as above by carer flag
    group_by(identifier,  total_identified) %>%
    summarise(total_gp_events = sum(gp_event_flag)) %>%

    ungroup() %>% 
    select(identifier, total_identified, total_gp_events)
  
  
   
    
    cruderate_df <- la_df1 %>%
      # calculate crude rate and corresponding lower and upper confidence levels
      mutate(crude_rate = round((total_gp_events/total_identified*rate_multiplier), digits = 3),
             lowercl = (rate_multiplier/total_identified) * (total_gp_events - (1.96 * sqrt(total_gp_events))),
             uppercl = (rate_multiplier/total_identified) * (total_gp_events + (1.96 * sqrt(total_gp_events))))
    
    # format crude rate table to have crude rates of unpaid carer and non-carers side in a wide table rather than long
    wider_cruderate <- tidyr::pivot_wider(cruderate_df,
                                          
                                          names_from = identifier,
                                          values_from = c("total_identified", "total_gp_events", "crude_rate", "lowercl", "uppercl")) %>%
      as.data.frame()
    
    # ratio and upper/lower confidence level calculations between comparison groups
    wider_cruderate$ratio <- wider_cruderate[,6] /wider_cruderate[,5]
    wider_cruderate$ratio_lowercl <- wider_cruderate[,8] /wider_cruderate[,9]
    wider_cruderate$ratio_uppercl <- wider_cruderate[,10] /wider_cruderate[,7]
    
    wider_cruderate <- wider_cruderate %>%
      mutate_if(is.numeric, ~round(., 3)) %>%
      select(cnames) 

      
      return(wider_cruderate)
}




## TIMELINE ANALYSIS
# function adds event_time_period to the input dataframe
gp_add_timeline_cols <- function(cleaned_df){
  # create time group for gp interactions 
  # by timeline 3m/6m/1yr
  event_timgrp <- cleaned_df %>%
    select(alf_pe, first_identified_date, event_dt) %>% 
    # select only those with interaction event date
    filter(!is.na(event_dt)) %>%
    # create time intervals leading to index date
    mutate(event_time_period = interval(event_dt, first_identified_date) %/% months(1),
           event_time_grp = case_when(
             event_time_period >= 0 & event_time_period <= 3 ~ '3-0 months',
             event_time_period >= 4 & event_time_period <= 6 ~ '6-4 months',
             event_time_period >= 7 & event_time_period <= 9 ~ '9-7 months',
             event_time_period > 9 ~ '12-10 months'),
           event_time_grp = factor(event_time_grp, levels = c('12-10 months', '9-7 months', '6-4 months', '3-0 months')))
  
  # joining tables to form timeline subset data
  timegrp_df <- cleaned_df %>%
    filter(!is.na(event_dt)) %>%
    select(alf_pe, carer_desc, first_identified_date, first_identified_by, event_dt, total_identified, total_identified_lagp) %>%
    left_join(event_timgrp) 
  
  return(timegrp_df)
  
}


# function calculates the the event rate per person in each time period (i.e total GP events/total cohort)
gp_timeline_eventrate_tables <- function(timegrp_df, by_lagp){ 
  
  
  if(by_lagp == 1){
    timegrp_df1 <- timegrp_df %>%
      filter(carer_desc == "Unpaid carers") %>%
      mutate(cohort = paste0(first_identified_by, "-identified"),
             cohort = factor(cohort, levels = c('LA-identified', 'GP-identified')))
    
    timeline_df <- timegrp_df1 %>%
      group_by(cohort, total_identified_lagp, event_time_grp) %>%
      # count total gp interactions
      summarise(total_interactions_grp = n()) %>%
      # get rate of event i.e total interactions/total cohort
      mutate(rate_grp = total_interactions_grp/total_identified_lagp ) %>%
      # select required columns
      select(cohort, total_identified_lagp, event_time_grp, total_interactions_grp, rate_grp) %>%
      # rename column
      rename(time_grp = event_time_grp) %>%
      distinct_all() %>%
      ungroup()
    
  } else{
    timegrp_df1 <- timegrp_df %>%
      mutate(cohort = factor(carer_desc, levels = c('Unpaid carers', 'Non-carers')))
    
    timeline_df <- timegrp_df1 %>%
      group_by(cohort, total_identified, event_time_grp) %>%
      summarise(total_interactions_grp = n()) %>%
      mutate(rate_grp = total_interactions_grp/total_identified) %>%
      select(cohort, total_identified, event_time_grp, total_interactions_grp, rate_grp) %>%
      rename(time_grp = event_time_grp) %>%
      distinct_all() %>%
      ungroup()
  }
  
  # remove where there are no interactions
  timeline_df <- timeline_df %>%
    filter(!is.na(time_grp))
  
  return(as.data.frame(timeline_df))
  
}
  

# function creates timeline plots using event rate table produced by (gp_timeline_eventrate_tables) for each LA 
# for cohorts specified by the by_lagp flag.
gp_timeline_rate_plots <- function(df, la_name, by_lagp){
  library(lubridate)
  #pre-define labels and titles
  y_lab <- "GP interaction rate"
  x_lab <- "Month groups prior to index date"
  plot_type <- "dodge"
  
  
  title_subtext <- "GP interactions"
  
  
  if(by_lagp == 1){
    plot_title <- paste0(la_name, " rate of GP interactions per person \nin months prior to index assessment date for unpaid carers \nidentified by GP and LA" )
    plot_palette <- lagpcarers_palette
    legend_title <- "Unpaid carers cohort"
    
  } else {
    plot_title <- paste0(la_name, " rate of GP interactions per person \nin months prior to index assessment date by carer type" )
    plot_palette <- carertype_palette
    legend_title <- "Cohort"
  }
 
  df_plot <- df  %>% 
    ggplot() + 
    scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
    geom_col(aes(x = time_grp, 
                 y = rate_grp,
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
                  y = rate_grp, 
                  # display rates as text rounded to 1dp
                  label = sprintf("%.1f", rate_grp),
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

                               