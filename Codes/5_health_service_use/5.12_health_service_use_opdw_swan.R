# loading required packages
library(dplyr)
library(ggplot2)
library(openxlsx)
library(stringr)
library(xlsx)
library(PHEindicatormethods)
library(lubridate)
library(stringr)

RODBC::odbcCloseAll()
initials <- ""
source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_RODBC_login_20221031.R'))
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(opdw_path,"functions/1429_opdw_functions_gbh_20221111.R" ))



# read in data from DB2
swansea_matched_op_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.GBH_MATCHEDCOHORT_OPDW_SWANSEA_2")

# STEP 1: Running data prep functions 
swansea_matched_op_raw2 <- outpatients_dataprep(swansea_matched_op_raw)


# STEP 2: Adding counts by individual
# Counts of number of carers with X OPDW events in year prior to identification
# as a carer by using identification cohort (attended, missed, cancelled, or no appointment) by:
# - carer status
# - carer status and lagp

# by carer status
swansea_matched_opdw_indv_counts <- outpatients_indv_count_table(swansea_matched_op_raw2)

# by carer status and lagp
swansea_matched_opdw_indv_counts_lagp <- outpatients_indv_count_table_lagp(swansea_matched_op_raw2)



# STEP 3: Adding counts and percentages by individual (binned)
# Counts of number of carers with 0/1/2+ OPDW events in year prior to identification
# as a carer by using identification cohort (attended, missed, cancelled, or no appointment) by:
# - carer status
# - carer status and lagp

# by carer status
swansea_matched_opdw_counts_bins <- outpatients_indv_bins_table(swansea_matched_opdw_indv_counts)

# by carer status and lagp
swansea_matched_opdw_counts_bins_lagp <- outpatients_indv_bins_table_lagp(swansea_matched_opdw_indv_counts_lagp)


# STEP 4: Add counts and percentages by appointment type
# Counts of number of carers with X OPDW events in year prior to identification
# as a carer by using identification cohort and attendance type count cohort (attended, missed, cancelled, or no appointment) by:
# - carer status
# - carer status and lagp

# by Carer status
swansea_matched_opdw_app_counts<- outpatients_app_count_table(swansea_matched_op_raw2)

# by Carer status and lagp
swansea_matched_opdw_app_counts_lagp<- outpatients_app_count_table_lagp(swansea_matched_op_raw2)



# STEP 5: Split into separate tables for attendance type for graphing.
swansea_op_subsets <- outpatient_subset_proportions(swansea_matched_opdw_app_counts, swansea_matched_opdw_app_counts_lagp)


# Appointment Subset by carer status
swansea_matched_opdw_apps <- swansea_op_subsets[[1]]

# No appointment by carer status
swansea_matched_opdw_no_apps <- swansea_op_subsets[[2]]

# Appointment Subset by carer status + lagp
swansea_matched_opdw_apps_lagp <- swansea_op_subsets[[3]]

# No appointment by carer status
swansea_matched_opdw_no_apps_lagp <- swansea_op_subsets[[4]]



# Step 6: Rate Ratios for overall counts 
df_standardpop_lkup <- readRDS(paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")) # read in standard population look up


# 6a. age-sex std alfs with op attendance over total cohort
swansea_op_dummy <- create_dummy_table("attended_alf")

# - local age and sex denominator for unpaid carers 
swansea_op_local_denom_carers <- swansea_matched_op_raw2 %>% 
  filter(carer_desc == 'Unpaid carers') %>% 
  group_by(first_identified_by, age_group, sex) %>% 
  mutate(total_agesex_identified_lagp = n_distinct(alf_pe)) %>% 
  select(first_identified_by, age_group, sex, total_agesex_identified_lagp) %>% 
  ungroup() %>% 
  distinct_all()

# - local age and sex counts
swansea_op_local_numerator <- swansea_matched_op_raw2 %>%
  filter(carer_desc == "Unpaid carers") %>% 
  # flag rows with an OPDW attendance (5 or 6) with a 1 and no OPDW event as a 0
  mutate(opdw_att_flag =  ifelse(!is.na(attend_dt) & attend_cd %in% c(5, 6), 1, 0)) %>%
  filter(opdw_att_flag == 1) %>% 
  select(alf_pe, first_identified_by, age_group, sex, opdw_att_flag) %>%
  distinct_all() %>% 
  # group as above by carer status
  group_by( first_identified_by, age_group, sex) %>%
  summarise(count_grp = n()) %>% 
  mutate(events_grp = "Total attended appointments")


# run direct standardisation function
swansea_op_att_alf_agesex_std <- op_dsr(swansea_op_dummy, swansea_op_local_numerator, swansea_op_local_denom_carers, df_standardpop_lkup, "Swansea")



# 6b. crude prevalence rates 
# attended appts over total cohort 
swansea_op_att_alf_rate <- op_cruderate_alf(swansea_matched_op_raw2,  1)
swansea_op_att_alf_rate_lagp <- op_cruderate_alf(swansea_matched_op_raw2, 0)



# STEP 7 - timeline breakdown in quarters of total attendances over total cohort
swansea_op_timegrp_table <- op_add_timeline_cols(swansea_matched_op_raw2)

# carer status
swansea_op_att_df <- op_timeline_appt_tables(swansea_op_timegrp_table, 0)
# lagp
swansea_op_att_df_lagp <- op_timeline_appt_tables(swansea_op_timegrp_table, 1)

# plot timelines - carer status
swansea_op_att_tl_plot <- op_timeline_perc_plots(swansea_op_att_df, "Swansea", 0)
# plot timelines - lagp
swansea_op_att_tl_plot_lagp <- op_timeline_perc_plots(swansea_op_att_df_lagp, "Swansea", 1)



# Export rate and timeline table and plots
# one excel with multiple sheets for rates
# rate comparisons
outfn <- paste0(opdw_path,'data/processed/1429_swansea_op_rates_stats_20230720.xlsx')
# Prevalence/age sex standardised rates 
# carer type
xlsx::write.xlsx(swansea_op_att_alf_rate, file=outfn, sheetName = "carertype_crude_att_alfs", row.names = FALSE) 
# lagp crude rates
xlsx::write.xlsx(swansea_op_att_alf_rate_lagp, file=outfn, sheetName = "lagp_crude_att_alfs", append = TRUE,row.names = FALSE) 
#lagp age-sex std rates
xlsx::write.xlsx(swansea_op_att_alf_agesex_std, file=outfn, sheetName = "lagp_standardised_att_alfs", append = TRUE,row.names = FALSE) 


# timeline table and plots
outfn <- paste0(opdw_path,'data/processed/1429_swansea_op_timeline_20230720.xlsx')
#carer non carers
xlsx::write.xlsx(swansea_op_att_df, file=outfn, sheetName = "carertype_alfs_attended", row.names = FALSE) 

# carers lagp only
xlsx::write.xlsx(swansea_op_att_df_lagp, file=outfn, sheetName = "lagpcarers_alfs_attended", append = TRUE, row.names = FALSE) 


save_ggplot(swansea_op_att_tl_plot, opdw_path)
save_ggplot(swansea_op_att_tl_plot_lagp, opdw_path)



# STEP 8 Plot Proportions
# Creating plots that show the percentages each bin group makes up for the overall cohorts.

# % individuals in each count bin by carer status
swansea_opdw_binned_perc_plot <- swansea_matched_opdw_counts_bins %>%
  ggplot() +
  geom_col(aes(x = events_grp,
               y = events_percentage_grp,
               fill = carer_desc), 
           position = 'dodge') +
  labs(title = "Swansea outpatients: % of Appointments Made (Bins) - Overall",
       x = "Number of Outpatients Events / Individual",
       y = "Percentage of Individuals (%)") +
  geom_text(aes(x = events_grp,  
                y = events_percentage_grp, 
                label=sprintf("%.1f", events_percentage_grp),
                group = carer_desc),  position= position_dodge(width = 1), vjust=-.25,
            size = 3.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 70),
                     breaks = seq(0, 70, 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white", color = "grey50"),
        strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black")) +
  scale_fill_manual(values = c('Unpaid carer' = "black", 'Non-carer' = "gray80"), name = 'Cohort')

# swansea_opdw_binned_perc_plot

# % individuals in each count bin by carer status and lagp
swansea_opdw_binned_perc_lagp_plot <- swansea_matched_opdw_counts_bins_lagp %>%
  ggplot() +
  geom_col(aes(x = events_grp,
               y = events_percentage_grp_lagp,
               fill = carer_desc), position = 'dodge') +
  facet_wrap(~first_identified_by) +
  labs(title = "Swansea outpatients: % of Appointments Made (Bins) - Cohorts",
       x = "Number of Outpatient Events / Individual",
       y = "Percentage of Individuals (%)") +
  geom_text(aes(x = events_grp,  
                y = events_percentage_grp_lagp, 
                label=sprintf("%.1f", events_percentage_grp_lagp),
                group = carer_desc),  
            position= position_dodge(width = 1), 
            vjust=-.25,
            size = 3.5) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 75),
                     breaks = seq(0, 70, 10)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white", color = "grey50"),
        strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black")) +
  scale_fill_manual(values = c('Unpaid carer' = "black", 'Non-carer' = "grey"), name = 'Cohort')




# Export proportion tables and plots 
# bins table
save_csv_rds(swansea_matched_opdw_counts_bins, opdw_path)
save_csv_rds(swansea_matched_opdw_counts_bins_lagp, opdw_path)


# attendance tables
save_csv_rds(swansea_matched_opdw_apps, opdw_path)
save_csv_rds(swansea_matched_opdw_apps_lagp, opdw_path)


# bins plots
save_ggplot(swansea_opdw_binned_perc_plot, opdw_path)
save_ggplot(swansea_opdw_binned_perc_lagp_plot, opdw_path)


