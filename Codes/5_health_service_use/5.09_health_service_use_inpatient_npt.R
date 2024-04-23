library(lubridate)
library(openxlsx)
library(ggplot2)
library(RODBC)
library(tidyr)
library(dplyr)
library(reshape2)
library(xlsx)
library(PHEindicatormethods)

RODBC::odbcCloseAll()
initials <- ""
source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_RODBC_login_20221031.R'))
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(pedw_path,"functions/1429_inpatient_histogram_functions_peje_20230517.R" ))


npt_matched_pedw_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_MATCHEDCOHORT_INPATIENT_NPT_2")

# STEP 1 Run preprocessing function
npt_matched_pedw_raw2 <- inpatients_dataprep(npt_matched_pedw_raw)


# STEP 2 Add counts
# Counts of number of carers and percentage of carers with X PEDW events in year prior to identification as a carer by:  
# - identification cohort  
# - admission type (planned/emergency)  
# # **NOTE:** Percentages may not total 100 as the admission type groups are not mutually exclusive.
npt_matched_pedw_counts <- inpatients_countperc_table(npt_matched_pedw_raw2)

inpatients_countperc_table_lagp(npt_matched_pedw_raw2, "npt")




# STEP 3 Counts of number of carers and percentage of carers with 0 to >=1 bins PEDW events in year prior to identification as a carer by:  
# - carer or non-carer and identification type
# - admission type (planned/emergency)  
# # **NOTE:** Percentages may not total 100 as the admission type groups are not mutually exclusive.

npt_matched_pedw_counts_grp <- inpatients_binsperc_table(npt_matched_pedw_counts)
npt_matched_pedw_counts_grp_lagp <- inpatients_binsperc_table_lagp(npt_matched_pedw_counts_lagp)


# STEP 4 Split into separate tables for planned and emergency admissions for graphing.
# get binned counts and percentages using function by carer type
npt_overall_cnt <- inpatient_overall_countsperc(npt_matched_pedw_counts_grp, 0, "npt")
inpatient_admtype_countsperc(npt_matched_pedw_counts_grp, 0, "npt")
# get binned counts and percentages using function by carer type and lagp
npt_overall_cnt_lagp <- inpatient_overall_countsperc(npt_matched_pedw_counts_grp_lagp, 1, "npt")
inpatient_admtype_countsperc(npt_matched_pedw_counts_grp_lagp, 1, "npt")



# STEP 5 timeline
# get overall timeline breakdown by carer type & carer type + lagp
npt_pedw_overall_tl <- inpatient_timeline_tables(npt_matched_pedw_raw2, 0)
npt_pedw_overall_tl_lagp <- inpatient_timeline_tables(npt_matched_pedw_raw2, 1)
# emergency by carer type & carer type + lagp
npt_raw_emergency <- npt_matched_pedw_raw2 %>%  filter(admis_type_desc == "Emergency" )
npt_pedw_emergency_tl <- inpatient_timeline_tables(npt_raw_emergency, 0)
npt_pedw_emergency_tl_lagp <- inpatient_timeline_tables(npt_raw_emergency, 1)
# planned by carer type & carer type + lagp
npt_raw_planned <- npt_matched_pedw_raw2 %>%  filter(admis_type_desc == "Planned" )
npt_pedw_planned_tl <- inpatient_timeline_tables(npt_raw_planned, 0)
npt_pedw_planned_tl_lagp <- inpatient_timeline_tables(npt_raw_planned, 1)


# STEP 6 prev rate ratio calculation
# standardised rate ratio for unpaid carers cohort la vs gp
df_standardpop_lkup <- readRDS(paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")) # read in needed rds
# - run standardisation function
npt_inpatient_std_hf <- inpatient_standardisation(npt_agesex_pedw_counts_hf, npt_agesex_pedw_totals_hf, "Neath Port Talbot", df_standardpop_lkup)
# - format output of above function
npt_formatted_inpatient_std_hf <- inpatient_reformat_std_table(npt_inpatient_std_hf)

# overall crude rate calc by carer type
npt_overall_cruderate <- inpatient_calc_cruderate(npt_matched_pedw_raw2, by_admistype = 0, by_carertype = 1 )
# crude rate calc by admission type and carer type
npt_admistype_cruderate <- inpatient_calc_cruderate(npt_matched_pedw_raw2,  by_admistype = 1, by_carertype = 1)

# overall crude rate calc by LA and GP-identified
npt_overall_cruderate_lagp <- inpatient_calc_cruderate(npt_matched_pedw_raw2, by_admistype = 0, by_carertype = 0 )
# crude rate calc by admission type and LA and GP-identified
npt_admistype_cruderate_lagp <- inpatient_calc_cruderate(npt_matched_pedw_raw2,  by_admistype = 1, by_carertype = 0)






# STEP 7 Create plots

# a. percentages (1+ only)
# - overall
npt_inpatient_overall_plot <- inpatient_bins_plot(npt_overall_cnt, "NPT", "", 0, 0)
npt_inpatient_overall_plot_lagp <- inpatient_bins_plot(npt_overall_cnt_lagp, "NPT", "", 0, 1)

# - planned
npt_inpatient_planned_admis_plot <- inpatient_bins_plot(npt_Planned_adm_cnt, "NPT", "planned ", 0, 0)
npt_inpatient_planned_admis_plot_lagp <- inpatient_bins_plot(npt_Planned_adm_cnt_lagp, "NPT", "planned ", 0, 1)

# - emergency
npt_inpatient_emergency_admis_plot <- inpatient_bins_plot(npt_Emergency_adm_cnt, "NPT", "emergency ", 0, 0)
npt_inpatient_emergency_admis_plot_lagp <- inpatient_bins_plot(npt_Emergency_adm_cnt_lagp, "NPT", "emergency ", 0, 1)


# b. timeline percentages
# - overall (1+ only)
npt_inpatient_overall_timeline_plot <- inpatient_timeline_perc_plots(npt_pedw_overall_tl, "NPT", 0, "")
npt_inpatient_overall_timeline_plot_lagp <- inpatient_timeline_perc_plots(npt_pedw_overall_tl_lagp, "NPT", 1, "")

# - planned
npt_inpatient_planned_timeline_plot <- inpatient_timeline_perc_plots(npt_pedw_planned_tl, "NPT", 0, "planned")
npt_inpatient_planned_timeline_plot_lagp <- inpatient_timeline_perc_plots(npt_pedw_planned_tl_lagp, "NPT", 1, "planned")

# - emergency
npt_inpatient_emergency_timeline_plot <- inpatient_timeline_perc_plots(npt_pedw_emergency_tl, "NPT", 0, "emergency")
npt_inpatient_emergency_timeline_plot_lagp <- inpatient_timeline_perc_plots(npt_pedw_emergency_tl_lagp, "NPT", 1, "emergency")



# STEP 8 Save plots 

save_ggplot(npt_inpatient_overall_plot, pedw_path)
save_ggplot(npt_inpatient_overall_plot_lagp, pedw_path)
save_ggplot(npt_inpatient_planned_admis_plot, pedw_path)
save_ggplot(npt_inpatient_planned_admis_plot_lagp, pedw_path)
save_ggplot(npt_inpatient_emergency_admis_plot, pedw_path)
save_ggplot(npt_inpatient_emergency_admis_plot_lagp, pedw_path)


# timeline (only non-disclosive ones included)
save_ggplot(npt_inpatient_overall_timeline_plot, pedw_path)
save_ggplot(npt_inpatient_planned_timeline_plot, pedw_path)
save_ggplot(npt_inpatient_emergency_timeline_plot, pedw_path)




# STEP 9 Save data tables
# timeline
outfn <- paste0(pedw_path,'data/processed/1429_npt_pedw_timelines.xlsx')
xlsx::write.xlsx(data.frame(npt_pedw_overall_tl), file=outfn, sheetName = "overall_carertype", row.names = FALSE)
xlsx::write.xlsx(data.frame(npt_pedw_planned_tl), file=outfn, sheetName = "planned_carertype", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(data.frame(npt_pedw_emergency_tl), file=outfn, sheetName = "emergency_carertype", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(data.frame(npt_pedw_overall_tl_lagp), file=outfn, sheetName = "overall_lagp", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(data.frame(npt_pedw_planned_tl_lagp), file=outfn, sheetName = "planned_lagp", row.names = FALSE, append = TRUE)
xlsx::write.xlsx(data.frame(npt_pedw_emergency_tl_lagp), file=outfn, sheetName = "emergency_lagp", row.names = FALSE, append = TRUE)


# rates
outfn <- paste0(pedw_path,'data/processed/1429_npt_pedw_prevrates.xlsx')
xlsx::write.xlsx(npt_overall_cruderate, file=outfn, sheetName = "overall_crude_carertype", row.names = FALSE) # crude hospitalisation rate by carer type
xlsx::write.xlsx(npt_admistype_cruderate, file=outfn, sheetName = "admistype_crude_carertype", row.names = FALSE, append = TRUE) # admission type rates by carer type
xlsx::write.xlsx(npt_overall_cruderate_lagp, file=outfn, sheetName = "overall_crude_lagp", row.names = FALSE, append = TRUE) # crude hospitalisation rate by la gp carers
xlsx::write.xlsx(npt_admistype_cruderate_lagp, file=outfn, sheetName = "admistype_crude_lagp", row.names = FALSE, append = TRUE) # admission type rates by la gp carers
xlsx::write.xlsx(npt_formatted_inpatient_std_lagp, file=outfn, sheetName = "all_agesex_lagp", row.names = FALSE, append = TRUE) # age sex standardised hospital admission rates and admission type rates by la gp carers

