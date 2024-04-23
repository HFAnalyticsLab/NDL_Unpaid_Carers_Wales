library(lubridate)
library(ggplot2)
library(RODBC)
library(tidyr)
library(dplyr)
library(reshape2)
library(PHEindicatormethods)
library(stringr)
library(xlsx)

RODBC::odbcCloseAll()
initials <- "" # your initials here
source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_RODBC_login_20221031.R'))
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(ed_path, "functions/1429_matchedcohort_EDattendances_functions_peje_20221110.R"))

## STEP 1 import and clean data
swansea_matched_edatt_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_MATCHEDCOHORT_ED_SWANSEA_2;")
swansea_matched_edatt_raw2 <- edatt_dataprep(swansea_matched_edatt_raw)

## STEP 2 prepare counts by carer status and lagp using functions
swansea_edatt_counts <- edatt_countperc_table(swansea_matched_edatt_raw2)
edatt_countperc_table_lagp(swansea_matched_edatt_raw2, "swansea")


## STEP 3 Counts of number of carers and percentage of carers with ED attendances in year prior to identification by carer status and lagp 
swansea_edatt_lagp_binned_counts <- edatt_binsperc_table_lagp(swansea_edatt_lagp_counts)
swansea_edatt_binned_counts <- edatt_binsperc_table(swansea_edatt_counts)


## STEP 4 Breakdown by months prior to index assessment date
swansea_edatt_tl_table <- edatt_timeline_tables(swansea_matched_edatt_raw2,0)
swansea_edatt_tl_table_lagp <- edatt_timeline_tables(swansea_matched_edatt_raw2,1)


## STEP 5 prevalence rate ratios
# - age-sex standardised lagp carers
# standardised rate ratio for unpaid carers cohort la vs gp

paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")
df_standardpop_lkup <- readRDS(paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds"))  # read in rds
swansea_edatt_std_hf <- edatt_standardisation(swansea_agesex_edatt_counts_hf, swansea_agesex_edatt_totals_hf, "Swansea", df_standardpop_lkup)
swansea_formatted_edatt_std_lagp <- edatt_reformat_std_table(swansea_edatt_std_hf)


# - prevalence rates 
swansea_overall_edatt_cruderate <- edatt_calc_cruderate(swansea_matched_edatt_raw2, 1 ) #  carer type
swansea_overall_edatt_cruderate_lagp <- edatt_calc_cruderate(swansea_matched_edatt_raw2, 0 ) # lagp carers



## STEP 6 chi-sq calculation
swansea_edatt_chisq <- edatt_chisq(swansea_matched_edatt_raw2, by_carertype = 1) # carer type
swansea_edatt_chisq_lagp <- edatt_chisq(swansea_matched_edatt_raw2, by_carertype = 0) # lagp carers 


## STEP 7 Plot graphs

### Counts  
edatt_bins_plot(swansea_edatt_binned_counts, "Swansea", 1, 0 ) # count bin facet carer type
edatt_bins_plot(swansea_edatt_lagp_binned_counts, "Swansea", 1, 1 )# count bin facet lagp

## percentage  
swansea_edatt_perc_plot <- edatt_bins_plot(swansea_edatt_binned_counts, "Swansea", 0, 0 ) #facet carer type
swansea_edatt_perc_lagp_plot <- edatt_bins_plot(swansea_edatt_lagp_binned_counts, "Swansea", 0, 1 ) #facet lagp

## timeline 
swansea_edatt_timeline <- edatt_timeline_perc_plots(swansea_edatt_tl_table, "Swansea", 0) # by carer type
swansea_edatt_timeline_lagp <- edatt_timeline_perc_plots(swansea_edatt_tl_table_lagp, "Swansea", 1) #  compare carers lagp only



## STEP 8 Save data
# binned counts and percentages by carer type and lagp
outfn <- paste0(ed_path,'data/processed/1429_swansea_edatt_counts_peje.xlsx')
xlsx::write.xlsx(data.frame(swansea_edatt_binned_counts), file=outfn, sheetName = "ed_carertype", row.names = FALSE) # carer type
xlsx::write.xlsx(data.frame(swansea_edatt_lagp_binned_counts), file=outfn, sheetName = "ed_lagp", row.names = FALSE, append = TRUE) # lagp carers


# prevalance rates
outfn <- paste0(ed_path,'data/processed/1429_swansea_edatt_rates_stats_peje.xlsx')
xlsx::write.xlsx(swansea_overall_edatt_cruderate, file=outfn, sheetName = "ed_crude_carertype", row.names = FALSE) # carer type
xlsx::write.xlsx(swansea_overall_edatt_cruderate_lagp, file=outfn, sheetName = "ed_crude_lagp", row.names = FALSE, append = TRUE) # lagp carers
xlsx::write.xlsx(swansea_formatted_edatt_std_lagp, file=outfn, sheetName = "ed_agesex_lagp", row.names = FALSE, append = TRUE) # lagp carers
#stats
xlsx::write.xlsx(swansea_edatt_chisq, file=outfn, sheetName = "ed_chisq_carertype", row.names = FALSE, append = TRUE) # carer type
xlsx::write.xlsx(swansea_edatt_chisq_lagp, file=outfn, sheetName = "ed_chisq_lagp", row.names = FALSE, append = TRUE) # lagp carers

# timeline by carer type and lagp
outfn <- paste0(ed_path,'data/processed/1429_swansea_edatt_timeline_peje.xlsx')
xlsx::write.xlsx(data.frame(swansea_edatt_tl_table), file=outfn, sheetName = "carertype", row.names = FALSE) # carer type
xlsx::write.xlsx(data.frame(swansea_edatt_tl_table_lagp), file=outfn, sheetName = "lagp", row.names = FALSE, append = TRUE) # lagp carers


# percentage plots by carer type and lagp
save_ggplot(swansea_edatt_perc_plot, ed_path)
save_ggplot(swansea_edatt_perc_lagp_plot, ed_path)

# timeline by carer type and lagp
save_ggplot(swansea_edatt_timeline, ed_path)
save_ggplot(swansea_edatt_timeline_lagp, ed_path)

