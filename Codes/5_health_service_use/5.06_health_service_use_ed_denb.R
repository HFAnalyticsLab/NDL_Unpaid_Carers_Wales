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
denbighshire_matched_edatt_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_MATCHEDCOHORT_ED_DENBIGHSHIRE_2;")
nrow(denbighshire_matched_edatt_raw)
denbighshire_matched_edatt_raw2 <- edatt_dataprep(denbighshire_matched_edatt_raw)


## STEP 2 prepare counts by carer status and lagp using functions
denbighshire_edatt_counts <- edatt_countperc_table(denbighshire_matched_edatt_raw2)
edatt_countperc_table_lagp(denbighshire_matched_edatt_raw2, "denbighshire")


## STEP 3 Counts of number of carers and percentage of carers with ED attendances in year prior to identification by carer status and lagp 
denbighshire_edatt_lagp_binned_counts <- edatt_binsperc_table_lagp(denbighshire_edatt_lagp_counts)
denbighshire_edatt_binned_counts <- edatt_binsperc_table(denbighshire_edatt_counts)


## STEP 4 Breakdown by months prior to index assessment date
denbighshire_edatt_tl_table <- edatt_timeline_tables(denbighshire_matched_edatt_raw2,0)
# View(denbighshire_timeline_table)
denbighshire_edatt_tl_table_lagp <- edatt_timeline_tables(denbighshire_matched_edatt_raw2,1)
# View(denbighshire_timeline_table_lagp)


## STEP 5 prevalence rate ratios
# - age-sex standardised
# standardised rate ratio for unpaid carers cohort la vs gp
paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds")
df_standardpop_lkup <- readRDS(paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds"))  # read in rds
denbighshire_edatt_std_hf <- edatt_standardisation(denbighshire_agesex_edatt_counts_hf, denbighshire_agesex_edatt_totals_hf, "Denbighshire", df_standardpop_lkup)
denbighshire_formatted_edatt_std_lagp <- edatt_reformat_std_table(denbighshire_edatt_std_hf)



# - prevalence rates 
denbighshire_overall_edatt_cruderate <- edatt_calc_cruderate(denbighshire_matched_edatt_raw2, 1 ) #  carer type
denbighshire_overall_edatt_cruderate_lagp <- edatt_calc_cruderate(denbighshire_matched_edatt_raw2, 0 ) # lagp carers



## STEP 6 chi-sq calculation
denbighshire_edatt_chisq <- edatt_chisq(denbighshire_matched_edatt_raw2, by_carertype = 1) # carer type
denbighshire_edatt_chisq_lagp <- edatt_chisq(denbighshire_matched_edatt_raw2, by_carertype = 0) # lagp carers 



## STEP 7 Plot Graphs

### Counts  
edatt_bins_plot(denbighshire_edatt_binned_counts, "Denbighshire", 1, 0 ) # count bin facet carer type
edatt_bins_plot(denbighshire_edatt_lagp_binned_counts, "Denbighshire", 1, 1 )# count bin facet lagp

## percentage  
denbighshire_edatt_perc_plot <- edatt_bins_plot(denbighshire_edatt_binned_counts, "Denbighshire", 0, 0 ) #facet carer type
denbighshire_edatt_perc_lagp_plot <- edatt_bins_plot(denbighshire_edatt_lagp_binned_counts, "Denbighshire", 0, 1 ) #facet lagp

## timeline 
denbighshire_edatt_timeline <- edatt_timeline_perc_plots(denbighshire_edatt_tl_table, "Denbighshire", 0) # by carer type
denbighshire_edatt_timeline_lagp <- edatt_timeline_perc_plots(denbighshire_edatt_tl_table_lagp, "Denbighshire", 1) #  compare carers lagp only

denbighshire_edatt_tl_plot_lagp <- emergency_timeline_perc_plots(denbighshire_edatt_tl_table_lagp, "Denbighshire", 1) 

denbighshire_edatt_tl_lagp_masked <- read_excel(path = paste0(ed_path,"data/processed/1429_denb_edatt_tl_peje_masked.xlsx"), sheet = "lagp") # read in masked data for lagp comparison as original table has <10 counts
denbighshire_edatt_tl_plot_lagp_masked <- emergency_timeline_perc_plots(denbighshire_edatt_tl_lagp_masked, "Denbighshire", 1) 



## Step 8 save data and plots
# binned counts and percentages by carer type and lagp
outfn <- paste0(ed_path,'data/processed/1429_denbighshire_edatt_counts_peje.xlsx')
xlsx::write.xlsx(data.frame(denbighshire_edatt_binned_counts), file=outfn, sheetName = "ed_carertype", row.names = FALSE) # carer type
xlsx::write.xlsx(data.frame(denbighshire_edatt_lagp_binned_counts), file=outfn, sheetName = "ed_lagp", row.names = FALSE, append = TRUE) # lagp carers


# prevalance rates
outfn <- paste0(ed_path,'data/processed/1429_denbighshire_edatt_rates_stats_peje.xlsx')
xlsx::write.xlsx(denbighshire_overall_edatt_cruderate, file=outfn, sheetName = "ed_crude_carertype", row.names = FALSE) # carer type
xlsx::write.xlsx(denbighshire_overall_edatt_cruderate_lagp, file=outfn, sheetName = "ed_crude_lagp", row.names = FALSE, append = TRUE) # lagp carers
xlsx::write.xlsx(denbighshire_formatted_edatt_std_lagp, file=outfn, sheetName = "ed_agesex_lagp", row.names = FALSE, append = TRUE) # lagp carers
#stats
xlsx::write.xlsx(denbighshire_edatt_chisq, file=outfn, sheetName = "ed_chisq_carertype", row.names = FALSE, append = TRUE) # carer type
xlsx::write.xlsx(denbighshire_edatt_chisq_lagp, file=outfn, sheetName = "ed_chisq_lagp", row.names = FALSE, append = TRUE) # lagp carers

# timeline by carer type and lagp
outfn <- paste0(ed_path,'data/processed/1429_denbighshire_edatt_timeline_peje.xlsx')
xlsx::write.xlsx(data.frame(denbighshire_edatt_tl_table), file=outfn, sheetName = "carertype", row.names = FALSE) # carer type
xlsx::write.xlsx(data.frame(denbighshire_edatt_tl_table_lagp), file=outfn, sheetName = "lagp", row.names = FALSE, append = TRUE) # lagp carers


# percentage plots by carer type and lagp
save_ggplot(denbighshire_edatt_perc_plot, ed_path)
save_ggplot(denbighshire_edatt_perc_lagp_plot, ed_path)

# timeline
save_ggplot(denbighshire_edatt_tl_plot_lagp, ed_path) # lagp original
save_ggplot(denbighshire_edatt_tl_plot_lagp_masked, ed_path) # lagp masked
