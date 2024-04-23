#'This script creates identification timeline (yearly, quarterly and financial years) data tables and visuals 
#'for NPT unpaid carers '#

## ---- Demographics-npt-dataprep ----

library(odbc)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(busdater)
library(stringr)
library(forcats)
library(broom) 
library(gtsummary)

initials <- ' '# your initials here
source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_RODBC_login_20221031.R'))
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(demographics_path, 'functions/1429_demog_functions_peje_20221114.R'))
source(paste0(demographics_path, 'functions/1429_Plot_Demographics_functions.R'))



## ---- Demographics-npt-dataprep ----
## Import data
npt_dedup_raw <- sqlQuery(sql, "select * from SAILW1429V.PEJE_DEDUP_GP_LA_NPT")

# prepare data using function
df_npt_dedup <- npt_dedup_raw
df_npt_dedup <- demog_dataprep(npt_dedup_raw)

# calculate mean age
mean(df_npt_dedup$age) 
# perform t-test on age
df_npt_lagp_age_ttest <- tidy(t.test(age ~ factor(identifiedby, levels = c("LA", "GP")), df_npt_dedup)) 
save_csv_rds(df_npt_lagp_age_ttest, demographics_path) # save t-test results

## Counts - overall
df_npt_dedup_overall <- demog_overall_counts(df_npt_dedup)

# counts for unpaid carers
df_npt_dedup_overall_carertype <- df_npt_dedup_overall %>% group_by(variable, level) %>% 
  filter(variable %in% c("age", "sex", "wimd", "ruc")) %>% 
  mutate(total_count = sum(count),
         total_identified = sum(total),
         total_perc = format(round(total_count/total_identified*100, 1),nsmall = 1)) %>% 
  select(variable, level, total_count, total_identified, total_perc) %>% 
  distinct_all()
save_csv_rds(df_npt_dedup_overall_carertype, demographics_path)


# count and percentage for wimd - unpaid carer vs non-carers
npt_matched <- sqlQuery(sql, "select *, AGE_INDEXDATE as AGE from SAILW1429V.PEJE_MATCHED_COHORT_NPT_2")

npt_matched_cleaned <- demog_dataprep(npt_matched) # clean dataset

npt_ucnc_wimd <- npt_matched_cleaned %>% 
  # select required columns
  select(alf_pe, treated, wimd, wimd_desc) %>% 
  distinct_all() %>% 
  mutate(identifier = ifelse(treated == 0, "Non-carers", "Unpaid carers")) %>% 
  group_by(identifier) %>% 
  mutate(total_identified = n()) %>% 
  ungroup() %>% 
  group_by(identifier, total_identified, wimd) %>%
  count() %>%
  rename(count = n, level = wimd) %>%
  mutate(variable = "wimd") %>% 
  mutate(percentage = format(round(count/total_identified*100, 1),nsmall = 1))

save_csv_rds(npt_ucnc_wimd, demographics_path)


## Counts - yearly
df_npt_dedup_yrly <- demog_calendaryear_counts(df_npt_dedup)

## Counts - financial year
df_npt_dedup_financial_yr <- demog_financialyear_counts(df_npt_dedup)


# prepare tables for plots
npt_out_list <- demog_prep_plot_tables(df_npt_dedup_overall, "npt")
list2env(npt_out_list, envir = .GlobalEnv)


# prepare age frequency data
demog_carer_perc_age_hist_npt_hf <- demog_cntperc_age_npt %>% select(variable, factor_levels, count, total) %>% 
  group_by(variable,  factor_levels) %>% 
  mutate(count =sum(count), total = sum(total))  %>% distinct_all() %>% ungroup() %>%  
  mutate( percentage= calc_percentage(count, total),
          Age_Group = factor(factor_levels, levels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))

# statistics - HF
write_demog_stats(df_npt_dedup, "npt") 

## Graphs - overall counts
## ---- Demographics-npt-overall-counts ----

demog_overall_counts_npt_plot <- demog_plot_overall_counts(demog_cnt_npt, "NPT", nptlagp_palette)

## ---- Demographics-npt-overall-counts-exploration ----

plot_demog_lagp(demog_cntperc_sex_npt, "NPT", 1, "Sex", rev(sex_palette))

plot_demog_lagp(demog_cntperc_age_npt, "NPT", 1, "Age group", age_palette)

plot_demog_lagp(demog_cntperc_wimd_npt, "NPT", 1, "WIMD", wimd_palette)

plot_demog_lagp(demog_cntperc_ruc_npt, "NPT", 1, "Rurality", ruc_palette)

## Graphs - overall percentages

## ---- Demographics-npt-overall-percentage-hf ----

demog_carer_perc_sex_npt_hf_plot <- plot_demog_lagp(demog_cntperc_sex_npt, "NPT", 0, "Sex", rev(sex_palette))

demog_carer_perc_age_npt_hf_plot <- plot_demog_lagp(demog_cntperc_age_npt, "NPT", 0, "Age group", age_palette)

demog_carer_perc_age_hist_npt_hf_plot <- demog_plot_age_hist(demog_carer_perc_age_hist_npt_hf, "NPT", rev(age_palette))

demog_carer_perc_wimd_npt_hf_plot <- plot_demog_lagp(demog_cntperc_wimd_npt, "NPT", 0, "WIMD", wimd_palette)

demog_carer_perc_sexwimd_npt_hf_plot <- demog_plot_sexwimd_lagp(demog_cntperc_sexwimd_npt, "NPT", sexwimd_palette)

demog_carer_perc_ruc_npt_hf_plot <-plot_demog_lagp(demog_cntperc_ruc_npt, "NPT", 0, "Rurality", ruc_palette)



## ---- Demographics-npt-save-data ----
save_csv_rds( demog_carer_perc_age_hist_npt_hf, demographics_path)
# save_csv_rds( df_npt_dedup_overall, demographics_path)
# save_csv_rds( df_npt_dedup_yrly, demographics_path)
# save_csv_rds( df_npt_dedup_financial_yr, demographics_path)
# save_csv_rds( npt_demog_stats_data, demographics_path)

## ---- Demographics-npt-save-plots ----

save_ggplot(demog_overall_counts_npt_plot, demographics_path)
save_ggplot(demog_carer_perc_sex_npt_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_age_npt_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_age_hist_npt_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_wimd_npt_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_sexwimd_npt_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_ruc_npt_hf_plot, demographics_path)



