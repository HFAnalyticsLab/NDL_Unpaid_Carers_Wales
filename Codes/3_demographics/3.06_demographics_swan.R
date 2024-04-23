#'This script creates identification timeline (yearly, quarterly and financial years) data tables and visuals 
#'for Swansea unpaid carers '#

## ---- Demographics-swansea-deduplicated-login ----
# Load libraries
library(odbc)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(busdater)
library(stringr)
library(broom)
library(gtsummary)
library(forcats)


initials <- ' '# your initials here
source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_RODBC_login_20221031.R'))
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(demographics_path, 'functions/1429_demog_functions_peje_20221114.R'))
source(paste0(demographics_path, 'functions/1429_Plot_Demographics_functions.R'))


## ---- Demographics-swansea-dataprep ----
## Import data
swansea_dedup_raw <- sqlQuery(sql, "select * from SAILW1429V.PEJE_DEDUP_GP_LA_SWANSEA")

# convert variable names to lower case
df_swansea_dedup <- swansea_dedup_raw 
df_swansea_dedup <- demog_dataprep(swansea_dedup_raw)

# calculate mean age
mean(df_swansea_dedup$age) 
# perform t-test on age
df_swansea_lagp_age_ttest <- tidy(t.test(age ~ factor(identifiedby, levels = c("LA", "GP")), df_swansea_dedup)) 
save_csv_rds(df_swansea_lagp_age_ttest, demographics_path) # save t-test results

## Counts - overall
df_swansea_dedup_overall <- demog_overall_counts(df_swansea_dedup)

# counts for unpaid carers
df_swansea_dedup_overall_carertype <- df_npt_dedup_overall %>% group_by(variable, level) %>% 
  filter(variable %in% c("age", "sex", "wimd", "ruc")) %>% 
  mutate(total_count = sum(count),
         total_identified = sum(total),
         total_perc = format(round(total_count/total_identified*100, 1),nsmall = 1)) %>% 
  select(variable, level, total_count, total_identified, total_perc) %>% 
  distinct_all()
save_csv_rds(df_swansea_dedup_overall_carertype, demographics_path)

# count and percentage for wimd - unpaid carer vs non-carers
swansea_matched <- sqlQuery(sql, "select *, AGE_INDEXDATE as AGE from SAILW1429V.PEJE_MATCHED_COHORT_SWANSEA_2")

swansea_matched_cleaned <- demog_dataprep(swansea_matched) # clean dataset

swansea_ucnc_wimd <- swansea_matched_cleaned %>% 
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

save_csv_rds(swansea_ucnc_wimd, demographics_path)


## Counts - yearly
df_swansea_dedup_yrly <- demog_calendaryear_counts(df_swansea_dedup)


## Counts - financial year
df_swansea_dedup_financial_yr <- demog_financialyear_counts(df_swansea_dedup)


# prepare tables for plots
swansea_out_list <- demog_prep_plot_tables(df_swansea_dedup_overall, "swansea")
list2env(swansea_out_list, envir = .GlobalEnv)


# prepare age frequency data
demog_carer_perc_age_hist_swansea_hf <- demog_cntperc_age_swansea %>% 
  select(variable, factor_levels, count, total) %>% 
  group_by(variable,  factor_levels) %>% 
  mutate(count =sum(count), total = sum(total))  %>% 
  distinct_all() %>% 
  ungroup() %>%  
  mutate( percentage= calc_percentage(count, total),
          Age_Group = factor(factor_levels, levels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+")))



# statistics - HF
write_demog_stats(df_swansea_dedup, "swansea")



## Graphs - overall counts
## ---- Demographics-swansea-overall-counts ----

demog_overall_counts_swansea_plot <- demog_plot_overall_counts(demog_cnt_swansea, "Swansea", swansealagp_palette)

## ---- Demographics-swansea-overall-counts-exploration ----

plot_demog_lagp(demog_cntperc_sex_swansea, "Swansea", 1, "Sex", rev(sex_palette))

plot_demog_lagp(demog_cntperc_age_swansea, "Swansea", 1, "Age group", age_palette)

plot_demog_lagp(demog_cntperc_wimd_swansea, "Swansea", 1, "WIMD", wimd_palette)

plot_demog_lagp(demog_cntperc_ruc_swansea, "Swansea", 1, "Rurality", ruc_palette)



## Graphs - overall percentages
## ---- Demographics-swansea-overall-percentage-hf ----

demog_carer_perc_sex_swansea_hf_plot <- plot_demog_lagp(demog_cntperc_sex_swansea, "Swansea", 0, "Sex", rev(sex_palette))

demog_carer_perc_age_swansea_hf_plot <- plot_demog_lagp(demog_cntperc_age_swansea, "Swansea", 0, "Age group", age_palette)

demog_carer_perc_age_hist_swansea_hf_plot <- demog_plot_age_hist(demog_carer_perc_age_hist_swansea_hf, "Swansea", rev(age_palette))

demog_carer_perc_wimd_swansea_hf_plot <- plot_demog_lagp(demog_cntperc_wimd_swansea, "Swansea", 0, "WIMD", wimd_palette)

demog_carer_perc_sexwimd_swansea_hf_plot <- demog_plot_sexwimd_lagp(demog_cntperc_sexwimd_swansea, "Swansea", sexwimd_palette)

demog_carer_perc_ruc_swansea_hf_plot <-plot_demog_lagp(demog_cntperc_ruc_swansea, "Swansea", 0, "Rurality", ruc_palette)


## ---- Demographics-swansea-save-data ----
save_csv_rds( demog_carer_perc_age_hist_swansea_hf, demographics_path)
# save_csv_rds(df_swansea_dedup_financial_yr, demographics_path)
# save_csv_rds(df_swansea_dedup_yrly, demographics_path)
# save_csv_rds( df_swansea_dedup_overall, demographics_path)


## ---- Demographics-swansea-save-plots ----
save_ggplot(demog_overall_counts_swansea_plot, demographics_path)
save_ggplot(demog_carer_perc_sex_swansea_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_age_swansea_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_age_hist_swansea_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_wimd_swansea_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_sexwimd_swansea_hf_plot, demographics_path)
save_ggplot(demog_carer_perc_ruc_swansea_hf_plot, demographics_path)