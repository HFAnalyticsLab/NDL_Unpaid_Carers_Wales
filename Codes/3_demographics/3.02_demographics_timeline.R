## ---- timeline-login ----
# date: '2022-10-04'
# converted from Rmd to .R

#' This script produces a count of those identified as carers over time.
#' - For LA identified carers, an identification timeline is available for the period for which data was provided.
#' - For GP identified carers, the time span has been extended to 2017-01-01 to 2022-06-30 to allow investigation of a longer time span.'#


## ---- timeline-dataprep ----
library(odbc)
library(dplyr)
library(ggplot2)
library(openxlsx)
library(busdater)
library(stringr)

initials <- ' # your initials here'
source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_RODBC_login_20221031.R'))
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(demographics_path, 'functions/1429_timeline_functions_peje_20221111.R'))



## Import tables

# Swansea LA
swansea_la_raw <- sqlQuery(sql, "select ALF_PE, SEX, AGE, FIRST_ASSESS AS EVENT_DT, RUC_DESC,  WIMD AS WIMD_DESC from SAILW1429V.LB_LA_CARER_SWANSEA")
# Swansea GP
swansea_gp_extended_raw <- sqlQuery(sql, "select ALF_PE,SEX, LA_NAME, RUC11 AS RUC_DESC, WIMD_2019_QUINTILE AS WIMD_DESC, EVENT_DT, AGE from SAILW1429V.LB_GPCARER_SWANSEA_EXTENDED")


# NPT LA
npt_la_raw <- sqlQuery(sql, "select CARER_ALF_PE AS ALF_PE, SEX_CARER AS SEX, AGE_CARER AS AGE, FIRST_CARER_ASSMT AS EVENT_DT, WIMD_CARER AS WIMD_DESC, RUC_DESC_CARER AS RUC_DESC, TOTAL_NUM_ASSESSMENTS from SAILW1429V.LB_LA_CARER_NPT")
# NPT GP
npt_gp_extended_raw <- sqlQuery(sql, "select ALF_PE, SEX, AGE, EVENT_DT, RUC11 AS RUC_DESC, WIMD_2019_QUINTILE AS WIMD_DESC from SAILW1429V.LB_GPCARER_NPT_EXTENDED")


# Denbighshire LA
denbighshire_la_raw <- sqlQuery(sql, "select ALF_PE, SEX, AGE, REFERRAL_DATE AS EVENT_DT, RUC_DESC,  WIMD AS WIMD_DESC from SAILW1429V.LB_LA_CARER_DEN")
# Denbighshire GP
denbighshire_gp_extended_raw <- sqlQuery(sql, "select ALF_PE, SEX, AGE, EVENT_DT, RUC11 AS RUC_DESC, WIMD_2019_QUINTILE AS WIMD_DESC from SAILW1429V.LB_GPCARER_DENBIGHSHIRE_EXTENDED")


# Gwynedd GP
gwynedd_gp_extended_raw <- sqlQuery(sql, "select ALF_PE, SEX, AGE, EVENT_DT, RUC11 AS RUC_DESC, WIMD_2019_QUINTILE AS WIMD_DESC from SAILW1429V.LB_GPCARER_GWYNEDD_EXTENDED")


# All Wales
allwales_gp_extended_raw <- sqlQuery(sql, "select ALF_PE, SEX, AGE, EVENT_DT,LA_NAME, RUC11 AS RUC_DESC, WIMD_2019_QUINTILE AS WIMD_DESC from SAILW1429V.LB_GPCARER_ALLWALES_EXTENDED")


## add age group, financial year, calendar year, calendar month and calendar quarter

# Swansea LA
# convert variable names to lower case
df_swansea_la <- timeline_dataprep(swansea_la_raw)  
# Swansea extended GP
df_swansea_gp_extended <- timeline_dataprep(swansea_gp_extended_raw)


# NPT LA
# **NOTE:** NPT data set is one row per carer-client pairing, not one row per carer (some carers provide care to multiple clients). In order to get a carer count, need to distinct on carer. 
df_npt_la <- timeline_dataprep(npt_la_raw )
# remove all client based info so only one row per carer for counting
df_npt_la_carers <- df_npt_la %>%
  select(alf_pe:total_num_assessments, age_group:index_quarter) %>%
  distinct()

# NPT extended GP
df_npt_gp_extended <- timeline_dataprep(npt_gp_extended_raw)



# Denbighshire LA
# convert variable names to lower case
df_denbighshire_la <- timeline_dataprep(denbighshire_la_raw)  
# Denbighshire extended GP
df_denbighshire_gp_extended <- timeline_dataprep(denbighshire_gp_extended_raw)



# Gwynedd extended GP
df_gwynedd_gp_extended <- timeline_dataprep(gwynedd_gp_extended_raw)


# All Wales extended GP
df_allwales_gp_extended <- timeline_dataprep(allwales_gp_extended_raw)


## ---- timeline-addcounts-swansea ----

## Counts - financial year Swansea LA 
df_swansea_la_yr <- timeline_countsby_financialyr(df_swansea_la, "swansea_la")
# financial year Swansea GP
df_swansea_gp_extended_yr <- timeline_countsby_financialyr(df_swansea_gp_extended, "swansea_gp")
# join tables
df_swansea_counts_yr <- df_swansea_la_yr %>%
  merge(df_swansea_gp_extended_yr, all = TRUE)


## Counts quarterly Swansea

# Swansea LA counts per quarter and year
df_swansea_la_index_quarter <- counts_quarterly(df_swansea_la, "swansea_la")

# Swansea GP counts per quarter by demographics
df_swansea_gp_extended_index_quarter <- counts_quarterly(df_swansea_gp_extended, "swansea_gp")

df_swansea_gp_extended_quarter <- timeline_countsby_quarter(df_swansea_gp_extended ,"swansea_gp")


## ---- timeline-addcounts-npt ----

## Counts - financial year NPT

# **NOTE:** using dataset with client info removed and distincted, so one row per carer for counting.
# - LA counts by demographics
df_npt_la_yr <- timeline_countsby_financialyr(df_npt_la_carers, "npt_la")

# GP counts by demographics
df_npt_gp_extended_yr <- timeline_countsby_financialyr(df_npt_gp_extended, "npt_gp")

# Merge NPT yearly tables
df_npt_counts_yr <- df_npt_la_yr %>%
  merge(df_npt_gp_extended_yr, all = TRUE)


## Counts quarterly NPT 

# count by index quarter and year
df_npt_gp_extended_index_quarter <- counts_quarterly(df_npt_gp_extended, "npt_gp")

df_npt_gp_extended_quarter <-timeline_countsby_quarter(df_npt_gp_extended, "npt_gp")


## ---- timeline-addcounts-denbigshshire ----

## Counts - financial year Denbighshire
df_denbighshire_la_yr <- timeline_countsby_financialyr(df_denbighshire_la, "denbighshire_la")
# GP counts by demographics
df_denbighshire_gp_extended_yr <- timeline_countsby_financialyr(df_denbighshire_gp_extended, "denbighshire_gp")

# Merge denbigshshire yearly tables
df_denbighshire_counts_yr <- df_denbighshire_la_yr %>%
  merge(df_denbighshire_gp_extended_yr, all = TRUE)

## Counts quarterly Denbighshire
df_denbighshire_la_index_quarter <- counts_quarterly(df_denbighshire_la, "denbighshire_la")

# count by index quarter and year
df_denbighshire_gp_extended_index_quarter <- counts_quarterly(df_denbighshire_gp_extended, "denbighshire_gp")

df_denbighshire_gp_extended_quarter <-timeline_countsby_quarter(df_denbighshire_gp_extended, "denbighshire_gp")

## ---- timeline-addcounts-gwynedd ----

## Counts - financial year gwynedd

# GP counts by demographics
df_gwynedd_gp_extended_yr <- timeline_countsby_financialyr(df_gwynedd_gp_extended, "gwynedd_gp")


## Counts quarterly gwynedd
# count by index quarter and year
df_gwynedd_gp_extended_index_quarter <- counts_quarterly(df_gwynedd_gp_extended, "gwynedd_gp")

## ---- timeline-addcounts-allwales ----

## Counts - financial year all wales

# counts for all of Wales by LA and demographics (done to see if Swansea is an anomaly in the wider context of all of Wales)

# Count by year
df_allwales_gp_extended_financial_yr <- df_allwales_gp_extended %>%
  group_by(la_name, financial_yr) %>%
  count() %>%
  rename(count = n) %>%
  mutate(variable = "year", level = "year", cohort = "allwales_gp") %>%
  arrange(financial_yr)

# Count by year and sex
df_allwales_gp_extended_financial_yr_sex <- df_allwales_gp_extended %>%
  group_by(la_name, sex, financial_yr) %>%
  count() %>%
  rename(count = n, level = sex) %>%
  mutate(variable = "sex", cohort = "allwales_gp") %>%
  arrange(level, financial_yr)

# Count by year and age group
df_allwales_gp_extended_financial_yr_age <- df_allwales_gp_extended %>%
  group_by(la_name, age_group, financial_yr) %>%
  count() %>%
  rename(count = n, level = age_group) %>%
  mutate(variable = "age_group", cohort = "allwales_gp") %>%
  arrange(level, financial_yr)

# count by year and WIMD
df_allwales_gp_extended_financial_yr_wimd <- df_allwales_gp_extended %>%
  group_by(la_name, wimd_desc, financial_yr) %>%
  count() %>%
  rename(count = n, level = wimd_desc) %>%
  mutate(variable = "wimd", cohort = "allwales_gp") %>%
  arrange(level, financial_yr)

# count by year and RUC
df_allwales_gp_extended_financial_yr_ruc <- df_allwales_gp_extended %>%
  group_by(la_name, ruc_desc, financial_yr) %>%
  count() %>%
  rename(count = n, level = ruc_desc) %>%
  mutate(variable = "ruc", cohort = "allwales_gp") %>%
  arrange(level, financial_yr)

# merge
df_allwales_gp_extended_yr <- df_allwales_gp_extended_financial_yr %>%
  merge(df_allwales_gp_extended_financial_yr_sex, all = TRUE) %>%
  merge(df_allwales_gp_extended_financial_yr_age, all = TRUE) %>%
  merge(df_allwales_gp_extended_financial_yr_wimd, all = TRUE) %>%
  merge(df_allwales_gp_extended_financial_yr_ruc, all = TRUE) %>%
  select(variable, level, everything()) %>%
  arrange(variable, level)

# Remove interim tables
rm(df_allwales_gp_extended_financial_yr, df_allwales_gp_extended_financial_yr_sex, df_allwales_gp_extended_financial_yr_age, df_allwales_gp_extended_financial_yr_wimd, df_allwales_gp_extended_financial_yr_ruc)


## Counts quarterly all wales

# count by index quarter and year
df_allwales_gp_extended_index_quarter <- df_allwales_gp_extended %>%
  group_by(la_name, index_yr, index_quarter) %>%
  count() %>%
  rename(count = n) %>%
  mutate(variable = "year", level = "year", cohort = "allwales_gp") %>%
  arrange(index_yr, index_quarter)


## ---- timeline-addcounts-merge ----

# Merge LAs into one table - yearly
df_carer_counts_yr <- df_swansea_counts_yr %>%
  merge(df_npt_counts_yr, all = TRUE) %>%
  merge(df_denbighshire_counts_yr, all = TRUE) %>%
  merge(df_gwynedd_gp_extended_yr, all = TRUE)


# Merge LAs into one table - quarterly
df_carer_counts_quarter <- df_swansea_gp_extended_index_quarter %>%
  merge(df_npt_gp_extended_index_quarter, all = TRUE) %>%
  merge(df_denbighshire_gp_extended_index_quarter, all = TRUE) %>%
  merge(df_gwynedd_gp_extended_index_quarter, all = TRUE) %>%
  select(variable, level, everything()) %>%
  arrange(variable, level)





## Graphs yearly
## ---- timeline-plots-yearly ----
timeline_plots_yearly_npt_data <- df_carer_counts_yr %>%
  filter(variable == "year" & (cohort %in% c("npt_gp", "npt_la")) ) %>%
  mutate(LA = gsub('_la', '', cohort), 
         LA = gsub('_gp', '', LA))

timeline_plots_npt_yearly <- timeline_plots_yearly_npt_data %>% 
  ggplot() +
  geom_line(aes(x = financial_yr, 
                y = count, 
                colour = factor(LA, levels = c('npt'), labels = c('NPT')),
                group = cohort,
                linetype = cohort), linewidth = 1) +
  labs(x = "Financial Year",
       y = "Number of unpaid carers identified",
       title = "Number of unpaid carers identified in NPT per \n financial year  from 2017/18 to 2021/22. \n (Solid line = identified by GP, dotted line = identified by LA)",
       colour = "Geographical region") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 1200),breaks = seq(0, 1200, 200)) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values = c("solid", "dotted"), guide = "none") +
  scale_color_manual(values = c("#e17055", "#e17055")) 

save_csv_rds( timeline_plots_yearly_npt_data, demographics_path)
save_ggplot(timeline_plots_npt_yearly, demographics_path)

## ---- timeline-plots-yearly-demographics ----
## sex 
df_carer_counts_yr %>%
  filter(variable == "sex") %>% 
  mutate(
    factor_levels = factor(level, levels = c("1", "2"), labels = c("Male", "Female"))
         )  %>% timeline_plots_yrly_demog()

## age group 
df_carer_counts_yr %>%
  filter(variable == "age_group") %>% 
  mutate(factor_levels = factor(level, levels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+"))
         ) %>% timeline_plots_yrly_demog()


## wimd 
df_carer_counts_yr %>%
  filter(variable == "wimd") %>% 
  mutate(
    factor_levels = factor(level, levels = c("1", "2", "3", "4", "5"), labels = c("1 (most)", "2", "3", "4", "5 (least)"))
         ) %>% timeline_plots_yrly_demog()


## ruc
df_carer_counts_yr %>%
  filter(variable == "ruc") %>% 
  mutate(
    factor_levels = factor(level, levels = c("Urban city and town", "Rural town and fringe", "Rural town and fringe in a sparse setting", "Rural village and dispersed", "Rural village and dispersed in a sparse setting"))
  ) %>% timeline_plots_yrly_demog()


## Graphs yearly - all Wales LAs

# **NOTE:** added "." to "9180." for all Wales SQL table to check it's not just this code driving the difference in Swansea.  

## ---- timeline-plots-yearly-allwales ----
# By year
df_allwales_gp_extended_yr %>%
  filter(variable == "year") %>%
  ggplot() +
  geom_line(aes(x = financial_yr, 
                y = count, 
                colour = la_name,
                group = la_name)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) + labs(title = "All Wales")




## Graphs quarterly Swansea

## ---- timeline-plots-swansea-quarterly ----
#  By quarter

df_swansea_gp_extended_quarter %>%
  filter(variable == "year")  %>% 
  timeline_plots_la_qtrly()


## ---- timeline-plots-swansea-quarterly-lagp ----
df_swansea_lagp_qtrly <- rbind(df_swansea_gp_extended_quarter[ df_swansea_gp_extended_quarter$variable == "year",names(df_swansea_la_index_quarter)], df_swansea_la_index_quarter) %>%
  mutate(LA = gsub('_la', '', cohort),
         LA = gsub('_gp', '', LA))

swansea_lagp_qtrly_plot <- df_swansea_lagp_qtrly %>% 
  ggplot() +
  geom_line(aes(x = interaction(index_quarter, index_yr),
                y = count,
                group = cohort,
                colour = factor(LA, levels = "swansea", labels = "Swansea"),
                linetype = cohort),
            linewidth = 1) +
  labs(x = "Financial quarter (quarter.year)",
       y = "Number of unpaid carers identified",
       colour = "Geographical region",
       title = "Number of unpaid carers identified in Swansea by financial quarter and year \n from Apr 2017 to Mar 2022 \n (Solid line = identified by GP, dotted line = identified by LA)") +
  scale_linetype_manual(values = c("solid","dotted"), guide = "none") +
  scale_color_manual(values = c("#F39C12")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1200),
                     breaks = seq(0, 1200, 200)) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black")) 
  

save_csv_rds( df_swansea_lagp_qtrly, demographics_path)
save_ggplot(swansea_lagp_qtrly_plot, demographics_path)


## ---- timeline-plots-swansea-quarterly-demographics ----
# By quarter and sex
df_swansea_gp_extended_quarter %>%
  filter(variable == "sex") %>%
  timeline_plots_qtrly_demog()

# By quarter and age
df_swansea_gp_extended_quarter %>%
  filter(variable == "age_group") %>%
  timeline_plots_qtrly_demog()


# By quarter and wimd
df_swansea_gp_extended_quarter %>%
  filter(variable == "wimd") %>%
  timeline_plots_qtrly_demog()


# By quarter and ruc
df_swansea_gp_extended_quarter %>%
  filter(variable == "ruc") %>%
  timeline_plots_qtrly_demog()



## Graphs quarterly NPT

## ---- timeline-plots-npt-quarterly ----
#  By quarter
df_npt_gp_extended_quarter %>%
  filter(variable == "year") %>% 
  timeline_plots_la_qtrly()

## ---- timeline-plots-npt-quarterly-demographics ----
# By quarter and sex
df_npt_gp_extended_quarter %>%
  filter(variable == "sex") %>%
  timeline_plots_qtrly_demog()

# By quarter and age
df_npt_gp_extended_quarter %>%
  filter(variable == "age_group") %>%
  timeline_plots_qtrly_demog()


# By quarter and wimd
df_npt_gp_extended_quarter %>%
  filter(variable == "wimd") %>%
  timeline_plots_qtrly_demog()


# By quarter and ruc
df_npt_gp_extended_quarter %>%
  filter(variable == "ruc") %>%
  timeline_plots_qtrly_demog()







## Graphs quarterly - all Wales
## ---- timeline-plots-allwales-quarterly ----
#  By quarter
df_allwales_gp_extended_index_quarter %>%
  ggplot() +
  geom_line(aes(x = interaction(index_quarter, index_yr),
                y = count,
                group = la_name,
                colour = la_name)) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  )




## ---- timeline-plots-denbighshire-yearly ----
timeline_plots_yearly_den_data <- df_carer_counts_yr %>%
  filter(variable == "year" & (cohort %in% c("denbighshire_gp", "denbighshire_la")) ) %>%
  mutate(LA = gsub('_la', '', cohort), LA = gsub('_gp', '', LA))

timeline_plots_denbighshire_yearly <- timeline_plots_yearly_den_data %>% 
  ggplot() +
  geom_line(aes(x = financial_yr, 
                y = count, 
                colour = factor(LA, levels = c('denbighshire'), labels = c('Denbighshire')),
                group = cohort,
                linetype = cohort), linewidth = 1) +
  labs(x = "Financial Year",
       y = "Number of unpaid carers identified",
       title = "Number of unpaid carers identified in Denbighshire per \n financial year  from 2017/18 to 2021/22. \n (Solid line = identified by GP, dotted line = identified by LA)",
       colour = "Geographical region") +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 1200),breaks = seq(0, 1200, 200)) +
  theme( axis.text.x = element_text(angle = 45, hjust = 1), 
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         panel.background = element_blank(), 
         axis.line = element_line(colour = "black")) +
  scale_linetype_manual(values = c("solid", "dotted"), guide = "none") +
  scale_color_manual(values = c("#6D214F", "#6D214F")) 

save_csv_rds( timeline_plots_yearly_den_data, demographics_path)
save_ggplot(timeline_plots_denbighshire_yearly, demographics_path)





## ---- timeline-plots-denbighshire-quarterly ----
#  By quarter

df_denbighshire_gp_extended_quarter %>%
  filter(variable == "year")  %>% 
  timeline_plots_la_qtrly()



## ---- timeline-plots-denbighshire-quarterly-lagp ----
df_denbighshire_lagp_qtrly <- rbind(df_denbighshire_gp_extended_quarter[ df_denbighshire_gp_extended_quarter$variable == "year",names(df_denbighshire_la_index_quarter)], df_denbighshire_la_index_quarter) %>%
  mutate(LA = gsub('_la', '', cohort), 
         LA = gsub('_gp', '', LA))

denbighshire_lagp_qtrly_plot <- df_denbighshire_lagp_qtrly %>% 
  ggplot() +
  geom_line(aes(x = interaction(index_quarter, index_yr),
                y = count,
                group = cohort,
                colour = factor(LA, levels = "denbighshire", labels = "Denbighshire"),
                linetype = cohort),
            linewidth = 1) +
  labs(x = "Financial quarter (quarter.year)",
       y = "Number of unpaid carers identified",
       colour = "Geographical region",
       title = "Number of unpaid carers identified in Denbighshire by financial quarter and year \n from Apr 2017 to Mar 2022 \n (Solid line = identified by GP, dotted line = identified by LA)") +
  scale_linetype_manual(values = c("solid","dotted"), guide = "none") +
  scale_color_manual(values = c("#6D214F")) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1200),
                     breaks = seq(0, 1200, 200)) + 
  theme( axis.text.x = element_text(angle = 45, hjust = 1),
         panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         axis.line = element_line(colour = "black")) 


save_csv_rds( df_denbighshire_lagp_qtrly, demographics_path)
save_ggplot(denbighshire_lagp_qtrly_plot, demographics_path)



## ---- timeline-plots-denbighshire-quarterly-demographics ----
# By quarter and sex
df_denbighshire_gp_extended_quarter %>%
  filter(variable == "sex") %>%
  timeline_plots_qtrly_demog()

# By quarter and age
df_denbighshire_gp_extended_quarter %>%
  filter(variable == "age_group") %>%
  timeline_plots_qtrly_demog()


# By quarter and wimd
df_denbighshire_gp_extended_quarter %>%
  filter(variable == "wimd") %>%
  timeline_plots_qtrly_demog()


# By quarter and ruc
df_denbighshire_gp_extended_quarter %>%
  filter(variable == "ruc") %>%
  timeline_plots_qtrly_demog()

