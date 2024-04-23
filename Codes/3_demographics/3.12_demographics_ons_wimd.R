## ---- load_libraries_ons_wimd ---- 

pkgs <- c('dplyr', 'data.table', 'readr', 'tidyr', 'janitor', 'ggplot2', 'stringr')
lapply(pkgs, library, character.only = T)

## ---- load_data_ons_wimd ---- 

## Run file_paths_lb.R

source(paste0(main_path,"1429_RODBC_login_20221031.R")) # login
source(paste0(main_path,"1429_general_functions.R")) # source general functions
source(paste0(peje_demog_path, "scripts/1429_demog_deduplicated_npt_script_peje_20221114.R")) # source Jerlyn's script for npt demographics.
source(paste0(peje_demog_path, "scripts/1429_demog_deduplicated_swansea_script_peje_20221114.R")) # source Jerlyn's script for swansea demographics
source(paste0(peje_demog_path, 'scripts/1429_demog_deduplicated_denbighshire_script_peje_20230421.R')) # source Jerlyn's script for denbighshire demographics
source(paste0(main_path, "1429_color_palette.R")) # source colour palette

ons_lsoa_raw <- read_csv(paste0(look_up_path, "ons_ukpopestimatesmid2020_lsoa.csv")) # read in ons estimates

lsoa_wimd_raw <- sqlQuery(sql, "SELECT * FROM SAILREFRV.WIMD2019_INDEX_AND_DOMAIN_RANKS_BY_SMALL_AREA") # read in lsoa-wimd lookup


## ---- functions_ons_wimd ----

source(paste0(lb_demog_path, 'functions/1429_ons_functions_lb_20221221.R'))

#run swansea/npt as lb, then denbighshire as ep
initials <- "lb"

## ---- data_prep_ons_wimd ----

df_ons_lsoa <- lsoa_data_prep(ons_lsoa_raw)

df_lsoa_wimd <- lsoa_wimd_raw %>% # lsoa-wimd mapping taken from eclipse table SAILREFRV.WIMD2019_INDEX_AND_DOMAIN_RANKS_BY_SMALL_AREA
  select(LSOA2011_CD, LSOA2011_DESC, LA_DESC, OVERALL_QUINTILE) %>% # select LSOA info and wimd quintile (inc. health)
  mutate(across(where(is.character), str_trim)) %>% # trim trailing spaces to link
  clean_names() %>% # clean names to remove spaces and convert to lower case
  rename(name = la_desc, 
         wimd = overall_quintile) # rename LA ID column to "name" for consistency with our data
  
df_ons_wimd <- df_ons_lsoa %>%
  left_join(df_lsoa_wimd, by = c("lsoa_code" = "lsoa2011_cd")) # join ons lsoa estimates to sail lsoa-wimd lookup

# df_ons_wimd %>% filter(is.na(lsoa2011_desc)) %>% count() # checking all ons rows link

df_ons_wimd <- df_ons_wimd %>% # calculate ons wimd estimates per lsoa
  group_by(name, wimd) %>% # group by la and wimd
  mutate(count_ons = sum(count)) %>% # sum ons estimates
  ungroup() %>% 
  select(name, wimd, count_ons) %>% # select columns
  distinct() %>% # remove duplicate rows
  group_by(name) %>%
  mutate(total_ons = sum(count_ons)) %>%
  arrange(name, wimd) %>% # order
  mutate(percentage_ons = as.numeric(formatC(count_ons / total_ons * 100, format = "f", digits = 1)), # calculate percentage per sex for each LA to 1 dp
         wimd = factor(wimd, 
                       levels = c("1", "2", "3", "4", "5"), 
                       labels = c("1 (most)", "2", "3", "4", "5 (least)"))) # change wimd to factor for plotting
  

## ---- data_prep_carers_ons_wimd

df_dedup_wimd_npt <- carer_demog_count(demog_cntperc_wimd_npt, "Neath Port Talbot", "wimd") # count the number of carers per wimd category in NPT
df_dedup_wimd_swansea <- carer_demog_count(demog_cntperc_wimd_swansea, "Swansea", "wimd") # count the number of carers per wimd category in Swansea
df_dedup_wimd_denbighshire <- carer_demog_count(demog_cntperc_wimd_denbigh, "Denbighshire", "wimd") # count the number of carers per wimd category in Denbighshire

df_dedup_wimd_npt_lagp <- carer_demog_count_lagp(demog_cntperc_wimd_npt, "Neath Port Talbot", "wimd") # count the number of carers per wimd category by GP/LA in NPT
df_dedup_wimd_swansea_lagp <- carer_demog_count_lagp(demog_cntperc_wimd_swansea, "Swansea", "wimd") # count the number of carers per wimd category by GP/LA in Swansea
df_dedup_wimd_denbighshire_lagp <- carer_demog_count_lagp(demog_cntperc_wimd_denbigh, "Denbighshire", "wimd") # count the number of carers per wimd category by GP/LA in Denbighshire

df_la_proportion_ons_wimd_npt <- carer_percentage_ons(df_dedup_wimd_npt, df_ons_wimd, "wimd") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_wimd_npt, lb_demog_path)

df_la_proportion_ons_wimd_swansea <- carer_percentage_ons(df_dedup_wimd_swansea, df_ons_wimd, "wimd") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_wimd_swansea, lb_demog_path)

df_la_proportion_ons_wimd_denbighshire <- carer_percentage_ons(df_dedup_wimd_denbighshire, df_ons_wimd, "wimd") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_wimd_denbighshire, lb_demog_path)

# combine all 3 LAs into one table
df_la_proportion_ons_wimd <- df_la_proportion_ons_wimd_npt %>%
  merge(df_la_proportion_ons_wimd_swansea, all = TRUE) %>%
  merge(df_la_proportion_ons_wimd_denbighshire, all = TRUE)

save_csv_rds(df_la_proportion_ons_wimd, lb_demog_path)

df_la_proportion_ons_wimd_npt_lagp <- carer_percentage_ons(df_dedup_wimd_npt_lagp, df_ons_wimd, "wimd") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_wimd_npt_lagp, lb_demog_path)
df_la_proportion_ons_wimd_swansea_lagp <- carer_percentage_ons(df_dedup_wimd_swansea_lagp, df_ons_wimd, "wimd") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_wimd_swansea_lagp, lb_demog_path)
df_la_proportion_ons_wimd_denbighshire_lagp <- carer_percentage_ons(df_dedup_wimd_denbighshire_lagp, df_ons_wimd, "wimd") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_wimd_denbighshire_lagp, lb_demog_path)


## ---- plot_npt_ons_wimd ----  
plot_npt_ons_wimd <- plot_ons(df_ons_wimd, "Neath Port Talbot", "wimd", "Deprivation quintile", wimd_palette) # overall ons estimates by wimd for NPT

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_wimd_lb_20230123.png"), plot_npt_ons_wimd)

## ---- plot_swansea_ons_wimd ----  
plot_swansea_ons_wimd <- plot_ons(df_ons_wimd, "Swansea", "wimd", "Deprivation quintile", wimd_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_wimd_lb_20230123.png"), plot_swansea_ons_wimd)

## ---- plot_gwynedd_ons_wimd ----  
plot_gwynedd_ons_wimd <- plot_ons(df_ons_wimd, "Gwynedd", "wimd", "Deprivation quintile", wimd_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_gwynedd_ons_wimd_lb_20230123.png"), plot_gwynedd_ons_wimd)

## ---- plot_denbighshire_ons_wimd ----  
plot_denbighshire_ons_wimd <- plot_ons(df_ons_wimd, "Denbighshire", "wimd", "Deprivation quintile", wimd_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_wimd_lb_20230123.png"), plot_denbighshire_ons_wimd)

## ---- plot_npt_carer_ons_wimd_proportions ----  
plot_npt_ons_wimd_carers <- plot_carer_ons(df_la_proportion_ons_wimd_npt, "Neath Port Talbot", "wimd", "deprivation quintile", "Deprivation quintile", wimd_palette) # overall ons estimates by wimd for NPT

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_wimd_carers_lb_20230123.png"), plot_npt_ons_wimd_carers)

## ---- plot_npt_carer_ons_age_proportions_lagp ----  
plot_npt_ons_wimd_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_wimd_npt_lagp, "Neath Port Talbot", "wimd", "deprivation quintile", "Deprivation quintile", wimd_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_wimd_carers_lagp_lb_20221216.png"), plot_npt_ons_wimd_carers_lagp)

## ---- plot_swansea_carer_ons_wimd_proportions ----  
plot_swansea_ons_wimd_carers <- plot_carer_ons(df_la_proportion_ons_wimd_swansea, "Swansea", "wimd", "deprivation quintile", "Deprivation quintile", wimd_palette) # overall ons estimates by wimd for Swansea

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_wimd_carers_lb_20230123.png"), plot_swansea_ons_wimd_carers)

## ---- plot_swansea_carer_ons_age_proportions_lagp ----  
plot_swansea_ons_wimd_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_wimd_swansea_lagp, "Swansea", "wimd", "deprivation quintile", "Deprivation quintile", wimd_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_wimd_carers_lagp_lb_20221216.png"), plot_swansea_ons_wimd_carers_lagp)

## ---- plot_denbighshire_carer_ons_wimd_proportions ----  
plot_denbighshire_ons_wimd_carers <- plot_carer_ons(df_la_proportion_ons_wimd_denbighshire, "Denbighshire", "wimd", "deprivation quintile", "Deprivation quintile", wimd_palette) # overall ons estimates by wimd for Denbighshire

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_wimd_carers_ep_20230518.png"), plot_denbighshire_ons_wimd_carers)

## ---- plot_denbighshire_carer_ons_age_proportions_lagp ----  
plot_denbighshire_ons_wimd_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_wimd_denbighshire_lagp, "Denbighshire", "wimd", "deprivation quintile", "Deprivation quintile", wimd_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_wimd_carers_lagp_ep_20230518.png"), plot_denbighshire_ons_wimd_carers_lagp)
