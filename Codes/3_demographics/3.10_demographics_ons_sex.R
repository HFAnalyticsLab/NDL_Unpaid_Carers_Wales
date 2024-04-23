## ---- load_libraries_ons_sex ---- 

pkgs <- c('dplyr', 'data.table', 'readr', 'tidyr', 'janitor', 'ggplot2')
lapply(pkgs, library, character.only = T)


## ---- load_data_ons_sex ---- 

# Run files_paths_lb.R
# Run filepaths.R

source(paste0(main_path,"1429_RODBC_login_20221031.R")) # login
source(paste0(main_path,"1429_general_functions.R")) # source general functions
source(paste0(lb_demog_path, 'scripts/1429_demog_deduplicated_npt_script_peje_20221114.R')) # source script for NPT demographics
source(paste0(lb_demog_path, 'scripts/1429_demog_deduplicated_swansea_script_peje_20221114.R')) # source script for Swansea demographics
source(paste0(lb_demog_path, 'scripts/1429_demog_deduplicated_denbighshire_script_peje_20230421.R')) # source script for Denbighshire demographics
source(paste0(main_path, "1429_color_palette.R")) # source colour palette

source(paste0(lb_demog_path, 'functions/1429_ons_functions_lb_20221221.R')) # source ONS demographics functions


ons_male_raw <- read_csv(paste0(look_up_path, "ons_ukpopestimatesmid2020_male.csv"))

ons_female_raw <- read_csv(paste0(look_up_path, "ons_ukpopestimatesmid2020_female.csv"))

initials <- "lb" # set initials for data save function

## ---- data_prep_ons_sex ----

la_names <- c("Gwynedd", "Denbighshire", "Swansea", "Neath Port Talbot") # assign LAs of interest to vector

df_ons_male <- ons_male_raw %>%
  clean_names() %>% # clean names to remove caps and spaces
  rowwise() %>%
  mutate(count_ons = sum(c_across("x18":"x90"))) %>%
  select(name, count_ons) %>% # select la name and total count fields
  filter(name %in% la_names) %>% # filter by LAs of interest
  mutate(sex = "male") # add field for sex and populate with male

df_ons_female <- ons_female_raw %>%
  clean_names() %>% # clean names to remove caps and spaces
  rowwise() %>%
  mutate(count_ons = sum(c_across("x18":"x90"))) %>%
  select(name, count_ons) %>% # select la name and total count fields
  filter(name %in% la_names) %>% # filter by LAs of interest
  mutate(sex = "female") # add field for sex and populate with female

df_ons_sex <- rbind(df_ons_male, df_ons_female) %>% # combine male and female ons count tables
  group_by(name) %>% # group by la
  mutate(sex = factor(sex, # assigns order for ggplot x axis. Factorising not yet incorporated into function 
                      levels = c('male', 'female'),
                      labels = c('Male', 'Female')), # capitalised to match peje unpaid carers
         total_ons = sum(count_ons)) %>% # calculate total for LA to use as denominator for sex percentage
  ungroup() %>%
  mutate(percentage_ons = as.numeric(formatC(count_ons / total_ons * 100, format = "f", digits = 1))) # calculate percentage per sex for each LA to 1 dp

save_csv_rds(df_ons_sex, lb_demog_path)


## ---- data_prep_carers_ons_sex ----

df_dedup_sex_npt <- carer_demog_count(demog_cntperc_sex_npt, "Neath Port Talbot", "sex") # count the number of carers per sex category in NPT
df_dedup_sex_swansea <- carer_demog_count(demog_cntperc_sex_swansea, "Swansea", "sex") # count the number of carers per sex category in Swansea
df_dedup_sex_denbighshire <- carer_demog_count(demog_cntperc_sex_denbigh, "Denbighshire", "sex") # count the number of carers per sex category in Denbighshire

df_dedup_sex_npt_lagp <- carer_demog_count_lagp(demog_cntperc_sex_npt, "Neath Port Talbot", "sex") # count the number of carers per sex category by GP/LA in NPT
df_dedup_sex_swansea_lagp <- carer_demog_count_lagp(demog_cntperc_sex_swansea, "Swansea", "sex") # count the number of carers per sex category by GP/LA in Swansea
df_dedup_sex_denbighshire_lagp <- carer_demog_count_lagp(demog_cntperc_sex_denbigh, "Denbighshire", "sex") # count the number of carers per sex category by GP/LA in Denbighshire

df_la_proportion_ons_sex_npt <- carer_percentage_ons(df_dedup_sex_npt, df_ons_sex, "sex") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_sex_npt, lb_demog_path)

df_la_proportion_ons_sex_swansea <- carer_percentage_ons(df_dedup_sex_swansea, df_ons_sex, "sex") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_sex_swansea, lb_demog_path)

df_la_proportion_ons_sex_denbighshire <- carer_percentage_ons(df_dedup_sex_denbighshire, df_ons_sex, "sex") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_sex_denbighshire, lb_demog_path)

df_la_proportion_ons_sex_npt_lagp <- carer_percentage_ons(df_dedup_sex_npt_lagp, df_ons_sex, "sex") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_sex_npt_lagp, lb_demog_path)

df_la_proportion_ons_sex_swansea_lagp <- carer_percentage_ons(df_dedup_sex_swansea_lagp, df_ons_sex, "sex") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_sex_swansea_lagp, lb_demog_path)

df_la_proportion_ons_sex_denbighshire_lagp <- carer_percentage_ons(df_dedup_sex_denbighshire_lagp, df_ons_sex, "sex") # calculate count as a percentage of ONS estimates
save_csv_rds(df_la_proportion_ons_sex_denbighshire_lagp, lb_demog_path)

## ---- plot_npt_ons_sex ----  
plot_npt_ons_sex <- plot_ons(df_ons_sex, "Neath Port Talbot", "sex", "Sex", rev(sex_palette)) # overall ons estimates by sex for NPT

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_sex_lb_20221216.png"), plot_npt_ons_sex)

## ---- plot_swansea_ons_sex ----  
plot_swansea_ons_sex <- plot_ons(df_ons_sex, 'Swansea', "sex", "Sex", rev(sex_palette)) # overall ons estimates by sex for Swansea

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_sex_lb_20221216.png"), plot_swansea_ons_sex)

## ---- plot_denbighshire_ons_sex ----  
plot_denbighshire_ons_sex <- plot_ons(df_ons_sex, 'Denbighshire', "sex", "Sex", rev(sex_palette)) # overall ons estimates by sex for Denbighshire

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_sex_lb_20221216.png"), plot_denbighshire_ons_sex)

## ---- plot_npt_carer_ons_proportions_sex ----  
plot_npt_ons_sex_carers <- plot_carer_ons(df_la_proportion_ons_sex_npt, 'Neath Port Talbot', "sex", "sex", "Sex", rev(sex_palette)) # Carer proportions of ONS estimates

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_sex_carers_lb_20221216.png"), plot_npt_ons_sex_carers)

## ---- plot_npt_carer_ons_sex_proportions_lagp ----  
plot_npt_ons_sex_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_sex_npt_lagp, "Neath Port Talbot", "sex", "sex group", "Sex", rev(sex_palette)) # Carer LA/GP proportions of ONS estimates

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_sex_carers_lagp_lb_20221216.png"), plot_npt_ons_sex_carers_lagp)

## ---- plot_swansea_carer_ons_proportions_sex ----  
plot_swansea_ons_sex_carers <- plot_carer_ons(df_la_proportion_ons_sex_swansea, 'Swansea', "sex", "sex", "Sex", rev(sex_palette)) # Carer proportions of ONS estimates

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_sex_carers_lb_20221216.png"), plot_swansea_ons_sex_carers)

## ---- plot_swansea_carer_ons_sex_proportions_lagp ----  
plot_swansea_ons_sex_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_sex_swansea_lagp, "Swansea", "sex", "sex group", "Sex", rev(sex_palette)) # Carer LA/GP proportions of ONS estimates

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_sex_carers_lagp_lb_20221216.png"), plot_swansea_ons_sex_carers_lagp)

## ---- plot_denbighshire_carer_ons_proportions_sex ----  
plot_denbighshire_ons_sex_carers <- plot_carer_ons(df_la_proportion_ons_sex_denbighshire, 'Denbighshire', "sex", "sex", "Sex", rev(sex_palette)) # Carer proportions of ONS estimates

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_sex_carers_ep_20230518.png"), plot_denbighshire_ons_sex_carers)

## ---- plot_denbighshire_carer_ons_sex_proportions_lagp ----  
plot_denbighshire_ons_sex_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_sex_denbighshire_lagp, "Denbighshire", "sex", "sex group", "Sex", rev(sex_palette)) # Carer LA/GP proportions of ONS estimates

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_sex_carers_lagp_ep_20230518.png"), plot_denbighshire_ons_sex_carers_lagp)
