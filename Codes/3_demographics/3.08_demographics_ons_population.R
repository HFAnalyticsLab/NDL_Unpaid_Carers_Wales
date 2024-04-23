## ---- load_libraries_ons_carer_perc ---- 

pkgs <- c('dplyr', 'data.table', 'readr', 'tidyr', 'janitor')
lapply(pkgs, library, character.only = T)


## ---- load_data_ons_carer_perc ---- 

source(paste0(main_path, "1429_RODBC_login_20221031.R")) # login
source(paste0(main_path, "1429_general_functions.R")) # source general functions

## Read in dedup data
df_npt_dedup <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_DEDUP_GP_LA_NPT")
df_swan_dedup <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_DEDUP_GP_LA_SWANSEA")
df_denb_dedup <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_DEDUP_GP_LA_DENBIGHSHIRE")

## Read in ONS lookup
ons_age_raw <- read_csv(paste0(look_up_path, "ons_ukpopestimatesmid2020_age.csv"))

## Read in cohort count table
df_cohort_counts <- read_csv(paste0(lb_demog_path, "data/processed/1429_df_cohort_counts_lb.csv"))

## ---- functions_ons_carer_perc ----

count_carers <- function(df_dedup){
  count <- df_dedup %>%
    clean_names() %>%
    filter(between(first_identified_date, as.Date("2021-04-01"), as.Date("2022-03-31"))) %>%
    count()
}

## ---- data_prep_ons_carer_perc ---- 

la_names <- c("Neath Port Talbot", "Swansea", "Denbighshire") # add LAs of interest to vector

df_ons_prep <- ons_age_raw %>%
  filter(Name %in% la_names) %>% # filter by LAs of interest
  clean_names() %>% # clean the column names so they aren't numbers
  select(name, "x18":"x90") # select adult ages

df_ons <- df_ons_prep %>%
  mutate(ons_adult_pop = rowSums(df_ons_prep[,2:74])) %>% # sum all adult ages to get 18+ pop estimates
  select(name, ons_adult_pop) %>%
  arrange(factor(name, levels = c("Neath Port Talbot", "Swansea", "Denbighshire"), labels = c("npt", "swansea", "denbighshire")))

npt_count <- count_carers(df_npt_dedup)
swan_count <- count_carers(df_swan_dedup)
denb_count <- count_carers(df_denb_dedup)

count <- c(npt_count, swan_count, denb_count)

df_ons$carer_count_202122 <- as.numeric(count)

df_ons$percentage_202122 <- df_ons$carer_count_202122 / df_ons$ons_adult_pop * 100

df_cohort_counts_202122perc <- df_cohort_counts %>%
  merge(df_ons, all = TRUE)

## ---- save_data_ons_carer_perc ----
initials <- 'lb'
save_csv_rds(df_cohort_counts_202122perc, lb_demog_path)
