## ---- load_libraries_ons_age ---- 

pkgs <- c('dplyr', 'data.table', 'readr', 'tidyr', 'janitor', 'ggplot2', 'reshape2')
lapply(pkgs, library, character.only = T)

# Run file_paths_lb.R

## ---- load_data_ons_age ---- 

ons_male_raw <- read_csv(paste0(look_up_path, "ons_ukpopestimatesmid2020_male.csv"))

ons_female_raw <- read_csv(paste0(look_up_path, "ons_ukpopestimatesmid2020_female.csv"))

## ---- data_prep_standardisation ----

la_names <- c("Gwynedd", "Denbighshire", "Swansea", "Neath Port Talbot") # assign LAs of interest to vector

df_ons_male_age <- ons_male_raw %>%
  clean_names() %>% # clean names to remove caps and spaces
  filter(name %in% la_names) %>% # filter by LAs of interest
  select(code:x90) %>%
  mutate(sex = "male") # add field for sex and populate with male

df_ons_female_age <- ons_female_raw %>%
  clean_names() %>% # clean names to remove caps and spaces
  filter(name %in% la_names) %>% # filter by LAs of interest
  select(code:x90) %>%
  mutate(sex = "female") # add field for sex and populate with male

df_ons_sex_age <- rbind(df_ons_male_age, df_ons_female_age) %>%
  rowwise() %>%
  mutate("Under 40" = sum(c_across("x18":"x39")),
         "40-49" = sum(c_across("x40":"x49")),
         "50-59" = sum(c_across("x50":"x59")),
         "60-69" = sum(c_across("x60":"x69")),
         "70-79" = sum(c_across("x70":"x79")),
         "80+" = sum(c_across("x80":"x90")),
         adult_sex_total = sum(c_across("x18":"x90"))) %>%
  select(-(x0:x90), -code, -geography, -all_ages) %>%
  group_by(name) %>%
  mutate(adult_total = sum(adult_sex_total)) %>%
  ungroup()

melt_vars <- c("name", "adult_sex_total", "adult_total", "sex")

df_ons_sex_age_long <- df_ons_sex_age %>%
  melt(id.vars = melt_vars,
       variable.name = "age",
       value.name = "pop")

df_ons_sex_age_proportions <- df_ons_sex_age_long %>%
  mutate(proportion = pop / adult_total) %>%
  group_by(name) %>%
  mutate(#check = sum(proportion), 
         sex_code = ifelse(sex == "male", 1, 2)) %>%
  select(name, sex_code, age, pop)

# save as lookup table
saveRDS(df_ons_sex_age_proportions, paste0(look_up_path, "1429_standardpop_lkup_lb_20230228.rds"))