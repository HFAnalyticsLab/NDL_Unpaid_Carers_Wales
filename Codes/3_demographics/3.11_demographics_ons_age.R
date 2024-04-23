## ---- load_libraries_ons_age ---- 

pkgs <- c('dplyr', 'data.table', 'readr', 'tidyr', 'janitor', 'ggplot2', 'forcats')
lapply(pkgs, library, character.only = T)


## ---- load_data_ons_age ---- 

## Run file_paths_lb.R

source(paste0(main_path,"1429_RODBC_login_20221031.R")) # login
source(paste0(main_path,"1429_general_functions.R")) # source general functions
source(paste0(peje_demog_path, 'scripts/1429_demog_deduplicated_npt_script_peje_20221114.R')) # source script for NPT demographics
source(paste0(peje_demog_path, 'scripts/1429_demog_deduplicated_swansea_script_peje_20221114.R')) # source script for Swansea demographics
source(paste0(peje_demog_path, 'scripts/1429_demog_deduplicated_denbighshire_script_peje_20230421.R')) # source script for Denbighshire demographics
source(paste0(main_path, "1429_color_palette.R")) # source colour palette

source(paste0(lb_demog_path, 'functions/1429_ons_functions_lb_20221221.R')) # source ONS demographics functions

ons_age_raw <- read_csv(paste0(look_up_path, "ons_ukpopestimatesmid2020_age.csv")) # load ONS look up data

initials <- "lb" # set initials for data save function

## ---- data_prep_ons_age ----

la_names <- c("Denbighshire", "Swansea", "Neath Port Talbot") # add LAs of interest to vector

df_ons_age <- ons_age_raw %>%
  filter(Name %in% la_names) %>% # filter by LAs of interest
  clean_names() %>% # clean the column names so they aren't numbers
  select(name, "x18":"x90") %>% # remove unnecessary columns
  pivot_longer(!name, # pivot so age is a variable
               names_to = "age",
               values_to = "count_ons") %>%
  mutate(age = gsub('x', '', age), # remove the "x"s added by the clean_names function
         age = as.numeric(age), # format age as a numeric
         age = cut(age, # mutate age group column with our age groups
                         breaks = c(17, 39, 49, 59, 69, 79, Inf),
                         labels = c("Under 40", "40-49", "50-59", "60-69", "70-79", "80+"))) %>%
  group_by(name, age) %>% # group by la and age group
  mutate(count_ons = sum(count_ons)) %>% # count number of people by age group per LA
  distinct() %>%
  ungroup() %>%
  group_by(name) %>% # group by la
  mutate(total_ons = sum(count_ons), # calculate totals per la
         percentage_ons = as.numeric(formatC(count_ons / total_ons * 100, format = "f", digits = 1))) %>% # calculate percentage per age group per la to 1 dp
  arrange(name)

save_csv_rds(df_ons_age, lb_demog_path)


## ---- data_prep_carers_ons_age ----

# count the number of carers per age category
df_dedup_age_npt <- carer_demog_count(demog_cntperc_age_npt, "Neath Port Talbot", "age") # NPT

df_dedup_age_swansea <- carer_demog_count(demog_cntperc_age_swansea, "Swansea", "age") # Swansea

df_dedup_age_denbighshire <- carer_demog_count(demog_cntperc_age_denbigh, 'Denbighshire', 'age') # Denbighshire


# count the number of carers per age category by GP/LA 
df_dedup_age_npt_lagp <- carer_demog_count_lagp(demog_cntperc_age_npt, "Neath Port Talbot", "age") # NPT 

df_dedup_age_swansea_lagp <- carer_demog_count_lagp(demog_cntperc_age_swansea, "Swansea", "age") # Swansea

df_dedup_age_denbighshire_lagp <- carer_demog_count_lagp(demog_cntperc_age_denbigh, "Denbighshire", "age") # Denbighshire


# mask small counts
df_dedup_age_denbighshire_lagp_masked <- df_dedup_age_denbighshire_lagp %>% 
  mutate(count = ifelse(level == "40-49" & identifiedby == "LA", 10, count),
         count = ifelse(level == "Under 40" & identifiedby == "LA", 10, count))


# calculate count as a percentage of ONS estimates
df_la_proportion_ons_age_npt <- carer_percentage_ons(df_dedup_age_npt, df_ons_age, "age") 

df_la_proportion_ons_age_swansea <- carer_percentage_ons(df_dedup_age_swansea, df_ons_age, "age")

df_la_proportion_ons_age_denbighshire <- carer_percentage_ons(df_dedup_age_denbighshire, df_ons_age, "age")


# save
save_csv_rds(df_la_proportion_ons_age_npt, lb_demog_path)

save_csv_rds(df_la_proportion_ons_age_swansea, lb_demog_path)

save_csv_rds(df_la_proportion_ons_age_denbighshire, lb_demog_path)


# calculate LA/GP count as a percentage of ONS estimates
df_la_proportion_ons_age_npt_lagp <- carer_percentage_ons(df_dedup_age_npt_lagp, df_ons_age, "age")

df_la_proportion_ons_age_swansea_lagp <- carer_percentage_ons(df_dedup_age_swansea_lagp, df_ons_age, "age")

df_la_proportion_ons_age_denbighshire_lagp <- carer_percentage_ons(df_dedup_age_denbighshire_lagp_masked, df_ons_age, "age") 


## ---- plot_npt_ons_age ----  
plot_npt_ons_age <- plot_ons(df_ons_age, "Neath Port Talbot", "age", "Age group", age_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_age_lb_20221216.png"), plot_npt_ons_age)

## ---- plot_swansea_ons_age ----  
plot_swansea_ons_age <- plot_ons(df_ons_age, "Swansea", "age", "Age group", age_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_age_lb_20221216.png"), plot_swansea_ons_age)

## ---- plot_denbighshire_ons_age ----  
plot_denbighshire_ons_age <- plot_ons(df_ons_age, "Denbighshire", "age", "Age group", age_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_age_lb_20221216.png"), plot_denbighshire_ons_age)

## ---- plot_npt_carer_ons_age_proportions ----  
plot_npt_ons_age_carers <- plot_carer_ons(df_la_proportion_ons_age_npt, "Neath Port Talbot", "age", "age group", "Age group", age_palette) # overall ons estimates by age for NPT

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_age_carers_lb_20221216.png"), plot_npt_ons_age_carers)

## ---- plot_npt_carer_ons_age_proportions_lagp ----  
plot_npt_ons_age_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_age_npt_lagp, "Neath Port Talbot", "age", "age group", "Age group", npt_lagpcarers_palette)

ggsave(paste0(lb_demog_path, "outputs/1429_plot_npt_ons_age_carers_lagp_lb_20221216.png"), plot_npt_ons_age_carers_lagp)

## ---- plot_swansea_carer_ons_age_proportions ----  
plot_swansea_ons_age_carers <- plot_carer_ons(df_la_proportion_ons_age_swansea, "Swansea", "age", "age group", "Age group", age_palette) # overall ons estimates by age for Swansea

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_age_carers_lb_20221216.png"), plot_swansea_ons_age_carers)

## ---- plot_swansea_carer_ons_age_proportions_lagp ----  
plot_swansea_ons_age_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_age_swansea_lagp, "Swansea", "age", "age group", "Age group", swansea_lagpcarers_palette) # overall ons estimates by age for Swansea

ggsave(paste0(lb_demog_path, "outputs/1429_plot_swansea_ons_age_carers_lagp_lb_20221216.png"), plot_swansea_ons_age_carers_lagp)

## ---- plot_denbighshire_carer_ons_age_proportions ----  
plot_denbighshire_ons_age_carers <- plot_carer_ons(df_la_proportion_ons_age_denbighshire, "Denbighshire", "age", "age group", "Age group", age_palette) # overall ons estimates by age for Denbighshire

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_age_carers_ep_20221216.png"), plot_denbighshire_ons_age_carers)

## ---- plot_denbigshire_carer_ons_age_proportions_lagp ----  
plot_denbighshire_ons_age_carers_lagp <- plot_carer_ons_lagp(df_la_proportion_ons_age_denbighshire_lagp, "Denbighshire", "age", "age group", "Age group", denbighshire_lagpcarers_palette) # overall ons estimates by age for Denbighshire

ggsave(paste0(lb_demog_path, "outputs/1429_plot_denbighshire_ons_age_carers_lagp_ep_20221216.png"), plot_denbighshire_ons_age_carers_lagp)

## ---- plot_facetted_carer_ons_age_proportions_lagp ---- 

# Used for 4 pager
df_la_proportion_ons_age_lagp <- df_la_proportion_ons_age_npt_lagp %>%
  merge(df_la_proportion_ons_age_swansea_lagp, all = TRUE) %>%
  merge(df_la_proportion_ons_age_denbighshire_lagp, all = TRUE)

# save data for export
save_csv_rds(df_la_proportion_ons_age_lagp, lb_demog_path)

top_value <- df_la_proportion_ons_age_lagp %>%
  group_by(age, name) %>%
  filter(carer_proportion == max(carer_proportion))

plot_ons_age_carers_lagp <- df_la_proportion_ons_age_lagp %>%
  ggplot(aes(x = age, 
             y = carer_proportion, 
             fill = factor(identifiedby, levels = c("LA", "GP"), labels = c("LA-identified", "GP-identified")))) +
  geom_col(position = "dodge") + 
  geom_blank(data = top_value, aes(x = age,
                                   y = carer_proportion * 1.1,
                                   label = carer_proportion)) + # plot blank geom above highest y value so geom_text is not cut off
  labs(title = paste('Percentage of general population identified as unpaid carers, by age group and LA/GP \n(based on ONS 2020 adult MYE)', sep = " "),
       x = "Age group",
       y = 'Percentage (%)',
       fill = "Unpaid carers cohort") +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_discrete(limits = rev) + # underlying data is in reverse order for stack plots, so revert back
  geom_text(aes(x = age,  
                y = carer_proportion, 
                label = sprintf("%.1f", carer_proportion), 
                group = factor(identifiedby, levels = c("LA", "GP"), labels = c("LA-identified", "GP-identified"))),
            position = position_dodge(width = 1),
            size = 3,
            vjust = -0.5) +
  facet_grid(~ factor(name, levels = c("Neath Port Talbot", "Swansea", "Denbighshire")), switch = "both") + # order la facets
  scale_fill_manual(values = denbighshire_lagpcarers_palette) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.placement = "outside",
    strip.background = element_blank(),
  )

ggsave(paste0(lb_demog_path, "outputs/1429_plot_ons_age_carers_lagp_lb_20230601.png"), plot_ons_age_carers_lagp, width = 9.0, height=9.0, units = "in")

## ---- mean_age ----

## Cohort

# get mean ages for la/gp combined carers
df_dedup <- df_npt_dedup %>%
  merge(df_denbighshire_dedup, all = TRUE) %>%
  merge(df_swansea_dedup, all = TRUE)

df_dedup_age_means <- df_dedup %>%
  group_by(la_name) %>%
  summarise(mean_age = mean(age)) %>%
  mutate(identifiedby = "full_cohort")

df_dedup_age_means_lagp <- df_dedup %>%
  group_by(la_name, identifiedby) %>%
  summarise(mean_age = mean(age))

df_dedup_age_means_all_lagp <- df_dedup_age_means %>%
  merge(df_dedup_age_means_lagp, all = TRUE)

save_csv_rds(df_dedup_age_means_all_lagp, lb_demog_path)

## ONS estimates

df_ons_age_exact <- ons_age_raw %>%
  filter(Name %in% la_names) %>% # filter by LAs of interest
  clean_names() %>% # clean the column names so they aren't numbers
  select(name, "x18":"x90") %>% # remove unnecessary columns
  pivot_longer(!name, # pivot so age is a variable
               names_to = "age",
               values_to = "count_ons") %>%
  mutate(age = gsub('x', '', age), # remove the "x"s added by the clean_names function
         age = as.numeric(age)) %>%
  group_by(name, age) %>% # group by la and age group
  mutate(count_ons = sum(count_ons)) %>% # count number of people by age group per LA
  distinct() %>%
  ungroup() %>%
  group_by(name) %>% # group by la
  arrange(name)

ons_means <- df_ons_age_exact %>%
  group_by(name) %>%
  summarise(mean_Age = weighted.mean(age, count_ons))
