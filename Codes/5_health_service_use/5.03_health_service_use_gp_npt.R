library(dplyr)
library(ggplot2)
library(lubridate)
library(xlsx)
library(PHEindicatormethods)

source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_RODBC_login_20221031.R'))
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(gp_path, "functions/1429_matchedcohort_gp_functions_gbh_20221206.R"))


# read in data from DB2
npt_matched_gp_raw <- sqlQuery(sql, "SELECT * FROM SAILW1429V.GBH_MATCHEDCOHORT_GP_NPT")

# STEP 1: Running data prep function
npt_matched_gp_raw2 <- gp_dataprep(npt_matched_gp_raw)


# STEP 2: Add counts by individual
# by Unpaid carer vs Non-carer
npt_gp_indv_counts <- gp_indv_count_table(npt_matched_gp_raw2)

# by La-identified vs gp-identified 
npt_gp_indv_counts_lagp <- gp_indv_count_table_lagp(npt_matched_gp_raw2)



# STEP 3: Adding Counts by individual (binned)

# JP 2022-02-14: by carer flag
npt_gp_counts_bins <- gp_indv_bins_table(npt_gp_indv_counts)

# by  LA-identified vs gp_identified 
npt_gp_counts_bins_lagp <- gp_indv_bins_table_lagp(npt_gp_indv_counts_lagp)




# Step 4: prevalence rate ratio calculations using total visits / total population size
# lagp standardised
npt_gp_overall_agesex_std <- gp_agesex_standardisation(npt_matched_gp_raw2, "Neath Port Talbot")


# crude rates - carer type
npt_gp_overall_rate <- gp_cruderate_alf(npt_matched_gp_raw2, 1)
# crude rates -  lagp
npt_gp_overall_rate_lagp <- gp_cruderate_alf(npt_matched_gp_raw2, 0) 



# Step 5: Timeline
npt_gp_timegrp_table <- gp_add_timeline_cols(npt_matched_gp_raw2)

# - total interacters per person in the year prior to index date
npt_gp_interaction_rate_tl <- gp_timeline_eventrate_tables(npt_gp_timegrp_table, 0)
npt_gp_interaction_rate_tl_lagp <- gp_timeline_eventrate_tables(npt_gp_timegrp_table, 1)

# plots timeline
# timeline for gp interactions
npt_gp_visits_tl_plot <- gp_timeline_rate_plots(npt_gp_interaction_rate_tl, "Neath Port Talbot", 0)
npt_gp_visits_tl_plot
# la-carer vs gp-carer
npt_gp_visits_tl_plot_lagp <- gp_timeline_rate_plots(npt_gp_interaction_rate_tl_lagp, "Neath Port Talbot", 1)
npt_gp_visits_tl_plot_lagp


## plot binned proportions 
# - by carer status
npt_gp_perc_plot <- npt_gp_counts_bins %>%
  ggplot() +
  geom_col(aes(x = events_grp,
               y = events_percentage_grp,
               fill = carer_desc), 
           position = 'dodge') +
  labs(title = "Percentage of GP Interactions (Bins) for NPT unpaid carers vs non-carers",
       x = "Total primary care interactions per individual",
       y = "Percentage (%)") +
  geom_text(aes(x = events_grp,  
                y = events_percentage_grp, 
                label = sprintf("%.1f", events_percentage_grp), 
                group = carer_desc),  
            position= position_dodge(width = 1), 
            vjust = -.25,
            size = 3.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white", color = "grey50"),
        strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black")) +
  # remove margin between bar plots and x-axis
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  scale_fill_manual(values = npt_carertype_palette, name = 'Cohort')

# - by lagp identified unpaid carers
npt_gp_perc_lagp_plot <- npt_gp_counts_bins_lagp %>%
  filter(carer_desc == "Unpaid carers") %>% 
  mutate(identifier = paste0(first_identified_by, "-identified")) %>% 
  ggplot() +
  geom_col(aes(x = events_grp,
               y = events_percentage_grp_lagp,
               fill = identifier), 
           position = 'dodge') +
 
  labs(title = "Percentage of GP Interactions (Bins) for NPT unpaid carers identified by GP vs LA",
       x = "Total primary care interactions per individual",
       y = "Percentage (%)") +
  geom_text(aes(x = events_grp,  
                y = events_percentage_grp_lagp, 
                label = sprintf("%.1f", events_percentage_grp_lagp), 
                group = identifier), 
            position= position_dodge(width = 1), 
            vjust = -.25,
            size = 3.5) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(fill = "white", color = "grey50"),
        strip.background = element_rect(fill = "grey93", linetype = "solid", color = "black")) +
  # remove margin between bar plots and x-axis
  scale_y_continuous(expand = expansion(mult = c(0,0.05))) +
  scale_fill_manual(values = npt_lagpcarers_palette, name = 'Cohort')



### Exporting Tables and Plots ###


# table
outfn <- paste0(gp_path,'data/processed/1429_npt_gp_counts.xlsx')
xlsx::write.xlsx(data.frame(npt_gp_counts_bins), file=outfn, sheetName = "gp_carertype", row.names = FALSE) 
xlsx::write.xlsx(data.frame(npt_gp_counts_bins_lagp), file=outfn, sheetName = "gp_lagp", row.names = FALSE, append = TRUE) 

# rates + stats
outfn <- paste0(gp_path,'data/processed/1429_npt_gp_rates_stats.xlsx')
# prevalance rates
xlsx::write.xlsx(npt_gp_overall_rate, file=outfn, sheetName = "gp_crude_carertype", row.names = FALSE) 
xlsx::write.xlsx(npt_gp_overall_rate_lagp, file=outfn, sheetName = "gp_crude_lagp", row.names = FALSE, append = TRUE) 
xlsx::write.xlsx(npt_gp_overall_agesex_std, file=outfn, sheetName = "gp_agesex_lagp", row.names = FALSE, append = TRUE) 
# stats
xlsx::write.xlsx(npt_gp_stats, file=outfn, sheetName = "gp_mw_carertype", row.names = FALSE, append = TRUE) 
xlsx::write.xlsx(npt_gp_stats_lagp, file=outfn, sheetName = "gp_mw_lagp", row.names = FALSE, append = TRUE) 



# timeline
outfn <- paste0(gp_path,'data/processed/1429_npt_gprate_timeline.xlsx')
xlsx::write.xlsx(npt_gp_interaction_rate_tl, file=outfn, sheetName = "gp_carertype", row.names = FALSE) 
xlsx::write.xlsx(npt_gp_interaction_rate_tl_lagp, file=outfn, sheetName = "gp_lagp", row.names = FALSE, append = TRUE) #



# percentage plots
save_ggplot(npt_gp_perc_plot, gp_path)
save_ggplot(npt_gp_perc_lagp_plot, gp_path)

# timeline plots
save_ggplot(npt_gp_tl_plot, gp_path)
save_ggplot(npt_gp_tl_plot_lagp, gp_path)
save_ggplot(npt_gp_visits_tl_plot, gp_path)
save_ggplot(npt_gp_visits_tl_plot_lagp, gp_path)
save_ggplot(npt_gp_visits_tl_plot, gp_path)
save_ggplot(npt_gp_visits_tl_plot_lagp, gp_path)


