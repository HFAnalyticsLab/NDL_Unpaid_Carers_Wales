## ---- Load libraries ----

pkgs <- c('dplyr', 'eulerr', 'png', 'grid', 'gridExtra', 'ggplot2', 'ggh4x', 'magrittr', 'tidyr')
lapply(pkgs, library, character.only = T)

## ---- Load data ----

# Run file_paths_lb.R

source(paste0(main_path, "1429_RODBC_login_20221031.R")) # login
source(paste0(lb_demog_path, "scripts/1429_timeline_script_peje_20221111.R")) # timeline tables
source(paste0(main_path, "1429_color_palette.R")) # source colour palette for timeline plot
source(paste0(main_path, "1429_general_functions.R")) # source general functions


## ---- functions ----

## calculate overlap
overlap_count <- function(df){
  
  out_df <- df %>%
    group_by(IDENTIFIEDBY) %>%
    count() %>%
    as.data.frame()
  
  return(out_df)
}


## Create table with GP-identified, LA-identified and overlaps
la_counts <- function(df_dedup, df_overlap, la_name){
  
  df_overlap$IDENTIFIEDBY <- gsub("GP", "gp_overlap",
                                   gsub("LA", "la_overlap", df_overlap$IDENTIFIEDBY))
  
  df_out <- df_dedup %>% merge(df_overlap, all = TRUE)
  
  df_out <- df_out %>% pivot_wider(names_from = IDENTIFIEDBY, values_from = n) %>%
    mutate(name = la_name)
}

## Create Euler plot
fun_eulerr_fig <- function(dat, study_period){
  venn <- plot(euler(c(LA = dat[[3]], GP = (dat[[1]]), "GP&LA" = dat[[2]] + dat[[4]])), 
       quantities = list(TRUE, fontsize = 10),
       labels = list(fontsize = 12),
       main = list(label = paste("Study period:", study_period, "\nN =", dat[[1]]+dat[[3]]), fontsize = 8, vjust = 1))
  }

## Create text grobs for euler plot
euler_text <- function(la_name){
  textGrob(la_name, gp=gpar(fontsize=18, fontface = "bold")) 
}

## Create timeline plot
timeline_plots_all_qtrly <- function(df, xmin, xmax, study_period){
  
  out_df <-  df %>%
    ggplot(aes(x = interaction(index_quarter, index_yr),
               y = count,
               group = factor(identifiedby, levels = c("LA-identified", "GP-identified")))) +
    geom_line(colour="grey") +
    geom_line(data=df[df$study_per==1,], aes(colour = factor(identifiedby, levels = c("LA-identified", "GP-identified")))) +
    geom_point(colour="grey", aes(shape = factor(identifiedby, levels = c("LA-identified", "GP-identified")))) +
    geom_point(data=df[df$study_per==1,], aes(colour = factor(identifiedby, levels = c("LA-identified", "GP-identified")),shape = factor(identifiedby, levels = c("LA-identified", "GP-identified")))) +
    scale_colour_manual(values = lagpcarers_palette) +
    labs(x = "Quarter",
         y = "N Unpaid Carers first identified",
         shape = "Service",
         linetype = "Service", 
         colour = "Service") +
    annotate("rect",xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, alpha = 0.1) +
    scale_x_discrete(NULL, guide = "axis_nested") +
    scale_y_continuous(expand = c(0,0))+
    theme( axis.text.x = element_text(angle = 45, hjust = 1), 
           panel.background = element_blank(), 
           axis.line = element_line(colour = "black"), 
           legend.position = "none") 
  
  return(out_df)
}

## Create text grobs for euler plot
timeline_text <- function(la_name){
  textGrob(la_name, gp=gpar(fontsize=18, fontface = "bold"), rot=90) 
}

## ---- Euler ----

## Read in dedup data
df_npt_dedup <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_DEDUP_GP_LA_NPT")
df_swan_dedup <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_DEDUP_GP_LA_SWANSEA")
df_denb_dedup <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_DEDUP_GP_LA_DENBIGHSHIRE")

## Get count for first identified by
npt_dedup <- overlap_count(df_npt_dedup)
swan_dedup <- overlap_count(df_swan_dedup)
denb_dedup <- overlap_count(df_denb_dedup)

## Read in overlap data
df_npt_overlap <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_OVERLAP_GP_LA_NPT")
df_swan_overlap <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_OVERLAP_GP_LA_SWA")
df_denb_overlap <- sqlQuery(sql, "SELECT * FROM SAILW1429V.PEJE_OVERLAP_GP_LA_DEN")

## Get count for LA overlap and GP overlap
npt_overlap <- overlap_count(df_npt_overlap)
swan_overlap <- overlap_count(df_swan_overlap)
denb_overlap <- overlap_count(df_denb_overlap)

## Merge
npt <- la_counts(npt_dedup, npt_overlap, "Neath Port Talbot")
swan <- la_counts(swan_dedup, swan_overlap, "Swansea")
denb <- la_counts(denb_dedup, denb_overlap, "Denbighshire")

# venn_data <- npt %>%
#   merge(swan, all = TRUE) %>%
#   merge(denb, all = TRUE)

## Euler diagrams
venn_npt <- fun_eulerr_fig(npt, "July 2017 - May 2022")
venn_swan <- fun_eulerr_fig(swan, "April 2021 - June 2022")
venn_denb <- fun_eulerr_fig(denb, "April 2020 - March 2022")

## Create counts table for export (combines LA overlap and GP overlap into one overlap column for SDC)
df_cohort_counts <- npt %>%
  merge(swan, all = TRUE) %>%
  merge(denb, all = TRUE) %>%
  mutate(overlap = gp_overlap + la_overlap,
         total = GP + LA,
         GP_perc = GP/total * 100,
         overlap_perc = overlap/total * 100) %>%
  select(-gp_overlap, -la_overlap) %>%
  select(name, everything())

initials <- "lb"
save_csv_rds(df_cohort_counts, lb_demog_path) # save table


## ---- Timeline ----

## NPT

df_npt_la_quarter <- counts_quarterly(df_npt_la_carers, "npt_la") # quarterly count of LA-identified

# Combine GP-identified quarterly counts from extended dates with LA-identified quarterly counts
df_npt_all_quarter <- rbind(
  df_npt_gp_extended_quarter %>%
  filter(variable == "year") %>% 
  select(c("index_yr","index_quarter","count", "cohort")),
  df_npt_la_quarter %>% 
    select(c("index_yr","index_quarter","count", "cohort")) 
  ) 

# Add flag for counts within study period
df_npt_all_quarter <- df_npt_all_quarter %>% 
  mutate(study_per = case_when((index_yr == "2017" & index_quarter == "Q2")  ~ 0,
                               TRUE ~ 1),
         unmasked_count = count,
         count = ifelse(count <10, 10, count),
         masked = ifelse(count != unmasked_count, "*", ""), # mask counts <10
         identifiedby = paste(toupper(str_sub(cohort, -2, -1)), "-identified", sep = ""))

# Plot timeline, add "*" labels for masked counts
quart_timeline_npt <- df_npt_all_quarter %>%
  timeline_plots_all_qtrly(2, 20.8, "July 2017 - May 2022") +
  geom_text(data = df_npt_all_quarter, 
            aes(x = interaction(index_quarter, index_yr),
                y = count,
                group = cohort,
                label = masked),
            vjust = 0.5,
            colour = lagpcarers_palette[[1]],
            size = 8) +
  theme(legend.position = "top",
        legend.title = element_blank())


## Denbighshire

# Combine GP-identified quarterly counts from extended dates with LA-identified quarterly counts
df_denb_all_quarter<-rbind(
  df_denbighshire_gp_extended_quarter %>%
    filter(variable == "year") %>% 
    select(c("index_yr","index_quarter","count", "cohort")),
  df_denbighshire_la_index_quarter %>% 
    select(c("index_yr","index_quarter","count", "cohort")) 
)

# Add flag for counts within study period
df_denb_all_quarter <- df_denb_all_quarter %>% 
  mutate(study_per = case_when((index_yr == "2020" & index_quarter != "Q1") | 
                                 index_yr == "2021" | 
                                 index_yr == "2022"   ~ 1,
                               TRUE ~ 0),
         identifiedby = paste(toupper(str_sub(cohort, -2, -1)), "-identified", sep = ""))

# Plot timeline
quart_timeline_denb <- df_denb_all_quarter %>%
  timeline_plots_all_qtrly(13,20, "April 2020 - March 2022")


## Swansea

# Combine GP-identified quarterly counts from extended dates with LA-identified quarterly counts
df_swan_all_quarter<-rbind(
  df_swansea_gp_extended_quarter %>%
    filter(variable == "year") %>% 
    select(c("index_yr","index_quarter","count", "cohort")),
  df_swansea_la_index_quarter %>% 
    select(c("index_yr","index_quarter","count", "cohort")) 
)

# Add flag for counts within study period
df_swan_all_quarter <- df_swan_all_quarter %>% 
   mutate(study_per = case_when((index_yr == "2021" & index_quarter != "Q1") | index_yr ==  "2022" ~ 1,
                                TRUE ~ 0),
          identifiedby = paste(toupper(str_sub(cohort, -2, -1)), "-identified", sep = ""))
                               
# Plot timeline
quart_timeline_swan <- df_swan_all_quarter %>%
  timeline_plots_all_qtrly(17,20.8, "April 2021 - June 2022")

# combine all 3 LAs into one table for export
df_timeline_quarterly <- df_npt_all_quarter %>%
  merge(df_swan_all_quarter, all = TRUE) %>%
  merge(df_denb_all_quarter, all = TRUE) %>%
  select(-unmasked_count) %>%
  arrange(cohort, index_yr, index_quarter)

df_timeline_quarterly$masked[is.na(df_timeline_quarterly$masked)] <- ""

save_csv_rds(df_timeline_quarterly, lb_demog_path)


## ---- Arranging plots ----

# create labels for timeline
npt_text <- timeline_text("Neath Port Talbot")
swan_text <- timeline_text("Swansea")
denb_text <- timeline_text("Denbighshire")

# arrange timeline plots
timeline_plot <- grob_layout(
  grob_row(grob_col(p=1, npt_text), grob_col(p=8, quart_timeline_npt)),
  grob_row(grob_col(p = 1, swan_text), grob_col(p = 8, quart_timeline_swan)),
  grob_row(grob_col(p = 1, denb_text), grob_col(p = 8, quart_timeline_denb)),
  height = 250,
  width = 310,
  padding = 0
)

# view_grob(timeline_plot)

# save timeline layout
png(filename = paste0(lb_demog_path, "outputs/1429_timeline_lb.png"), width = 310, height = 250, units = "mm", res = 100)
view_grob(timeline_plot)
dev.off()

# create labels for euler plots
npt_text <- euler_text("Neath Port Talbot")
swan_text <- euler_text("Swansea")
denb_text <- euler_text("Denbighshire")

# arrange euler plots
euler_plot <- grob_layout(
  grob_row(p = 1, grob_col("Neath Port Talbot"), grob_col("Swansea"), grob_col("Denbighshire")),
  grob_row(p = 6, grob_col(venn_npt), grob_col(venn_swan), grob_col(venn_denb)),
  height = 80,
  width = 310,
  padding = 0
) 

# save euler layout
png(filename = paste0(lb_demog_path, "outputs/1429_euler_lb.png"), width = 310, height = 80, units = "mm", res = 100)
view_grob(euler_plot)
dev.off()