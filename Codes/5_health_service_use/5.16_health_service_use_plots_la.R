## ---- hsu-dataprep ----
#read in data from DB2
library(lubridate)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(readxl)
library(ggsignif)
library(stringr)


initials <- ""
source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(hsu_path, 'functions/1429_health_services_analysis_function.R'))


### health service use presented in 1 row, 3 column format. ###

# Emergency department (ED) attendances and planned/emergency hospital admissions
#- read file
#- filter to lagp crude rates only
ed_pedw_LAs_hsu <- read_excel(path = paste0(hsu_path,"data/raw/1429_health_services_ratios_1dp_jp.xlsx"), sheet = "ED_PEDW_LAs")
ed_pedw_LAs_hsu <- ed_pedw_LAs_hsu[ed_pedw_LAs_hsu$group_breakdown == "lagp_crude" ,]

# define annotation for significance
npt_ed_pedw_anno <- data.frame(x1 = c(1.75), x2 = c(2.25), 
                           y1 = c(148), y2 = c(149), 
                           xstar = c(2), ystar = c(150),
                           lab = c("*"),
                           LA = c("Neath Port Talbot"))

# call plot function for each LA
npt_ed_pedw_signif_plot <- plot_hsu_signif_la(ed_pedw_LAs_hsu, "ed_pedw", npt_ed_pedw_anno, "Neath Port Talbot")
npt_ed_pedw_signif_plot
ggsave(paste0(hsu_path,'outputs/1429_npt_edpedw_plot.png'),  npt_ed_pedw_signif_plot, width = 15.0, height=15.0, units = "in")

swansea_ed_pedw_signif_plot <- plot_hsu_signif_la(ratio_df = ed_pedw_LAs_hsu, analysis_group = "ed_pedw",  la_name = "Swansea")
swansea_ed_pedw_signif_plot
ggsave(paste0(hsu_path,'outputs/1429_swansea_edpedw_plot.png'),  swansea_ed_pedw_signif_plot, width = 15.0, height=15.0, units = "in")

denbighshire_ed_pedw_signif_plot <- plot_hsu_signif_la(ratio_df = ed_pedw_LAs_hsu, analysis_group = "ed_pedw",  la_name = "Denbighshire")
denbighshire_ed_pedw_signif_plot
ggsave(paste0(hsu_path,'outputs/1429_denbighshire_edpedw_plot.png'),  denbighshire_ed_pedw_signif_plot, width = 15.0, height=15.0, units = "in")





# Outpatient attendances
op_LAs_hsu <- read_excel(path = paste0(hsu_path,"data/raw/1429_op_hsu_ratios_1dp_jp_20230724.xlsx"), sheet = "OP_LAs")
op_LAs_hsu <- op_LAs_hsu[op_LAs_hsu$group_breakdown == "lagp_crude" ,]

op_anno <- data.frame(x1 = c(0.75, 0.75), x2 = c(1.25, 1.25), 
                      y1 = c(523, 443), y2 = c(524, 444), 
                      xstar = c(1, 1), ystar = c(525, 445),
                      lab = c("*", "*"),
                      LA = c("Neath Port Talbot","Denbighshire"))

npt_op_signif_plot <- plot_hsu_signif_la(op_LAs_hsu, "op", op_anno[1,], "Neath Port Talbot")
ggsave(paste0(hsu_path,'outputs/1429_npt_op_plot.png'),  npt_op_signif_plot,  height=15.0, width = 15.0, units = "in")


swansea_op_signif_plot <- plot_hsu_signif_la(ratio_df =op_LAs_hsu, analysis_group ="op",  la_name ="Swansea")
ggsave(paste0(hsu_path,'outputs/1429_swansea_op_plot.png'),  swansea_op_signif_plot,  height=15.0, width = 15.0, units = "in")


denbighshire_op_signif_plot <- plot_hsu_signif_la(op_LAs_hsu, "op",  op_anno[2,], "Denbighshire")
ggsave(paste0(hsu_path,'outputs/1429_denbighshire_op_plot.png'),  denbighshire_op_signif_plot,  height=15.0, width = 15.0, units = "in")





# GP interactions
gp_LAs_hsu <- read_excel(path = paste0(hsu_path,"data/raw/1429_gp_hsu_ratios_1dp_jp_20230721.xlsx"), sheet = "GP_LAs")
gp_LAs_hsu <- gp_LAs_hsu[gp_LAs_hsu$group_breakdown == "lagp_crude" ,]

gp_anno <- data.frame(x1 = c(0.75), x2 = c(1.25, 1.25, 1.25), 
                      y1 = c(28, 33, 32), y2 = c(29, 34, 33), 
                      xstar = c(1,  1,  1), ystar = c(30, 35, 34),
                      lab = c("*", "*", "*"),
                      LA = c("Neath Port Talbot", "Swansea", "Denbighshire"))

npt_gp_signif_plot <- plot_hsu_signif_la(gp_LAs_hsu, "gp", gp_anno[1,], "Neath Port Talbot")
ggsave(paste0(hsu_path,'outputs/1429_npt_gp_plot.png'),  npt_gp_signif_plot, width = 15.0, height=15.0, units = "in")


swansea_gp_signif_plot <- plot_hsu_signif_la(gp_LAs_hsu, "gp", gp_anno[2,], "swansea")
ggsave(paste0(hsu_path,'outputs/1429_swansea_gp_plot.png'),  swansea_gp_signif_plot, width = 15.0, height=15.0, units = "in")


denbighshire_gp_signif_plot <- plot_hsu_signif_la(gp_LAs_hsu, "gp", gp_anno[3,], "Denbighshire")
ggsave(paste0(hsu_path,'outputs/1429_denbighshire_gp_plot.png'),  denbighshire_gp_signif_plot, width = 15.0, height=15.0, units = "in")

