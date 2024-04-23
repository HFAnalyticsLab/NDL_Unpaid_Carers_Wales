library(lubridate)
library(ggplot2)
library(tidyr)
library(plyr)
library(dplyr)
library(ggsignif)
library(readxl)

source('../../filepaths.R') # read in filepaths
source(paste0(main_project_path, '1429_general_functions.R'))
source(paste0(main_project_path, '1429_color_palette.R'))
source(paste0(hsu_path, 'functions/1429_health_services_analysis_function.R'))


## ED, PEDW and Outpatient
# read in sheet and filter crude rates forlagp
ed_pedw_op_LAs_hsu <- read_excel(path = paste0(hsu_path,"data/processed/1429_hsu_rate ratios_1dp_JP_20230817.xlsx"), sheet = "ED_PEDW_OP")
ed_pedw_op_LAs_hsu <- ed_pedw_op_LAs_hsu[ed_pedw_op_LAs_hsu$group_breakdown == "lagp_crude",]

# significance annotation for ed department, inpatient (planned/emergency) and outpatient significance
ed_pedw_op_anno <- data.frame(x1 = c(1.75, 3.75), 
                           x2 = c(2.25, 4.25),
                           y1 = c(160, 530),
                           y2 = c(165, 535),
                           xstar = c(2, 4),
                           ystar = c(166, 536),
                           lab = c("*", "*"),
                           LA = c("Neath Port Talbot", "Neath Port Talbot"))





ed_pedw_op_signif_plot <- plot_hsu_signif(ed_pedw_op_LAs_hsu, "ed_pedw_op", ed_pedw_op_anno)
ggsave("1429_ed_pedw_op_20230817.png", width = 15.0, height = 12.0, units = "in")


## GP interactions
# read in sheet and filter crude rates forlagp
gp_LAs_hsu <- read_excel(path = paste0(hsu_path,"data/processed/1429_hsu_rate ratios_1dp_JP_20230817.xlsx"), sheet = "GP")
gp_LAs_hsu <- gp_LAs_hsu[gp_LAs_hsu$group_breakdown == "lagp_crude",]

# significance annotation for GP visits
gp_anno <- data.frame(x1 = c(0.75, 2.75), 
                         x2 = c(1.25, 3.25),
                         y1 = c(28, 33),
                         y2 = c(29, 34),
                         xstar = c(1, 3),
                         ystar = c(30, 35),
                         lab = c("*", "*"),
                         LA = c("Neath Port Talbot", "Denbighshire"))

gp_signif_plot <- plot_gp_signif(gp_LAs_hsu, "gp", gp_anno)
ggsave("1429_gp_20230817.png", width = 15.0, height = 12.0, units = "in")


