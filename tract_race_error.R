library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scales)
library(redist)
library(patchwork)
library(sf)
library(mgcv)
theme_set(theme_pubr())

source(".../dataverse_files/R/00_custom_functions.R")

tract_data = read_csv(".../nhgis_ppdd_20210428_12-2_tract/nhgis_ppdd_20210428_12-2_tract.csv")


race_df <- tract_data %>%
  select(gisjoin, state, H72001_dp, H72001_sf, H72003_sf) %>%
  rename(countycode = gisjoin,  dp_total = H72001_dp, sf_total = H72001_sf, 
         sf_white = H72003_sf) %>%
  mutate(percent_nonwhite = (sf_total - sf_white) / sf_total,
         error = dp_total - sf_total)


plot_statePop_vs_percentNonWhite = function(stateNum, stateName) {
  
  statedf <- subset(race_df, state == stateNum)
  title <- paste(stateName)
  
  if (stateName == "Alabama" | stateName == "Pennsylvania") {
    x_label = "Percent Non-White"
    y_label = "Error (people)"
  } else {
    x_label = "Percent Non-White"
    y_label = ""
  }
  
  
  ggplot(data = statedf, aes(x = percent_nonwhite, y = error, color = sf_total)) +
    geom_point(alpha = .6, size = .5) + labs(x = x_label, y = y_label, color="Population") +
    theme_bw() +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_color_viridis_c(option="A", begin = .3) +
    scale_x_continuous(labels=percent,expand=expansion(mult=0), breaks = seq(0.2, 0.8, 0.2)) +
    scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
    geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222", 
              size = .65, se = FALSE, alpha = 0.5)
  
}

#--Plotting 8 States from Kenny et al. Paper------------------------------------------------------------------- 

pa_race_graph <- plot_statePop_vs_percentNonWhite("42", "Pennsylvania")
pa_race_graph

nc_race_graph <- plot_statePop_vs_percentNonWhite("37", "North Carolina")
nc_race_graph

sc_race_graph <- plot_statePop_vs_percentNonWhite("45", "South Carolina")
sc_race_graph

la_race_graph <- plot_statePop_vs_percentNonWhite("22", "Louisiana")
la_race_graph

al_race_graph <- plot_statePop_vs_percentNonWhite("01", "Alabama")
al_race_graph

de_race_graph <- plot_statePop_vs_percentNonWhite("10", "Delaware")
de_race_graph

ut_race_graph <- plot_statePop_vs_percentNonWhite("49", "Utah")
ut_race_graph

wa_race_graph <- plot_statePop_vs_percentNonWhite("53", "Washington")
wa_race_graph

race_figure <- ggarrange(pa_race_graph, nc_race_graph, sc_race_graph, la_race_graph, al_race_graph,
                         de_race_graph, ut_race_graph, wa_race_graph,
                         ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
race_figure
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/race_figure.pdf", width = 9.54, height = 5.81)
