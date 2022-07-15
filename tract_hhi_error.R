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

hhi_df <- tract_data %>%
  select(gisjoin, state, H72001_dp, H72001_sf, H72003_sf, H72004_sf, H72005_sf,
         H72006_sf, H72007_sf, H72008_sf, H72009_sf) %>%
  rename(countycode = gisjoin, dp_total = H72001_dp, sf_total = H72001_sf, sf_white_pop = H72003_sf,
         sf_BAA_pop = H72004_sf, sf_AIAN_pop = H72005_sf , sf_asian_pop = H72006_sf,
         sf_NHOPI_pop = H72007_sf, sf_other_pop = H72008_sf, sf_two_races = H72009_sf) %>%
  mutate(s_white_sf = sf_white_pop/sf_total,
         s_BAA_sf = sf_BAA_pop/sf_total,
         s_AIAN_sf = sf_AIAN_pop/sf_total,
         s_Asian_sf = sf_asian_pop/sf_total,
         s_NHOPI_sf = sf_NHOPI_pop/sf_total,
         s_other_sf = (sf_other_pop + sf_two_races)/sf_total)%>%
  mutate (hhi = s_white_sf^2+s_BAA_sf^2+s_AIAN_sf^2+s_Asian_sf^2+s_NHOPI_sf^2+s_other_sf^2,.after=countycode,
          error = dp_total - sf_total)



plot_statePop_vs_hhi = function(stateNum, stateName) {
  
  statedf <- subset(hhi_df, state == stateNum)
  title <- paste(stateName)
  
  if (stateName == "Alabama" | stateName == "Pennsylvania") {
    x_label = "HHI"
    y_label = "Error (people)"
  } else {
    x_label = "HHI"
    y_label = ""
  }
  
  
  ggplot(data = statedf, aes(x = hhi, y = error, color = sf_total)) +
    geom_point(alpha = .4, size = .5) + labs(x = x_label, y = y_label, color="Population") +
    theme_bw() +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_color_viridis_c(option="A", begin = .3) +
    scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
    scale_y_continuous(limits=c(-120, 120), expand=expansion(mult=0)) + 
    geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222", 
              size = .65, se = FALSE, alpha = 0.5) 
  
}


pa_hhi_graph <- plot_statePop_vs_hhi("42", "Pennsylvania")
pa_hhi_graph

nc_hhi_graph <- plot_statePop_vs_hhi("37", "North Carolina")
nc_hhi_graph

sc_hhi_graph <- plot_statePop_vs_hhi("45", "South Carolina")
sc_hhi_graph

la_hhi_graph <- plot_statePop_vs_hhi("22", "Louisiana")
la_hhi_graph

al_hhi_graph <- plot_statePop_vs_hhi("01", "Alabama")
al_hhi_graph

de_hhi_graph <- plot_statePop_vs_hhi("10", "Delaware")
de_hhi_graph

ut_hhi_graph <- plot_statePop_vs_hhi("49", "Utah")
ut_hhi_graph

wa_hhi_graph <- plot_statePop_vs_hhi("53", "Washington")
wa_hhi_graph


hhi_figure <- ggarrange(pa_hhi_graph, nc_hhi_graph, sc_hhi_graph, la_hhi_graph, al_hhi_graph,
                         de_hhi_graph, ut_hhi_graph, wa_hhi_graph,
                         ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
hhi_figure
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/hhi_figure.pdf", width = 9.54, height = 5.81)


