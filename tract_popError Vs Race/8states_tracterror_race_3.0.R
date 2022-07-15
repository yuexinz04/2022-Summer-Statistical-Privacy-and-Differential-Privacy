#--Set up--------------------------------------------------------------------
setwd("/Users/AnnaYuexinZhang/Documents/Rutgers/Work/2022 Summer Project SUPER")
alltractdata_122<-read.csv('./Vintage210428_122_Tract/nhgis_ppdd_20210428_12-2_tract.csv')
library(dplyr)
library(gam)
library(tibble)
#install.packages("ggplot2")
library(ggplot2)
library(ggpubr)
library(scales)
library(RColorBrewer)
library(mgcv)

#--Calculating Percent Non-White -----------------------------------
prepare_df <- alltractdata_122 %>%
  select(gisjoin,state, H72003_sf, H72001_sf,H72001_dp) %>%
  rename(countycode = gisjoin,  sf_white = H72003_sf, 
         sf_total = H72001_sf, dp_total = H72001_dp) %>%
  mutate(percent_nonwhite = (sf_total - sf_white) / sf_total,
         popError = dp_total-sf_total)

dpsf_race_plotdf <- prepare_df %>%
  select(state,percent_nonwhite,sf_total,popError)%>%
  filter(!is.na(percent_nonwhite))

# > range(dpsf_race_plotdf$popError)
# [1] -217  143

#--Graphing 2021 April Noisy Measurement vs. Non White Percentage-----------------------------
plot_tractpopdpsf_vs_race = function(stateNum, stateName) {
  
  statedf <- subset(dpsf_race_plotdf, state == stateNum)
  title <- stateName
  
  ggplot(data = statedf, aes(x = percent_nonwhite, y = popError, color = sf_total)) +
    geom_point(alpha = .6) + 
    labs(x = "Percent Non-White", y = "Tract level Population Error DP vs SF", color="Tract Size") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman")) +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_color_viridis_c(option="A", begin = .3) +
    scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
    scale_y_continuous(limits=c(-120, 120), expand=expansion(mult=0)) + 
    geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222", size = 1.5, se = FALSE, alpha = 0.5) 
}

#--Producing 8 State Plots from Kenny et al. Paper (2021 April NM)-------------------------------------------
pa_graph <- plot_tractpopdpsf_vs_race("42", "Pennsylvania")
nc_graph <- plot_tractpopdpsf_vs_race("37", "North Carolina")
sc_graph <- plot_tractpopdpsf_vs_race("45", "South Carolina")
la_graph <- plot_tractpopdpsf_vs_race("22", "Louisiana")
al_graph <- plot_tractpopdpsf_vs_race("1", "Alabama")
de_graph <- plot_tractpopdpsf_vs_race("10", "Delaware")
ut_graph <- plot_tractpopdpsf_vs_race("49", "Utah")
wa_graph <- plot_tractpopdpsf_vs_race("53", "Washington")

tractpop_dpsf_race_figure <- ggarrange(pa_graph, nc_graph, sc_graph, la_graph, al_graph, de_graph, ut_graph, wa_graph, ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
tractpop_dpsf_race_figure
