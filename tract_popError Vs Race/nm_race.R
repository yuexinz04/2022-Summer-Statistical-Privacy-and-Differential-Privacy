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
  select(gisjoin,state, H72003_sf, H72001_sf) %>%
  rename(countycode = gisjoin,  sf_white = H72003_sf, sf_total = H72001_sf) %>%
  mutate(percent_nonwhite = (sf_total - sf_white) / sf_total)

nm_race_plotdf <- prepare_df %>%
  select(state,percent_nonwhite,sf_total)%>%
  filter(!is.na(percent_nonwhite))

#--Simulating Gaussian Noise and bind to Date Frame-------------------------
gaussianError <- rnorm(nrow(nm_race_plotdf), mean = 0, sd = 139.93)
# (-553.3379  608.9988)
prodGaussianError <- rnorm(nrow(nm_race_plotdf), mean = 0, sd = 2.47)
# (-10.21418   9.84833)

apr_nm_race_plotdf <- nm_race_plotdf %>%
  mutate(simulatedError = gaussianError) %>%
  rename (tract_size=sf_total)

pro_nm_race_plotdf <- nm_race_plotdf %>%
  mutate(simulatedError = prodGaussianError) %>%
  rename (tract_size=sf_total)

#--Graphing 2021 April Noisy Measurement vs. Non White Percentage-----------------------------
plot_simError_vs_hhi = function(stateNum, stateName) {
  
  statedf <- subset(apr_nm_race_plotdf, state == stateNum)
  title <- stateName
  
  ggplot(data = statedf, aes(x = percent_nonwhite, y = simulatedError, color = tract_size)) +
    geom_point(alpha = .6) + 
    labs(x = "Percent Non-White", y = "Noisy Measurement 2021 April Setting", color="Tract Size") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman")) +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_color_viridis_c(option="A", begin = .3) +
    scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
    scale_y_continuous(limits=c(-600, 650), expand=expansion(mult=0)) + 
    geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222", size = 1.5, se = FALSE, alpha = 0.5) 
}

#--Producing 8 State Plots from Kenny et al. Paper (2021 April NM)-------------------------------------------
pa_graph <- plot_simError_vs_hhi("42", "Pennsylvania")
nc_graph <- plot_simError_vs_hhi("37", "North Carolina")
sc_graph <- plot_simError_vs_hhi("45", "South Carolina")
la_graph <- plot_simError_vs_hhi("22", "Louisiana")
al_graph <- plot_simError_vs_hhi("1", "Alabama")
de_graph <- plot_simError_vs_hhi("10", "Delaware")
ut_graph <- plot_simError_vs_hhi("49", "Utah")
wa_graph <- plot_simError_vs_hhi("53", "Washington")

apr_nm_nw_figure <- ggarrange(pa_graph, nc_graph, sc_graph, la_graph, al_graph, de_graph, ut_graph, wa_graph, ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
#apr_nm_ts_figure

#--Graphing Production Setting Noisy Measurement  vs. Non White Percentage --------
plot_prod_simError_vs_hhi = function(stateNum, stateName) {
  
  statedf <- subset(pro_nm_race_plotdf, state == stateNum)
  title <- stateName
  
  ggplot(data = statedf, aes(x = percent_nonwhite, y = simulatedError, color = tract_size)) +
    geom_point(alpha = .6) + 
    labs(x = "Percent Non-White", y = "Noisy Measurement Production Setting", color="Tract Size") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman")) +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_color_viridis_c(option="A", begin = .3) +
    scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
    scale_y_continuous(limits=c(-12, 12), expand=expansion(mult=0)) + 
    geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222", size = 1.5, se = FALSE, alpha = 0.5) 
}

#--Producing 8 State Plots from Kenny et al. Paper (Production Setting NM)-----
pa_prod_graph <- plot_prod_simError_vs_hhi("42", "Pennsylvania")
nc_prod_graph <- plot_prod_simError_vs_hhi("37", "North Carolina")
sc_prod_graph <- plot_prod_simError_vs_hhi("45", "South Carolina")
la_prod_graph <- plot_prod_simError_vs_hhi("22", "Louisiana")
al_prod_graph <- plot_prod_simError_vs_hhi("1", "Alabama")
de_prod_graph <- plot_prod_simError_vs_hhi("10", "Delaware")
ut_prod_graph <- plot_prod_simError_vs_hhi("49", "Utah")
wa_prod_graph <- plot_prod_simError_vs_hhi("53", "Washington")

pro_nm_nw_figure <- ggarrange(pa_prod_graph, nc_prod_graph, sc_prod_graph, la_prod_graph, al_prod_graph, de_prod_graph, ut_prod_graph, wa_prod_graph, ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
#pro_nm_ts_figure

