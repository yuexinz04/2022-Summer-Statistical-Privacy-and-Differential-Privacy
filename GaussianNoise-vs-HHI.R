#--Set Up-----------------------------------------------------------------------------------
setwd("~/R Projects/ArestyDP/nhgis_ppdd_20210428_12-2_tract")
library(tidyverse)
library(gam)
library(tibble)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(scales)
library(redist)
library(patchwork)
library(sf)
library(mgcv)
        
alltractdata_122 <- read.csv("nhgis_ppdd_20210428_12-2_tract.csv")

#--Calculating Herfindahl-Hirschman Index-----------------------------------------------------
tract_hhisf <- alltractdata_122 %>%
  select(gisjoin, state, H72001_sf, H72002_sf, H72003_sf, H72004_sf, H72005_sf, H72006_sf, H72007_sf, H72008_sf, H72009_sf) %>%
  rename(tractcode = gisjoin, state = state, popTotal_sf = H72001_sf, popONErace_sf = H72002_sf, popWhite_sf = H72003_sf, popBAA_sf = H72004_sf, popAIAN_sf = H72005_sf , popAsian_sf = H72006_sf, popNHOPI_sf = H72007_sf, popOther_sf = H72008_sf, popTwoOrMore_sf = H72009_sf) %>%
  mutate(s_whitesf = popWhite_sf/popTotal_sf,
         s_BAAsf = popBAA_sf/popTotal_sf,
         s_AIANsf = popAIAN_sf/popTotal_sf,
         s_Asiansf = popAsian_sf/popTotal_sf,
         s_NHOPIsf = popNHOPI_sf/popTotal_sf,
         s_othersf = popOther_sf/popTotal_sf,
         s_twoOrMoresf = popTwoOrMore_sf/popTotal_sf) %>%
  mutate(hhi_sf=s_whitesf^2 + s_BAAsf^2 + s_AIANsf^2 + s_Asiansf^2 + s_NHOPIsf^2 + s_othersf^2 + s_twoOrMoresf^2, after = tractcode)

nrow(alltractdata_122)

#--Simulating Gaussian Noise-----------------------------------------------------------------

gaussianError <- rnorm(74002, mean = 0, sd = 139.93)

#--Creating Dataframe-------------------------------------------------------------------------

df <- data.frame(HHI = tract_hhisf$hhi_sf, simulatedError = gaussianError, state = alltractdata_122$state)

#--Graphing Gaussian Noise vs. HHI------------------------------------------------------------

plot_simError_vs_hhi = function(stateNum, stateName) {
  
  statedf <- subset(df, state == stateNum)
  title <- stateName
  
  
  ggplot(data = statedf, aes(x=HHI, y=simulatedError)) +
    geom_point(alpha = .6, color = "blue") + labs(x = "HHI", y = "Error (people)") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman")) +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_color_viridis_c(option="A", begin = .3) + 
    scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
    scale_y_continuous(limits=c(-500, 500), expand=expansion(mult=0)) +
    geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222", size = 1.5, se = FALSE, alpha = 0.5)
  
}


#--Producing 8 State Plots from Kenny et al. Paper-------------------------------------------
pa_graph <- plot_simError_vs_hhi("42", "Pennsylvania")
nc_graph <- plot_simError_vs_hhi("37", "North Carolina")
sc_graph <- plot_simError_vs_hhi("45", "South Carolina")
la_graph <- plot_simError_vs_hhi("22", "Louisiana")
al_graph <- plot_simError_vs_hhi("1", "Alabama")
de_graph <- plot_simError_vs_hhi("10", "Delaware")
ut_graph <- plot_simError_vs_hhi("49", "Utah")
wa_graph <- plot_simError_vs_hhi("53", "Washington")

race_figure <- ggarrange(pa_graph, nc_graph, sc_graph, la_graph, al_graph, de_graph, ut_graph, wa_graph, ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
race_figure
