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

#--Prepare Data Frame-------------------------------------------------
prepare_df <- alltractdata_122 %>%
  select(gisjoin, state, H72001_sf,H72003_sf,H72004_sf,H72005_sf,H72006_sf,H72007_sf) %>%
  rename(tract_code = gisjoin,tract_size = H72001_sf,popWhite_sf = H72003_sf, 
         popBAA_sf = H72004_sf, popAIAN_sf = H72005_sf,popAsian_sf = H72006_sf, 
         popNHOPI_sf = H72007_sf ) %>%
  mutate (s_whitesf = popWhite_sf/tract_size,
          s_BAAsf = popBAA_sf/tract_size,
          s_AIANsf = popAIAN_sf/tract_size,
          s_Asiansf = popAsian_sf/tract_size,
          s_NHOPIsf = popNHOPI_sf/tract_size,
          s_othersf = (tract_size-popWhite_sf-popBAA_sf-popAIAN_sf-popAsian_sf-popNHOPI_sf)/tract_size)

prepare_df <- prepare_df %>% mutate (max_race=colnames(prepare_df[9:14])[max.col(prepare_df[9:14])]) %>%
  mutate (largest_race = recode_factor(max_race, "s_whitesf"= "white","s_BAAsf"="black",
                                       "s_AIANsf"="aian","s_Asiansf"="asian",
                                       "s_NHOPIsf"="nativehawaiian","s_othersf"="other"))

nm_tractsize_plotdf <- prepare_df %>%
  select(tract_code,state,tract_size,largest_race)%>%
  filter(!is.na(largest_race))

#--Simulating Gaussian Noise and bind to Date Frame-------------------------
gaussianError <- rnorm(nrow(nm_tractsize_plotdf), mean = 0, sd = 139.93)
# (-576.4363  646.6136)
prodGaussianError <- rnorm(nrow(nm_tractsize_plotdf), mean = 0, sd = 2.47)
# (-9.894679  9.857540)

apr_nm_tractsize_plotdf <- nm_tractsize_plotdf %>%
  mutate(simulatedError = gaussianError)

pro_nm_tractsize_plotdf <- nm_tractsize_plotdf %>%
  mutate(simulatedError = prodGaussianError)

#--Graphing 2021 April Noisy Measurement vs. Tract Size-----------------------------
plot_simError_vs_hhi = function(stateNum, stateName) {
  
  statedf <- subset(apr_nm_tractsize_plotdf, state == stateNum)
  title <- stateName
  
  ggplot(data = statedf, aes(x=tract_size, y=simulatedError,color=largest_race)) +
    geom_point(alpha = .5) + 
    theme(axis.title = element_text(size = 8))+
    geom_smooth(method = "loess",size=1.5, se=F) +
    labs(x = "Tract Total Population", y = "Simulated Noisy Meansurement 2021 April Setting",
         color="Largest Racial Group") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman")) +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_y_continuous(limits=c(-700, 700), expand=expansion(mult=0),breaks = seq(-700, 700, 200))
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

apr_nm_ts_figure <- ggarrange(pa_graph, nc_graph, sc_graph, la_graph, al_graph, de_graph, ut_graph, wa_graph, ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
apr_nm_ts_figure

#--Graphing Production Setting Noisy Measurement  vs. Tract Size --------
plot_prod_simError_vs_hhi = function(stateNum, stateName) {
  
  statedf <- subset(pro_nm_tractsize_plotdf, state == stateNum)
  title <- stateName
  
  ggplot(data = statedf, aes(x=tract_size, y=simulatedError,color=largest_race)) +
    geom_point(alpha = .5) + 
    theme(axis.title = element_text(size = 8))+              
    geom_smooth(method = "loess",size=1.5, se=F) +
    labs(x = "Tract Total Population", y = "Production Setting Noisy Meansurement ",
         color="Largest Racial Group") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman")) +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_y_continuous(limits=c(-15, 15), expand=expansion(mult=0)) 
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

pro_nm_ts_figure <- ggarrange(pa_prod_graph, nc_prod_graph, sc_prod_graph, la_prod_graph, al_prod_graph, de_prod_graph, ut_prod_graph, wa_prod_graph, ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
pro_nm_ts_figure
