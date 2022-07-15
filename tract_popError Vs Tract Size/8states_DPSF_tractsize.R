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

#--Prepare Data Frame-------------------------------------------------
prepare_df <- alltractdata_122 %>%
  select(gisjoin,state,H72001_dp,H72001_sf,H72003_sf,H72004_sf,H72005_sf,
         H72006_sf,H72007_sf) %>%
  rename(tract_code = gisjoin,total_sf = H72001_sf, total_dp=H72001_dp,
         popWhite_sf = H72003_sf,popBAA_sf = H72004_sf,popAIAN_sf =H72005_sf,
         popAsian_sf = H72006_sf, popNHOPI_sf = H72007_sf ) %>%
  mutate (tractpop_error = total_dp-total_sf,
          s_whitesf = popWhite_sf/total_sf,
          s_BAAsf = popBAA_sf/total_sf,
          s_AIANsf = popAIAN_sf/total_sf,
          s_Asiansf = popAsian_sf/total_sf,
          s_NHOPIsf = popNHOPI_sf/total_sf,
          s_othersf = (total_sf-popWhite_sf-popBAA_sf-popAIAN_sf-popAsian_sf-popNHOPI_sf)/total_sf)

prepare_df <- prepare_df %>% mutate (max_race=colnames(prepare_df[11:16])[max.col(prepare_df[11:16])]) %>%
  mutate (largest_race=recode_factor(max_race, "s_whitesf"= "white","s_BAAsf"="black",
                                       "s_AIANsf"="aian","s_Asiansf"="asian",
                                       "s_NHOPIsf"="nativehawaiian","s_othersf"="other"))

tractpoperror_tractsize_plotdf <- prepare_df %>%
  select(tract_code,state,tractpop_error,total_sf,largest_race)%>%
  rename(tract_size = total_sf) %>%
  filter(!is.na(largest_race))

#--Graphing Tract level Population Error vs. Tract Size (for 8 states)-------
plot_tractpoperror_vs_tractsize = function(stateNum, stateName) {
  
  statedf <- subset(tractpoperror_tractsize_plotdf, state == stateNum)
  title <- stateName
  
  ggplot(data = statedf, aes(x=tract_size, y=tractpop_error,color=largest_race)) +
    geom_point(alpha = .5) + 
    theme(axis.title = element_text(size = 8))+
    geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222", 
              size = .65, se = FALSE, alpha = 0.5)+
    labs(x = "Tract Total Population", y = "Tract level Population Error DP vs SF",
         color="Largest Racial Group") +
    theme_bw() +
    theme(text = element_text(family = "Times New Roman")) +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish)+ 
    scale_x_continuous(limits=c(0, 18000), labels=comma,expand=expansion(mult=0)) +
    scale_y_continuous(limits=c(-120, 120), expand=expansion(mult=0),breaks = seq(-120, 120, 40))
}

#--Producing 8 State Plots from Kenny et al. Paper-------------------------------------------
pa_graph <- plot_tractpoperror_vs_tractsize("42", "Pennsylvania")
nc_graph <- plot_tractpoperror_vs_tractsize("37", "North Carolina")
sc_graph <- plot_tractpoperror_vs_tractsize("45", "South Carolina")
la_graph <- plot_tractpoperror_vs_tractsize("22", "Louisiana")
al_graph <- plot_tractpoperror_vs_tractsize("1", "Alabama")
de_graph <- plot_tractpoperror_vs_tractsize("10", "Delaware")
ut_graph <- plot_tractpoperror_vs_tractsize("49", "Utah")
wa_graph <- plot_tractpoperror_vs_tractsize("53", "Washington")

tpe_ts_figure <- ggarrange(pa_graph, nc_graph, sc_graph, la_graph, al_graph, de_graph, ut_graph, wa_graph, ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
