library(tidyverse)
library(ggplot2)
library(ggpubr)
library(scales)
library(redist)
library(patchwork)
library(sf)
library(mgcv)
theme_set(theme_pubr())


source("/Users/leahghazali/Desktop/dataverse_files/R/00_custom_functions.R")

tract_data = read_csv("/Users/leahghazali/Desktop/nhgis_ppdd_20210428_12-2_tract/nhgis_ppdd_20210428_12-2_tract.csv")


df <- tract_data %>%
  select(gisjoin, state, H72001_dp, H72001_sf, H72003_sf) %>%
  rename(countycode = gisjoin,  dp_total = H72001_dp, sf_total = H72001_sf, sf_white = H72003_sf) %>%
  mutate(percent_nonwhite = 100 * (sf_total - sf_white) / sf_total, error = dp_total - sf_total)


plot_statePop_vs_error = function(stateNum, stateName) {
  
  state_df <- subset(df, state == stateNum)
  title <- paste(stateName)
  
  ggplot(data = state_df, aes(x = sf_total, y = error, color = percent_nonwhite)) +
    geom_point(alpha = .6, size = .5) + labs(x = "Population", y = "Error (people)", color="Percent Non-white") +
    theme_bw() +
    ggtitle(title) +
    geom_hline(yintercept=0, lty="dashed") + 
    scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
    scale_color_viridis_c(option="A", begin = .3) +
    scale_x_continuous(labels=comma,expand=expansion(mult=0)) +
    scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
    geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222", 
              size = .65, se = FALSE, alpha = 0.5)
  
}

# Plotting all states

state_data = read.csv("/Users/leahghazali/Desktop/nhgis_ppdd_20210428_12-2_state/nhgis_ppdd_20210428_12-2_state.csv")

state_df <- state_data %>%
  select(name, state)


i = 1
while (i <= 52) {
  state_name = state_df$name[i]
  state_num = state_df$state[i]
  
  if (nchar(state_num) == 1) {
    new_num <- c("0",state_num)
    state_num <- paste(new_num, collapse = "")
  }
  
  figure <- plot_statePop_vs_error(state_num, state_name)
  path <- c("/Users/leahghazali/Desktop/nhgis_ppdd_20210428_12-2_tract/figs/", state_name, ".pdf")
  figure_path = paste(path, collapse = "")
  ggsave(figure_path)
  i = i + 1
}
