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
  select(gisjoin, H72001_dp, H72003_dp, H72001_sf, H74001_sf) %>%
  rename(countycode = gisjoin, dp_total = H72001_dp, dp_white = H72003_dp, 
         sf_total = H72001_sf, sf_old = H74001_sf) %>%
  mutate(percent_nonwhite = 100 * (dp_total - dp_white) / dp_total,
         error = dp_total - sf_total, percent_old = 100 * (sf_old/sf_total))




# al
al_data = race_df[1:1181,]

al_race_graph <- ggplot(al_data, aes(x = percent_nonwhite, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + 
  labs(x = "Percent Non-White", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(limits = c(0,100), labels=comma,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-150,150), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
  size = .65, se = FALSE, alpha = 0.5)

# "limits = 0:1" causing issues
  
al_race_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/al_race_graph.pdf", width=7.2, height=5.5)


# de
de_data = race_df[13700:13917,]

de_race_graph <- ggplot(de_data, aes(x = percent_nonwhite, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + 
  labs(x = "Percent Non-White", y = "Error (people)") + 
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=comma,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-150,150), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

de_race_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/de_race_graph.pdf", width=7.2, height=5.5)



#la
la_data = race_df[28304:29451,]

la_race_graph <- ggplot(la_data, aes(x = percent_nonwhite, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + 
  labs(x = "Percent Non-White", y = "Error (people)") + 

  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=comma,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-150,150), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

la_race_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/la_race_graph.pdf", width=7.2, height=5.5)



# nc
nc_data = race_df[48115:50309,]

nc_race_graph <- ggplot(nc_data, aes(x = percent_nonwhite, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + 
  labs(x = "Percent Non-White", y = "Error (people)") + 
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=comma,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-150,150), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

nc_race_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/nc_race_graph.pdf", width=7.2, height=5.5)



# pa
pa_data = race_df[55347:58564,]

pa_race_graph <- ggplot(pa_data, aes(x = percent_nonwhite, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + 
  labs(x = "Percent Non-White", y = "Error (people)") + 
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=comma,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-150,150), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

pa_race_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/pa_race_graph.pdf", width=7.2, height=5.5)



# sc
sc_data = race_df[58809:59911,]

sc_race_graph <- ggplot(sc_data, aes(x = percent_nonwhite, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + labs(x = "Percent Non-White", y = "Error (people)") + 
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=comma,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-150,150), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

sc_race_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/sc_race_graph.pdf", width=7.2, height=5.5)



# ut
ut_data = race_df[66896:67483,]

ut_race_graph <- ggplot(ut_data, aes(x = percent_nonwhite, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + 
  labs(x = "Percent Non-White", y = "Error (people)") + 
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=comma,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-150,150), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

ut_race_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/ut_race_graph.pdf", width=7.2, height=5.5)


# wa
wa_data = race_df[69575:71032,]

wa_race_graph <- ggplot(wa_data, aes(x = percent_nonwhite, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + 
  labs(x = "Percent Non-White", y = "Error (people)") + 
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=comma,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-150,150), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

wa_race_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/wa_race_graph.pdf", width=7.2, height=5.5)



race_figure <- ggarrange(pa_race_graph, nc_race_graph, sc_race_graph, la_race_graph, al_race_graph,
                    de_race_graph, ut_race_graph, wa_race_graph,
                    ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
race_figure
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/race_figure.pdf", width = 9.54, height = 5.81)
