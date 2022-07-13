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
  select(gisjoin,H72001_dp,H72001_sf,H72003_sf,H72004_sf,H72005_sf,
         H72006_sf,H72007_sf,H72008_sf,H72009_sf, H74001_sf) %>%
  rename(countycode = gisjoin, dp_total = H72001_dp, sf_total = H72001_sf, sf_white_pop = H72003_sf,
         sf_BAA_pop = H72004_sf, sf_AIAN_pop = H72005_sf , sf_asian_pop = H72006_sf,
         sf_NHOPI_pop = H72007_sf, sf_other_pop = H72008_sf, sf_two_races = H72009_sf, sf_old = H74001_sf) %>%
  mutate(s_white_sf = sf_white_pop/sf_total,
         s_BAA_sf = sf_BAA_pop/sf_total,
         s_AIAN_sf = sf_AIAN_pop/sf_total,
         s_Asian_sf = sf_asian_pop/sf_total,
         s_NHOPI_sf = sf_NHOPI_pop/sf_total,
         s_other_sf = (sf_other_pop + sf_two_races)/sf_total,
         percent_old = 100 * (sf_old/sf_total))%>%
  mutate (hhi = s_white_sf^2+s_BAA_sf^2+s_AIAN_sf^2+s_Asian_sf^2+s_NHOPI_sf^2+s_other_sf^2,.after=countycode,
          error = dp_total - sf_total)




# al
al_data = hhi_df[1:1181,]

al_hhi_graph <- ggplot(al_data, aes(x = hhi, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + labs(x = "HHI", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

al_hhi_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/al_hhi_graph.pdf", width=7.2, height=5.5)


# de
de_data = hhi_df[13700:13917,]

de_hhi_graph <- ggplot(de_data, aes(x = hhi, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + labs(x = "HHI", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

de_hhi_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/de_hhi_graph.pdf", width=7.2, height=5.5)



#la
la_data = hhi_df[28304:29451,]

la_hhi_graph <- ggplot(la_data, aes(x = hhi, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + labs(x = "HHI", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

la_hhi_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/la_hhi_graph.pdf", width=7.2, height=5.5)



# nc
nc_data = hhi_df[48115:50309,]

nc_hhi_graph <- ggplot(nc_data, aes(x = hhi, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + labs(x = "HHI", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

nc_hhi_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/nc_hhi_graph.pdf", width=7.2, height=5.5)



# pa
pa_data = hhi_df[55347:58564,]

pa_hhi_graph <- ggplot(pa_data, aes(x = hhi, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + labs(x = "HHI", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

pa_hhi_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/pa_hhi_graph.pdf", width=7.2, height=5.5)



# sc
sc_data = hhi_df[58809:59911,]

sc_hhi_graph <- ggplot(sc_data, aes(x = hhi, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + labs(x = "HHI", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

sc_hhi_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/sc_hhi_graph.pdf", width=7.2, height=5.5)



# ut
ut_data = hhi_df[66896:67483,]

ut_hhi_graph <- ggplot(ut_data, aes(x = hhi, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5) + labs(x = "HHI", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

ut_hhi_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/ut_hhi_graph.pdf", width=7.2, height=5.5)


# wa
wa_data = hhi_df[69575:71032,]

wa_hhi_graph <- ggplot(wa_data, aes(x = hhi, y = error, color = percent_old)) +
  geom_point(alpha = .4, size = .5, size = .5) + labs(x = "HHI", y = "Error (people)", color="Population") +
  geom_hline(yintercept=0, lty="dashed") + 
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3), oob=squish) + 
  scale_color_viridis_c(option="A", labels = percent, begin = .3) +
  scale_x_continuous(labels=percent,expand=expansion(mult=0)) +
  scale_y_continuous(limits=c(-80, 80), expand=expansion(mult=0)) + 
  geom_line(stat="smooth", method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
            size = .65, se = FALSE, alpha = 0.5)

wa_hhi_graph
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/wa_hhi_graph.pdf", width=7.2, height=5.5)



hhi_figure <- ggarrange(pa_hhi_graph, nc_hhi_graph, sc_hhi_graph, la_hhi_graph, al_hhi_graph,
                         de_hhi_graph, ut_hhi_graph, wa_hhi_graph,
                         ncol = 4, nrow = 2, common.legend = TRUE, legend = "bottom")
hhi_figure
ggsave(".../nhgis_ppdd_20210428_12-2_tract/figs/hhi_figure.pdf", width = 9.54, height = 5.81)
