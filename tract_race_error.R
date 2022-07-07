library(tidyverse)
library(scales)
library(redist)
library(patchwork)
library(sf)
library(mgcv)
library(ggplot2)

# load helpers ----
source("...dataverse_files/R/00_custom_functions.R")

tract_data = read_csv("...nhgis_ppdd_20210428_12-2_tract/nhgis_ppdd_20210428_12-2_tract.csv")


dp_total = tract_data[,4]
dp_white = tract_data[,6]
dp_nonwhite = dp_total - dp_white
percent_nonwhite = 100*(dp_nonwhite/dp_total)
sf_total = tract_data[,305]

dp_total[1,]              # 1928
dp_white[1,]              # 1618
dp_nonwhite[1,]           # 310
percent_nonwhite[1,]      # 16.079
sf_total[1,]              # 1912

error = dp_total - sf_total


# checked
al_data = tract_data[1:1181,]
al_data[1,4]
al_percent_nonwhite = percent_nonwhite[1:1181,]
al_error = error[1:1181,]

al_percent_nonwhite[1]    # 16.079
al_percent_nonwhite[1181] # 4.906
al_error[1]               # 16
al_error[1181]            # 23

al_graph <- ggplot(al_data, aes(x = al_percent_nonwhite, y = al_error, color = al_error)) +
  geom_point() + labs(x = "Percent Non-White", y = "Error (people)") +
  stat_smooth(method = "lm",
              col = "#4e0707",
              se = FALSE,
              size = 1)
al_graph
ggsave("...nhgis_ppdd_20210428_12-2_tract/figs/al.pdf", width=7.2, height=5.5)


# checked
de_data = tract_data[13700:13917,]
de_percent_nonwhite = percent_nonwhite[13700:13917,]
de_error = error[13700:13917,]

de_percent_nonwhite[1]    # 13.153
de_percent_nonwhite[218]  # NaN
de_error[1]               # 5
de_error[218]             # 0

de_graph <- ggplot(de_data, aes(x = de_percent_nonwhite, y = de_error, color = de_error)) +
  geom_point() + labs(x = "Percent Non-White", y = "Error (people)") +
  stat_smooth(method = "lm",
              col = "#4e0707",
              se = FALSE,
              size = 1)
de_graph
ggsave("...nhgis_ppdd_20210428_12-2_tract/figs/de.pdf", width=7.2, height=5.5)


# checked
la_data = tract_data[28304:29451,]
la_percent_nonwhite = percent_nonwhite[28304:29451,]
la_error = error[28304:29451,]

la_percent_nonwhite[1]    # 33.893
la_percent_nonwhite[1148] # 53.991
la_error[1]               # 36
la_error[1148]            # -10

la_graph <- ggplot(la_data, aes(x = la_percent_nonwhite, y = la_error, color = la_error)) +
  geom_point() + labs(x = "Percent Non-White", y = "Error (people)") +
  stat_smooth(method = "lm",
              col = "#000000",
              se = FALSE,
              size = 1)
la_graph
ggsave("...nhgis_ppdd_20210428_12-2_tract/figs/la.pdf", width=7.2, height=5.5)


# checked
nc_data = tract_data[48115:50309,]
nc_percent_nonwhite = percent_nonwhite[48115:50309,]
nc_error = error[48115:50309,]

nc_percent_nonwhite[1]    # 31.767
nc_percent_nonwhite[2195] # 2.880
nc_error[1]               # -36
nc_error[2195]            # 1

nc_graph <- ggplot(nc_data, aes(x = nc_percent_nonwhite, y = nc_error, color = nc_error)) +
  geom_point() + labs(x = "Percent Non-White", y = "Error (people)") +
  stat_smooth(method = "lm",
              col = "#000000",
              se = FALSE,
              size = 1)
nc_graph
ggsave("...nhgis_ppdd_20210428_12-2_tract/figs/nc.pdf", width=7.2, height=5.5)


# checked
pa_data = tract_data[55347:58564,]
pa_percent_nonwhite = percent_nonwhite[55347:58564,]
pa_error = error[55347:58564,]

pa_percent_nonwhite[1]    # 4.045
pa_percent_nonwhite[3218] # 3.005
pa_error[1]               # 16
pa_error[3218]            # 10

pa_graph <- ggplot(pa_data, aes(x = pa_percent_nonwhite, y = pa_error, color = pa_error)) +
  geom_point() + labs(x = "Percent Non-White", y = "Error (people)") +
  stat_smooth(method = "lm",
              col = "#000000",
              se = FALSE,
              size = 1)
pa_graph
ggsave("...nhgis_ppdd_20210428_12-2_tract/figs/pa.pdf", width=7.2, height=5.5)


# checked
sc_data = tract_data[58809:59911,]
sc_percent_nonwhite = percent_nonwhite[58809:59911,]
sc_error = error[58809:59911,]

sc_percent_nonwhite[1]    # 14.434
sc_percent_nonwhite[1103] # 15.541
sc_error[1]               # 16
sc_error[1103]            # -7

sc_graph <- ggplot(sc_data, aes(x = sc_percent_nonwhite, y = sc_error, color = sc_error)) +
  geom_point() + labs(x = "Percent Non-White", y = "Error (people)") +
  stat_smooth(method = "lm",
              col = "#000000",
              se = FALSE,
              size = 1)
sc_graph
ggsave("...nhgis_ppdd_20210428_12-2_tract/figs/sc.pdf", width=7.2, height=5.5)


# checked
ut_data = tract_data[66896:67483,]
ut_percent_nonwhite = percent_nonwhite[66896:67483,]
ut_error = error[66896:67483,]

ut_percent_nonwhite[1]    # 10.752
ut_percent_nonwhite[588]  # 7.237
ut_error[1]               # -14
ut_error[588]             # -64

ut_graph <- ggplot(ut_data, aes(x = ut_percent_nonwhite, y = ut_error, color = ut_error)) +
  geom_point() + labs(x = "Percent Non-White", y = "Error (people)") +
  stat_smooth(method = "lm",
              col = "#000000",
              se = FALSE,
              size = 1)
ut_graph
ggsave("...nhgis_ppdd_20210428_12-2_tract/figs/ut.pdf", width=7.2, height=5.5)


# checked
wa_data = tract_data[69575:71032,]
wa_percent_nonwhite = percent_nonwhite[69575:71032,]
wa_error = error[69575:71032,]

wa_percent_nonwhite[1]    # 6.832
wa_percent_nonwhite[1458] # 71.367
wa_error[1]               # 56
wa_error[1458]            # 47

wa_graph <- ggplot(wa_data, aes(x = wa_percent_nonwhite, y = wa_error, color = wa_error)) +
  geom_point() + labs(x = "Percent Non-White", y = "Error (people)") +
  stat_smooth(method = "lm",
              col = "#000000",
              se = FALSE,
              size = 1)
wa_graph
ggsave("...nhgis_ppdd_20210428_12-2_tract/figs/wa.pdf", width=7.2, height=5.5)

