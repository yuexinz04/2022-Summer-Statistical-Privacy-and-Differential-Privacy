
#--Set up--------------------------------------------------------------------
setwd("/Users/AnnaYuexinZhang/Documents/Rutgers/Work/2022 Summer Project SUPER")
alltractdata_122<-read.csv('./Vintage210428_122_Tract/nhgis_ppdd_20210428_12-2_tract.csv')
library(dplyr)
library(gam)
library(tibble)
#install.packages("ggplot2")
library(ggplot2)
library(scales)
library(RColorBrewer)

#--Occupancy Rate & Vacancy Rate---------------------------------------------
tract_orsf<- alltractdata_122 %>%
  select(gisjoin,IFE001_sf,IFE002_sf,IFE003_sf) %>%
  rename(countycode=gisjoin,total_vrsf=IFE001_sf, 
         occupied_vrsf=IFE002_sf,vacant_vrsf=IFE003_sf) %>%
  mutate(vacancyrate_sf=vacant_vrsf/total_vrsf,
         occupancyrate_sf=occupied_vrsf/total_vrsf,
         .before=total_vrsf)

tract_ordp<- alltractdata_122 %>%
  select(gisjoin,IFE001_dp,IFE002_dp,IFE003_dp) %>%
  rename(countycode=gisjoin, total_vrdp=IFE001_dp,
         occupied_vrdp=IFE002_dp,vacant_vrdp=IFE003_dp) %>%
  mutate(vacancyrate_dp=vacant_vrdp/total_vrdp,
         occupancyrate_dp=occupied_vrdp/total_vrdp,
         .before=total_vrdp)

tract_vr<- left_join(tract_orsf,tract_ordp, by='countycode') %>%
  mutate(vacancyrate_diff=vacancyrate_sf-vacancyrate_dp,
         occupancyrate_diff=occupancyrate_sf-occupancyrate_dp,
         occupancy_diff = occupied_vrsf-occupied_vrdp,
         .after=countycode)

#--Herfindahl-Hirschman Index-----------------------------------------------
tract_hhisf <- alltractdata_122 %>%
  select (gisjoin,state,H72001_sf,H72002_sf,H72003_sf,H72003_sf,H72004_sf,H72005_sf,
         H72006_sf,H72007_sf) %>%
  rename (countycode = gisjoin, tract_total = H72001_sf, popoONErace_sf = H72002_sf, 
         popWhite_sf = H72003_sf, popBAA_sf = H72004_sf, popAIAN_sf = H72005_sf, 
         popAsian_sf = H72006_sf, popNHOPI_sf = H72007_sf ) %>%
  mutate (s_whitesf = popWhite_sf/popoONErace_sf,
         s_BAAsf = popBAA_sf/popoONErace_sf,
         s_AIANsf = popAIAN_sf/popoONErace_sf,
         s_Asiansf = popAsian_sf/popoONErace_sf,
         s_NHOPIsf = popNHOPI_sf/popoONErace_sf,
         s_othersf = (tract_total-popWhite_sf-popBAA_sf-popAIAN_sf-popAsian_sf-popNHOPI_sf)/popoONErace_sf)%>%
  mutate (hhi_sf=s_whitesf^2+s_BAAsf^2+s_AIANsf^2+s_Asiansf^2+s_NHOPIsf^2+s_othersf^2,.after=countycode)
  
tract_hhisf <- tract_hhisf %>% mutate (max_race=colnames(tract_hhisf[11:16])[max.col(tract_hhisf[11:16])]) %>%
  mutate (largest_race = recode_factor(max_race, "s_whitesf"= "white","s_BAAsf"="black",
                                      "s_AIANsf"="aian","s_Asiansf"="asian",
                                      "s_NHOPIsf"="nativehawaiian","s_othersf"="other"))

#tract_hhidp <- alltractdata_122 %>%
#  select(gisjoin,H72002_dp,H72003_dp,H72003_dp,H72004_dp,H72005_dp,
#         H72006_dp,H72007_dp,H72008_dp) %>%
#  rename(countycode=gisjoin, popoONErace_dp = H72002_dp, popWhite_dp = H72003_dp,
#         popBAA_dp = H72004_dp, popAIAN_dp = H72005_dp , popAsian_dp = H72006_dp,
#         popNHOPI_dp = H72007_dp, popOther_dp = H72008_dp ) %>%
#  mutate(s_whitedp = popWhite_dp/popoONErace_dp,
#         s_BAAdp = popBAA_dp/popoONErace_dp,
#         s_AIANdp = popAIAN_dp/popoONErace_dp,
#         s_Asiandp = popAsian_dp/popoONErace_dp,
#         s_NHOPIdp = popNHOPI_dp/popoONErace_dp,
#         s_otherdp = popOther_dp/popoONErace_dp)%>%
#  mutate (hhi_dp=s_whitedp^2+s_BAAdp^2+s_AIANdp^2+s_Asiandp^2+s_NHOPIdp^2+s_otherdp^2,.after=countycode)

#tract_hhi<- left_join(tract_hhisf,tract_hhidp, by='countycode') %>%
#  mutate (hhi_diff = hhi_sf - hhi_dp,.after=countycode)

#--Join Data Frame--------------------------------------------------------
tract_vrhhi <- left_join(tract_hhisf, tract_vr, by='countycode')%>%
  mutate (x1test=hhi_sf*100,
          ytest =occupancyrate_diff*100 )

clean_tract_vehhi<-as(tract_vrhhi,"data.frame") %>%
  filter(!is.na(ytest), !is.na(x1test))

range(clean_tract_vehhi$ytest)
# -1 1
range(clean_tract_vehhi$occupancy_diff)
# -50  55


#--Tract Data by State----------------------------------------------
# 5 Geographic Regions:Northeast, Southeast, Midwest, Southwest, and West
# https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States#5_Geographic_Regions
nj_plotdf <-clean_tract_vehhi %>%
  filter(state=="34") %>%
  select(occupancy_diff,occupancyrate_diff,hhi_sf,largest_race,tract_total)

sc_plotdf <-clean_tract_vehhi %>%
  filter(state=="37") %>%
  select(occupancy_diff,occupancyrate_diff,hhi_sf,largest_race,tract_total)

mi_plotdf <-clean_tract_vehhi %>%
  filter(state=="26") %>%
  select(occupancy_diff,occupancyrate_diff,hhi_sf,largest_race,tract_total)

tx_plotdf <-clean_tract_vehhi %>%
  filter(state=="48") %>%
  select(occupancy_diff,occupancyrate_diff,hhi_sf,largest_race,tract_total)

ca_plotdf <-clean_tract_vehhi %>%
  filter(state=="6") %>%
  select(occupancy_diff,occupancyrate_diff,hhi_sf,largest_race,tract_total)

#--Plotting ordiff/odiff VS hhi----------------------------------------------
# for absolute occupancy error
ggplot(ca_plotdf, aes(x=hhi_sf, y=occupancy_diff, 
                      color=largest_race,size=tract_total)) +
  geom_point(alpha = 0.65)+
  geom_hline(yintercept=0, lty="dashed")+
  geom_smooth(se = FALSE, color = "black", size = 0.65)+
  ggtitle("California Census Tracts") +
  labs(x = "Herfindahl-Hirschman Index", y="Occupancy Absolute Error",
       color="Largest Racial Group")+
  theme_bw() +
  scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3),
                  oob=squish)+
  scale_x_continuous(labels = percent, limits = c(0.2, 1), breaks = seq(0.2, 1.0, 0.2)) +
  scale_y_continuous(limits=c(-50, 50)) +
  theme(text = element_text(family = "Times New Roman"))+
  geom_line(stat="smooth", method = gam, 
            formula = y~s(x, bs = "cs"), color = "#222222", 
            size = 1.5, se = FALSE, alpha = 0.5) 

# for occupancy rate error
ggplot(ca_plotdf, aes(x=hhi_sf, y=occupancyrate_diff, 
                      color=largest_race,size=tract_total)) +
  geom_point(alpha = 0.65)+
  geom_hline(yintercept=0, lty="dashed")+
  geom_smooth(se = FALSE, color = "black", size = 0.65)+
  ggtitle("California Census Tracts") +
  labs(x = "Herfindahl-Hirschman Index", y="Occupancy Absolute Error",
       color="Largest Racial Group")+
  theme_bw() +
  scale_x_continuous(labels = percent, limits = c(0.2, 1), breaks = seq(0.2, 1.0, 0.2)) +
  scale_y_continuous(limits=c(-1, 1)) +
  theme(text = element_text(family = "Times New Roman"))+
  geom_line(stat="smooth", method = gam, 
            formula = y~s(x, bs = "cs"), color = "#222222", 
            size = 1.5, se = FALSE, alpha = 0.5) 
            
            
