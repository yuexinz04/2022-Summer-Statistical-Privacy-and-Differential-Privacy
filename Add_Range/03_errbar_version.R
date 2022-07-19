library(here)
library(tidyverse)
library(scales)
library(redist)
library(patchwork)
library(sf)
library(mgcv)

# load helpers ----
source(here("R/00_custom_functions.R"))

# design ----
design <- "ABCD
           EFGH"

# Load data ---------------------------------------------------------------
sf::sf_use_s2(FALSE)
nc_error = read_rds(here("data/NC/nc_shp.rds")) %>%
    transmute(precinct = VTD, # VTD unique identifier
              cd = cd_17,
              D = EL12G_GV_D, #Number of votes for 2012 Democratic gubernatorial candidate
              R = EL12G_GV_R, #Number of votes for 2012 Republican gubernatorial candidate
              voters = EL12G_GV_TOT, # voter numbers
              pop_das12 = v12_pop,# das12 total population
              pop_orig = pop, # confidential total population, all ages
              vap_orig = vap, # voting age population, 18 and above
              white = pop_white / pop, # white pop percentage
              black = pop_black / pop,
              asian = pop_asian / pop,
              hisp = pop_hisp / pop,
              otherrace = (1 - white - black - asian - hisp),
              error = pop_das12 - pop_orig, # total population difference
              error_black = v12_pop_black - pop_black,
              error_white = v12_pop_white - pop_white,
              # non-white population difference v12 vs confidential
              error_min = (v12_pop - v12_pop_white) - (pop - pop_white),
              # white populaion difference v12 vs confidential
              error_other = (v12_pop - v12_pop_white) - (pop - pop_white) - error_black,
              # take log on total population difference, v12 vs confidential
              lerror = log(pop_das12) - log(pop_orig),
              # percent of eligible people who did vote??
              turnout = voters / vap_orig, 
              hectares = as.numeric(st_area(st_geometry(.)))/1e4,
              dens = pop_orig / hectares,
              # percentage of voting for Democratic gubernatorial candidate
              dem = D / (D + R)) %>% 
# Round 1
# head(nc_error$perturb_error) [1] 136  24  62  33 -15 -26
#        mutate(set.seed(length(error)), 
#        perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%


# Round 2
# head(nc_error$perturb_error) [1] -21   2  34  -4  77  64
#       mutate(set.seed(1),
#       perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
# Round 3
# head(nc_error$perturb_error) [1]  40  -5  10 -39  46   4
#       mutate(set.seed(21),
#       perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)#%>%
#    filter(!is.na(perturb_error))

sc_error = read_rds(here("data/SC/sc_shp.rds")) %>%
    mutate(precinct = row_number()) %>%
    transmute(precinct = precinct,
              cd = cd,
              D = gov_vest_DEM,
              R = gov_vest_REP,
              voters = D + R +gov_vest_OTH,
              pop_das12 = v12_pop,
              pop_orig = pop,
              vap_orig = vap,
              white = pop_white / pop,
              black = pop_black / pop,
              asian = pop_asian / pop,
              hisp = pop_hisp / pop,
              otherrace = (1 - white - black - asian - hisp),
              error = pop_das12 - pop_orig,
              error_black = v12_pop_black - pop_black,
              error_white = v12_pop_white - pop_white,
              error_min = (v12_pop - v12_pop_white) - (pop - pop_white),
              error_other = (v12_pop - v12_pop_white) - (pop - pop_white) - error_black,
              lerror = log(pop_das12) - log(pop_orig),
              turnout = voters / vap_orig,
              hectares = as.numeric(st_area(st_geometry(.)))/1e4,
              dens = pop_orig / hectares,
              dem = D / (D + R)) %>%
    #Round 1 
    #head(sc_error$perturb_error) [1]  -6  38   0  29  57 -18   
    #    mutate(set.seed(length(error)),
    #           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 2
    #head(sc_error$perturb_error) [1]  23 -58  -2 -24  19  72
#    mutate(set.seed(2),
#          perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
# Round 3
    #head(sc_error$perturb_error) [1]   1  28 -34 -93 -24  -8
#    mutate(set.seed(22),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)#%>%
  #  filter(!is.na(perturb_error))

vtd_das12 = read_rds(here("data/PA/pa_vtd_das_12.rds"))
vtd_das4 = read_rds(here("data/PA/pa_vtd_das_04.rds")) %>%
    rename_with(~ str_c(., "_das4"), -precinct)
vtd_orig = read_rds(here("data/PA/pa_vtd_orig.rds"))
pa_shp = read_rds(here("data/PA/pa_shp.rds")) %>%
    sf::st_as_sf()
vtds = inner_join(pa_shp, vtd_orig, by="precinct") %>%
    inner_join(vtd_das12, by="precinct", suffix=c("_orig", "_das12")) %>%
    inner_join(vtd_das4, by="precinct") %>%
    mutate(hectares = as.numeric(sf::st_area(pa_shp))/1e4)

pa_error = vtds %>%
    transmute(precinct = precinct,
              county = county,
              cd = cd,
              D = ndv,
              R = nrv,
              voters = D + R,
              pop_das12 = pop_das12,
              pop_orig = pop_orig,
              vap_orig = vap_orig,
              white = pop_white_orig / pop_orig,
              black = pop_black_orig / pop_orig,
              asian = pop_asian_orig / pop_orig,
              hisp = pop_hisp_orig / pop_orig,
              otherrace = (1 - white - black - asian - hisp),
              error = pop_das12 - pop_orig,
              error_black = pop_black_das12 - pop_black_orig,
              error_white = pop_white_das12 - pop_white_orig,
              error_min = (pop_das12 - pop_white_das12) - (pop_orig - pop_white_orig),
              error_other = (pop_das12 - pop_white_das12) - (pop_orig - pop_white_orig) - error_black,
              lerror = log(pop_das12) - log(pop_orig),
              turnout = voters / vap_orig,
              hectares = as.numeric(st_area(st_geometry(.)))/1e4,
              dens = pop_orig / hectares,
              dem = D / (D + R)) %>%
    
    #Round 1 
    #head(pa_error$perturb_error) [1]   2  -3   7  18 -15 -13 
    #    mutate(set.seed(length(error)),
    #           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 2
    #head(pa_error$perturb_error) [1]  -4   8  -6 -10 -28 -12
#    mutate(set.seed(3),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%    
    
    # Round 3
    #head(pa_error$perturb_error) [1]  10   7 -18  13   0 -20
#    mutate(set.seed(23),
#         perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%

    as_tibble() %>%
    filter(voters > 0, turnout <= 1)#%>%
 #   filter(!is.na(perturb_error))

la_error = read_rds(here("data/LA/la.rds")) %>%
    mutate(precinct = row_number()) %>%
    transmute(precinct = precinct,
              cd = cd,
              D = sos_vest_DEM_gen,
              R = sos_vest_REP_gen,
              voters = D + R + sos_vest_OTH_gen,
              pop_das12 = v12_pop,
              pop_orig = pop,
              vap_orig = vap,
              white = pop_white / pop,
              black = pop_black / pop,
              asian = pop_asian / pop,
              hisp = pop_hisp / pop,
              otherrace = (1 - white - black - asian - hisp),
              error = pop_das12 - pop_orig,
              error_black = v12_pop_black - pop_black,
              error_white = v12_pop_white - pop_white,
              error_min = (v12_pop - v12_pop_white) - (pop - pop_white),
              error_other = (v12_pop - v12_pop_white) - (pop - pop_white) - error_black,
              lerror = log(pop_das12) - log(pop_orig),
              turnout = voters / vap_orig,
              hectares = as.numeric(st_area(st_geometry(.)))/1e4,
              dens = pop_orig / hectares,
              dem = D / (D + R)) %>%
    
    #Round 1 
    #head(la_error$perturb_error) [1]  26  23 -42  -1   1  29
    #    mutate(set.seed(length(error)),
    #           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 2
    #head(la_error$perturb_error) [1] -16   2 -22 -15 -25  12
#    mutate(set.seed(4),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 3
    #head(la_error$perturb_error) [1]  10 -13  39  -9   5   3
#    mutate(set.seed(24),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)#%>%
#    filter(!is.na(perturb_error))

la_error$dem[930] <- 0

al_error <- read_rds(here("data/AL/al.rds")) %>%
    mutate(precinct = row_number()) %>%
    transmute(precinct = precinct,
              cd = cd,
              D = G18GOVDMAD,
              R = G18GOVRIVE,
              voters = D + R,
              pop_das12 = v12_pop,
              pop_orig = pop,
              vap_orig = vap,
              white = pop_white / pop,
              black = pop_black / pop,
              asian = pop_asian / pop,
              hisp = pop_hisp / pop,
              otherrace = (1 - white - black - asian - hisp),
              error = pop_das12 - pop_orig,
              error_black = v12_pop_black - pop_black,
              error_white = v12_pop_white - pop_white,
              error_min = (v12_pop - v12_pop_white) - (pop - pop_white),
              error_other = (v12_pop - v12_pop_white) - (pop - pop_white) - error_black,
              lerror = log(pop_das12) - log(pop_orig),
              turnout = voters / vap_orig,
              hectares = as.numeric(st_area(st_geometry(.)))/1e4,
              dens = pop_orig / hectares,
              dem = D / (D + R)) %>%
    
    
    #Round 1 
    #head(al_error$perturb_error) [1]   8  25  -1   1  12 -16
    #    mutate(set.seed(length(error)),
    #           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 2
    #head(al_error$perturb_error) 1]  11  46   0   9 -18  10
#    mutate(set.seed(5),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 3
    #head(al_error$perturb_error) [1]  -3 -20   3  21   1  -6
#    mutate(set.seed(25),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)#%>%
#    filter(!is.na(perturb_error))


de_error <- read_rds(here("data/DE/de.rds")) %>%
    mutate(precinct = row_number()) %>%
    transmute(precinct = precinct,
              cd = cd,
              D = G20PREDBID,
              R = G20PRERTRU,
              voters = D + R,
              pop_das12 = v12_pop,
              pop_orig = pop,
              vap_orig = vap,
              white = pop_white / pop,
              black = pop_black / pop,
              asian = pop_asian / pop,
              hisp = pop_hisp / pop,
              otherrace = (1 - white - black - asian - hisp),
              error = pop_das12 - pop_orig,
              error_black = v12_pop_black - pop_black,
              error_white = v12_pop_white - pop_white,
              error_min = (v12_pop - v12_pop_white) - (pop - pop_white),
              error_other = (v12_pop - v12_pop_white) - (pop - pop_white) - error_black,
              lerror = log(pop_das12) - log(pop_orig),
              turnout = voters / vap_orig,
              hectares = as.numeric(st_area(st_geometry(.)))/1e4,
              dens = pop_orig / hectares,
              dem = D / (D + R)) %>%
    
    
    #Round 1 
    #head(de_error$perturb_error) [1]  -8 -18 -23  15 -59  78
    #    mutate(set.seed(length(error)),
    #           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 2
    #head(de_error$perturb_error) [1] -95  11 -20   7  -8 -18
 #  mutate(set.seed(6),
 #          perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 3
    #head(de_error$perturb_error) [1] -20  14  78  -2  12   4
#    mutate(set.seed(26),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)#%>%
 #   filter(!is.na(perturb_error))


wa_error <- read_rds(here("data/WA/wa.rds")) %>%
    mutate(precinct = row_number()) %>%
    transmute(precinct = precinct,
              cd = cd,
              D = G18USSDCAN,
              R = G18USSRHUT,
              voters = D + R,
              pop_das12 = v12_pop,
              pop_orig = pop,
              vap_orig = vap,
              white = pop_white / pop,
              black = pop_black / pop,
              asian = pop_asian / pop,
              hisp = pop_hisp / pop,
              otherrace = (1 - white - black - asian - hisp),
              error = pop_das12 - pop_orig,
              error_black = v12_pop_black - pop_black,
              error_white = v12_pop_white - pop_white,
              error_min = (v12_pop - v12_pop_white) - (pop - pop_white),
              error_other = (v12_pop - v12_pop_white) - (pop - pop_white) - error_black,
              lerror = log(pop_das12) - log(pop_orig),
              turnout = voters / vap_orig,
              hectares = as.numeric(st_area(st_geometry(.)))/1e4,
              dens = pop_orig / hectares,
              dem = D / (D + R)) %>%
    
    #Round 1 
    #head(wa_error$perturb_error) [1]  -4  -7 -56 -10   0  -5
    #    mutate(set.seed(length(error)),
    #           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 2
    #head(wa_error$perturb_error) [1]   1  25  30 -13   3 -40
 #   mutate(set.seed(7),
 #          perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 3
    #head(wa_error$perturb_error) [1]  17  31 -14   0   4 -11
#    mutate(set.seed(27),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    as_tibble() %>%
    filter(voters > 0, turnout <= 1) #%>%
 #   filter(!is.na(perturb_error))
    
ut_error <- read_rds(here("data/UT/ut.rds")) %>%
    st_make_valid() %>%
    mutate(precinct = row_number()) %>%
    transmute(precinct = precinct,
              cd = cd,
              D = G18USSDWIL,
              R = G18USSRROM,
              voters = D + R + G18USSDWIL + G18USSIMCC + G18USSLBOW,
              pop_das12 = v12_pop,
              pop_orig = pop,
              vap_orig = vap,
              white = pop_white / pop,
              black = pop_black / pop,
              asian = pop_asian / pop,
              hisp = pop_hisp / pop,
              otherrace = (1 - white - black - asian - hisp),
              error = pop_das12 - pop_orig,
              error_black = v12_pop_black - pop_black,
              error_white = v12_pop_white - pop_white,
              error_min = (v12_pop - v12_pop_white) - (pop - pop_white),
              error_other = (v12_pop - v12_pop_white) - (pop - pop_white) - error_black,
              lerror = log(pop_das12) - log(pop_orig),
              turnout = voters / vap_orig,
              hectares = as.numeric(st_area(st_geometry(.)))/1e4,
              dens = pop_orig / hectares,
              dem = D / (D + R)) %>%
    
    
    #Round 1 
    #head(ut_error$perturb_error) [1] -79  -8 -16  -9   1  -2
#        mutate(set.seed(length(error)),
  #             perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 2
    #head(ut_error$perturb_error) [1]  -1 -30 -14 -10 -30  31
#    mutate(set.seed(8),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    # Round 3
    #head(ut_error$perturb_error) [1]   6  28 -32  -7 -18  -6
#    mutate(set.seed(28),
#           perturb_error=sample(x=(error), size=length(error), replace= FALSE))%>%
    
    as_tibble() %>%
    filter(voters > 0, turnout <= 1) #%>%
 #   filter(!is.na(perturb_error))


#' Compute and add column for herfindahl
add_herf <-  function(tbl) {
    hh <- tbl %>%
        select(precinct, white, black, asian, hisp, otherrace) %>%
        pivot_longer(-precinct) %>%
        group_by(precinct) %>%
        summarize(max_race = name[which.max(value)],
                  hh = sum(value^2))

    tbl %>%
        left_join(hh, by = "precinct")
}


state_errors = list(PA = pa_error,
                    NC = nc_error,
                    SC = sc_error,
                    LA = la_error,
                    AL = al_error,
                    DE = de_error,
                    UT = ut_error,
                    WA = wa_error) %>%
    map(add_herf)


# Fit models and make plots -----------------------------------------------

fit_model = function(d) {
    gam(error ~ t2(turnout, dem, log(dens)) + s(white) + s(hh), data = d)
    #gam(perturb_error ~ t2(turnout, dem, log(dens)) + s(white) + s(hh), data = d)
    }

# map functions transform input by applying a function to each element 
# and returning a vector the same length as the input.
state_models = map(state_errors, fit_model)

# Rsq (for nc)

# original version:
err_df <- tibble(error = nc_error$error, fitted = state_models$NC$fitted.values)
1 - sum((err_df$fitted - err_df$error)^2) / sum((err_df$error - mean(err_df$error))^2)
# 0.1090283 

#perturbed version:
#err_df <- tibble(perturb_error = nc_error$perturb_error, fitted = state_models$NC$fitted.values)
#1 - sum((err_df$fitted - err_df$perturb_error)^2) / sum((err_df$perturb_error - mean(err_df$perturb_error))^2)
# 1st Round: 0.0130383
# 2nd Round: 0.009047041
# 3rd Round: 0.01800035

# Bind SD to data frame
pa_predict=predict(state_models$PA,se.fit=T)
nc_predict=predict(state_models$NC,se.fit=T)
sc_predict=predict(state_models$SC,se.fit=T)
la_predict=predict(state_models$LA,se.fit=T)

al_predict=predict(state_models$AL,se.fit=T)
de_predict=predict(state_models$DE,se.fit=T)
ut_predict=predict(state_models$UT,se.fit=T)
wa_predict=predict(state_models$WA,se.fit=T)

state_errors$PA <- state_errors$PA %>% mutate(se=pa_predict$se.fit)
state_errors$NC <- state_errors$NC %>% mutate(se=nc_predict$se.fit)
state_errors$SC <- state_errors$SC %>% mutate(se=sc_predict$se.fit)
state_errors$LA <- state_errors$LA %>% mutate(se=la_predict$se.fit)

state_errors$AL <- state_errors$AL %>% mutate(se=al_predict$se.fit)
state_errors$DE <- state_errors$DE %>% mutate(se=de_predict$se.fit)
state_errors$UT <- state_errors$UT %>% mutate(se=ut_predict$se.fit)
state_errors$WA <- state_errors$WA %>% mutate(se=wa_predict$se.fit)

# Plotting functions
plot_error_turnout = function(d, m) {
    ggplot(d, aes(dem, fitted(m), size = voters, color = turnout)) +
        geom_hline(yintercept = 0, lty = "dashed") +
        geom_errorbar(aes(ymin=fitted(m)-1.96*se, ymax=fitted(m)+1.96*se), width=.1) +
        geom_point(alpha = 0.4) +
        scale_size_area(max_size = 1.0, labels = comma, limits = c(0, 20e3),
                        oob = squish) +
        scale_color_viridis_c(option = "A", labels = percent, limits = 0:1, begin = 0.3) +
        scale_x_continuous(labels = percent, limits = 0:1, expand = expansion(mult = 0),
                           breaks = seq(0, 0.8, 0.2)) +
        scale_y_continuous(limits = c(-40, 40), expand = expansion(mult = 0)) +
      #  geom_line(stat="smooth",method=gam, formula = y~s(x, bs = "cs"), color = "#222222",
      #            size = 0.65, alpha = 0.5) +
        geom_smooth(stat = "smooth",method = "gam", formula =y~s(x, bs = "cs"), 
                    color = "#222222",size = 0.55, alpha = 0.5,
                    se=TRUE, level=0.95,fill='lightblue',)+
        labs(x = "Democratic Vote", y = NULL, color = "Turnout", size = "Voters") +
        theme_ppmf()
}

plot_error_turnout_race = function(d, m) {
   ggplot(d, aes(1-white, fitted(m), size=voters, color=turnout)) +
        geom_hline(yintercept=0, lty="dashed") +
        geom_errorbar(aes(ymin=fitted(m)-1.96*se, ymax=fitted(m)+1.96*se), width=.1) +
        geom_point(alpha = 0.4) +
        scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3),
                        oob=squish) +
        scale_color_viridis_c(option="A", labels=percent, limits=0:1, begin = 0.3) +
        scale_x_continuous(labels=percent, limits = 0:1, expand=expansion(mult=0),
                           breaks = seq(0, 0.8, 0.2)) +
        scale_y_continuous(limits=c(-40, 40), expand=expansion(mult=0)) +
       # geom_line(stat="smooth",
       #           method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
       #           size = 0.65, alpha = 0.5) +
        geom_smooth(stat = "smooth",method = "gam", formula =y~s(x, bs = "cs"), 
                    color = "#222222",size = 0.55, alpha = 0.5,
                    se=TRUE, level=0.95,fill='lightblue',)+
        labs(x = "Percent Non-White", y=NULL, color="Turnout", size="Voters") +
        theme_ppmf()
}

plot_error_hh = function(d, m) {
    d %>%
        mutate(largest_race = recode_factor(max_race,
                                     white = "White",
                                     black = "Black",
                                     hisp = "Hispanic",
                                     .default = "Other"),
               #perturb_error = fitted(m)) %>% 
               error = fitted(m)) %>%
        sample_frac(1) %>%
    ggplot(aes(hh, error, size=voters, color = largest_race)) +
        geom_hline(yintercept=0, lty="dashed") +
        geom_errorbar(aes(ymin=fitted(m)-1.96*se, ymax=fitted(m)+1.96*se), width=.1) +
        geom_point(alpha = 0.25) +
        scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3),
                        oob=squish) +
        scale_color_manual(values = PAL_race) +
        scale_x_continuous(labels = percent, limits = c(0.2, 1),
                           breaks = seq(0.2, 0.8, 0.2)) + 
        scale_y_continuous(limits=c(-40, 40), expand=expansion(mult=0)) +
       # geom_line(stat="smooth",
       #           method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
       #           size = 0.65,alpha = 0.5) +
        geom_smooth(stat = "smooth",method = "gam", formula =y~s(x, bs = "cs"), 
                    color = "#222222",size = 0.55, alpha = 0.5,
                    se=TRUE, level=0.95,fill='lightblue',)+
        labs(x = "Herfindahl Index", y=NULL, color="Largest\nRacial Group", size="Voters") +
        theme_ppmf()
}

state_plots = map2(state_errors, state_models, plot_error_turnout) %>%
    map2(c("Pennsylvania", "North Carolina", "South Carolina", "Louisiana",
           'Alabama', 'Delaware', 'Utah', 'Washington'),
         ~ .x + ggtitle(.y))
state_plots$PA = state_plots$PA + labs(y = "Fitted DAS-12.2 population error")
state_plots$AL = state_plots$AL + labs(y = "Fitted DAS-12.2 population error")

wrap_plots(state_plots, design = design) +
    plot_layout(guides = "collect", )  &
    theme(plot.margin = margin(2, 0, 0, 0),
          legend.key.width = unit(20, "pt"),
          legend.key.height = unit(10, "pt"),
          legend.position = 'bottom')
ggsave(here("figs/partisan_error.pdf"), width = 7.2, height = 5.5)


# race
state_corrs = map_dbl(state_errors, ~ with(., cor(1-white, dem, method="spearman")))
state_plots = map2(state_errors, state_models, plot_error_turnout_race) %>%
    map2(c("Pennsylvania", "North Carolina", "South Carolina", "Louisiana",
           'Alabama', 'Delaware', 'Utah', 'Washington'),
         ~ .x + ggtitle(.y)) %>%
    map2(state_corrs, function(p, cor) {
        p + labs(subtitle=str_glue("Race-Party Corr.: {number(cor, 0.01)}"))
    })
state_plots$PA = state_plots$PA + labs(y = "Fitted DAS-12.2 population error")
state_plots$AL = state_plots$AL + labs(y = "Fitted DAS-12.2 population error")


wrap_plots(state_plots, design = design) +
    plot_layout(guides = "collect", )  &
    theme(plot.margin = margin(2, 0, 0, 0),
          legend.key.width = unit(20, "pt"),
          legend.key.height = unit(10, "pt"),
          legend.position = 'bottom')
ggsave(here("figs/race_error.pdf"), width=7.2, height=5.5)


# hh
state_plots = map2(state_errors, state_models, plot_error_hh) %>%
    map2(c("Pennsylvania", "North Carolina", "South Carolina", "Louisiana",
           'Alabama', 'Delaware', 'Utah', 'Washington'),
         ~ .x + ggtitle(.y))
state_plots$PA = state_plots$PA + labs(y = "Fitted DAS-12.2 population error")
state_plots$AL = state_plots$AL + labs(y = "Fitted DAS-12.2 population error")
state_plots$SC = state_plots$SC  + guides(color = FALSE)

wrap_plots(state_plots, design = design) +
    plot_layout(guides = "collect", )  &
    theme(plot.margin = margin(2, 0, 0, 0),
          legend.justification = 'right',
          legend.key.width = unit(20, "pt"),
          legend.key.height = unit(10, "pt"),
          legend.position = 'bottom')
ggsave(here("figs/herfindahl_error.pdf"), width=7.2, height=5.5)
