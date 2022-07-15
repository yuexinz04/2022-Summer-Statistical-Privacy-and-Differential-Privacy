library(tidyverse)
library(scales)
library(redist)
library(patchwork)
library(sf)
library(mgcv)

# load helpers ----
source(".../dataverse_files/R/00_custom_functions.R")

# design ----
design <- "ABCD
           EFGH"

# Load data ---------------------------------------------------------------
sf::sf_use_s2(FALSE)
nc_error = read_rds(".../dataverse_files/data/NC/nc_shp.rds") %>%
    transmute(precinct = VTD,
              cd = cd_17,
              D = EL12G_GV_D,
              R = EL12G_GV_R,
              voters = EL12G_GV_TOT,
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
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)

sc_error = read_rds(".../dataverse_files/data/SC/sc_shp.rds") %>%
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
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)

vtd_das12 = read_rds(".../dataverse_files/data/PA/pa_vtd_das_12.rds")
vtd_das4 = read_rds(".../dataverse_files/data/PA/pa_vtd_das_04.rds") %>%
    rename_with(~ str_c(., "_das4"), -precinct)
vtd_orig = read_rds(".../dataverse_files/data/PA/pa_vtd_orig.rds")
pa_shp = read_rds(".../dataverse_files/data/PA/pa_shp.rds") %>%
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
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)

la_error = read_rds(".../dataverse_files/data/LA/la.rds") %>%
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
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)
la_error$dem[930] <- 0

al_error <- read_rds(".../dataverse_files/data/AL/al.rds") %>%
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
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)

de_error <- read_rds(".../dataverse_files/data/DE/de.rds") %>%
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
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)


wa_error <- read_rds(".../dataverse_files/data/WA/wa.rds") %>%
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
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)


ut_error <- read_rds(".../dataverse_files/data/UT/ut.rds") %>%
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
    as_tibble() %>%
    filter(voters > 0, turnout <= 1)


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
}
state_models = map(state_errors, fit_model)

# Rsq
err_df <- tibble(error = nc_error$error, fitted = state_models$NC$fitted.values)
1 - sum((err_df$fitted - err_df$error)^2) / sum((err_df$error - mean(err_df$error))^2)



# Plotting functions
plot_error_turnout = function(d, m) {
    ggplot(d, aes(dem, fitted(m), size = voters, color = turnout)) +
        geom_hline(yintercept = 0, lty = "dashed") +
        geom_point(alpha = 0.4) +
        scale_size_area(max_size = 1.0, labels = comma, limits = c(0, 20e3),
                        oob = squish) +
        scale_color_viridis_c(option = "A", labels = percent, limits = 0:1, begin = 0.3) +
        scale_x_continuous(labels = percent, limits = 0:1, expand = expansion(mult = 0),
                           breaks = seq(0, 0.8, 0.2)) +
        scale_y_continuous(limits = c(-120, 120), expand = expansion(mult = 0)) +
        geom_line(stat="smooth",
                  method=gam, formula = y~s(x, bs = "cs"), color = "#222222",
                  size = 0.65, se = FALSE, alpha = 0.5) +
        labs(x = "Democratic Vote", y = NULL, color = "Turnout", size = "Voters") +
        theme_ppmf()
}
plot_error_turnout_race = function(d, m) {
    ggplot(d, aes(1-white, error, size=voters, color=turnout)) +
        geom_hline(yintercept=0, lty="dashed") +
        geom_point(alpha = 0.4) +
        scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3),
                        oob=squish) +
        scale_color_viridis_c(option="A", labels=percent, limits=0:1, begin = 0.3) +
        scale_x_continuous(labels=percent, limits = 0:1, expand=expansion(mult=0),
                           breaks = seq(0, 0.8, 0.2)) +
        scale_y_continuous(limits=c(-120, 120), expand=expansion(mult=0)) +
        geom_line(stat="smooth",
                  method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
                  size = 0.65, se = FALSE, alpha = 0.5) +
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
               error = error) %>%
        sample_frac(1) %>%
    ggplot(aes(hh, error, size=voters, color = largest_race)) +
        geom_hline(yintercept=0, lty="dashed") +
        geom_point(alpha = 0.25) +
        scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3),
                        oob=squish) +
        scale_color_manual(values = PAL_race) +
        scale_x_continuous(labels = percent, limits = c(0.2, 1),
                           breaks = seq(0.2, 0.8, 0.2)) + # hard-code xlim
        scale_y_continuous(limits=c(-120, 120), expand=expansion(mult=0)) +
        geom_line(stat="smooth",
                  method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
                  size = 0.65, se = FALSE, alpha = 0.5) +
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
ggsave(".../dataverse_files/new_figs/partisan_error.pdf", width = 7.2, height = 5.5)


# race
state_corrs = map_dbl(state_errors, ~ with(., cor(1-white, dem, method="spearman")))
state_plots = map2(state_errors, state_models, plot_error_turnout_race) %>%
    map2(c("Pennsylvania", "North Carolina", "South Carolina", "Louisiana",
           'Alabama', 'Delaware', 'Utah', 'Washington'),
         ~ .x + ggtitle(.y)) %>%
    map2(state_corrs, function(p, cor) {
        p + labs(subtitle=str_glue("Race-Party Corr.: {number(cor, 0.01)}"))
    })
state_plots$PA = state_plots$PA + labs(y="Fitted DAS-12.2 population error")
state_plots$AL = state_plots$AL + labs(y = "Fitted DAS-12.2 population error")


wrap_plots(state_plots, design = design) +
    plot_layout(guides = "collect", )  &
    theme(plot.margin = margin(2, 0, 0, 0),
          legend.key.width = unit(20, "pt"),
          legend.key.height = unit(10, "pt"),
          legend.position = 'bottom')
ggsave(".../dataverse_files/new_figs/race_error.pdf", width=7.2, height=5.5)


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
ggsave(".../dataverse_files/new_figs/herfindahl_error.pdf", width=7.2, height=5.5)

