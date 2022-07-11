
# Goal for Task 06 --> gam(error ~ t2(turnout, dem, log(dens)) + s(white) + s(hh), data = d) --> use true diff as error
# Only make modifications on the following parts.
# Others remain unchanged!!!

# Plotting functions
plot_error_turnout_race = function(d, m) {
    ggplot(d, aes(1-white, error, size=voters, color=turnout)) +
        geom_hline(yintercept=0, lty="dashed") +
        geom_point(alpha = 0.4) +
        scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3),
                        oob=squish) +
        scale_color_viridis_c(option="A", labels=percent, limits=0:1, begin = 0.3) +
        scale_x_continuous(labels=percent, limits = 0:1, expand=expansion(mult=0),
                           breaks = seq(0, 0.8, 0.2)) +
        scale_y_continuous(limits=c(-40, 40), expand=expansion(mult=0)) +
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
                                     .default = "Other"),)%>%
        sample_frac(1) %>%
    ggplot(aes(hh, error, size=voters, color = largest_race)) +
        geom_hline(yintercept=0, lty="dashed") +
        geom_point(alpha = 0.25) +
        scale_size_area(max_size=1.0, labels=comma, limits=c(0, 20e3),
                        oob=squish) +
        scale_color_manual(values = PAL_race) +
        scale_x_continuous(labels = percent, limits = c(0.2, 1),
                           breaks = seq(0.2, 0.8, 0.2)) + # hard-code xlim
        scale_y_continuous(limits=c(-40, 40), expand=expansion(mult=0)) +
        geom_line(stat="smooth",
                  method = gam, formula = y~s(x, bs = "cs"), color = "#222222",
                  size = 0.65, se = FALSE, alpha = 0.5) +
        labs(x = "Herfindahl Index", y=NULL, color="Largest\nRacial Group", size="Voters") +
        theme_ppmf()
}


