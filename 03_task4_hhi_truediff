
# Goal for Task 04 --> gam (error ~ s(hh)) --> use true diff as error --> produce hh plot only
# Only make modifications on the following parts.
# Others remain unchanged!!!

# Fit models and make plots -----------------------------------------------

fit_model = function(d) {
    gam (error ~ s(hh),data=d)
    }
    

# Plotting functions
plot_error_hh = function(d, m) {
    d %>%
        mutate(largest_race = recode_factor(max_race,
                                     white = "White",
                                     black = "Black",
                                     hisp = "Hispanic",
                                     .default = "Other"),) %>%
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


