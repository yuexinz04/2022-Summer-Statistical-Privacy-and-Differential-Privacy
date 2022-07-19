
# Goal for Task 05 --> gam (error ~ s(white)) --> use true diff as error -> produce non white plot only
# Only make modifications on the following parts.
# Others remain unchanged!!!

# Fit models and make plots -----------------------------------------------

fit_model = function(d) {
    gam(error ~ s(white), data = d)
    }
    
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


