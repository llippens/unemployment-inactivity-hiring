if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               ggplot2,
               ggtext,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "2_metareg.R"))
source(file.path(here(), "2_code", "2_prediction_ud.R"))

blue <- "#1E64C8"

data.visual.duration <-
  cbind(x = data$unemployment_duration_months,
        x_alt = data$years_average,
        y = ma$TE,
        y_exp = exp(ma$TE)-1,
        y_pred = mreg.pred$pred,
        w = ma$w.random,
        w_alt = 1/mreg$vi) %>%
  as_tibble()

ribbon.loess <- loess(y_pred ~ x, data = data.visual.duration,
                      weights = w, span = .8)
ribbon.pred <- predict(ribbon.loess, se = TRUE)

ribbon <-
  cbind(fit = ribbon.pred$fit,
        ci_low = ribbon.pred$fit - qnorm(0.975) * ribbon.pred$se.fit,
        ci_high = ribbon.pred$fit + qnorm(0.975) * ribbon.pred$se.fit,
        x = data$unemployment_duration_months) %>%
  as_tibble()

xtitle <- "Unemployment duration<br>(in months)"
ytitle <- "Difference in positive callbacks"

# Points are original estimates based on underlying studies
# Smoothed, weighted LOESS curve is based on predicted values, fully controlled

ggplot(
  data = data.visual.duration,
  mapping = aes(x = x, y = y_exp)) +
  geom_hline(yintercept = 0,
             linewidth = .25,
             linetype = "dashed",
             colour = "gray65") +
  geom_point(mapping = aes(size = w),
             colour = blue,
             alpha = .5) +
  geom_smooth(mapping = aes(y = y_pred-1, weight = w),
              colour = "grey20",
              fill = "grey20",
              method = "loess",
              formula = "y ~ x",
              linewidth = 1,
              alpha = 0.15,
              span = .8,
              level = 0.95) +
  geom_ribbon(data = ribbon,
              mapping = aes(y = fit-1,
                            ymin = ci_low-1, ymax = ci_high-1),
              fill = NA,
              colour = "grey20",
              linewidth = .1) +
  scale_y_continuous(limits = c(-1, 1.245),
                     breaks = seq(-1, 1.25, .25),
                     labels = scales::label_percent(
                       style_positive = "plus",
                       style_negative = "minus")) +
  scale_x_continuous(limits = c(0, 36),
                     breaks = seq(0, 36, 3)) +
  labs(
    #title = "Employed–Unemployed Callback Difference by Unemployment Duration",
    x = xtitle,
    y = ytitle
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "UGent Panno Text"),
    axis.title.x = element_markdown(family = "UGent Panno Text",
                                    hjust = .5,
                                    margin = margin(t = 8, r = 0, b = 0, l =0),
                                    colour = "gray20"),
    axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l =0),
                                colour = "gray20"),
    panel.grid.major = element_line(linewidth = .25,
                                    colour = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )

for(d in c("png", "tiff")){
  ggsave(
    filename = paste0("mreg-duration-loess.", d),
    path = file.path(here(), "3_figures"),
    device = d,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 1000,
    bg = "white"
  )
}



# Line and ribbon are based on (weighted) estimated marginal means
# Categorical predictors are fixed at their reference levels
# Continuous predictors are fixed at their average values

ggplot(
  data = data.visual.duration,
  mapping = aes(x = x, y = y_exp)) +
  geom_ribbon(data = mreg.emm.pred,
              mapping = aes(x = x, y = pred-1,
                            ymin = ci.lb-1, ymax = ci.ub-1),
              fill = "grey40",
              alpha = .15) +
  geom_smooth(mapping = aes(y = y_pred-1, weight = w),
              level = 0.99,
              colour = blue,
              fill = blue,
              alpha = .15,
              method = "loess",
              formula = "y ~ x",
              se = TRUE,
              linewidth = 1) +
  geom_ribbon(data = ribbon,
              mapping = aes(y = fit-1,
                            ymin = ci_low-1, ymax = ci_high-1),
              fill = NA,
              colour = blue,
              linewidth = .25,
              linetype = "dashed") +
  geom_point(mapping = aes(size = w),
             colour = blue,
             alpha = .5) +
  geom_line(data = mreg.emm.pred,
            mapping = aes(x = x, y = pred-1),
            colour = "grey40",
            linewidth = 1,
            linetype = "twodash") +
  scale_y_continuous(limits = c(-1, 1.245),
                     breaks = seq(-1, 1.25, .25),
                     labels = scales::label_percent(
                       style_positive = "plus",
                       style_negative = "minus")) +
  scale_x_continuous(limits = c(0, 36),
                     breaks = seq(0, 36, 3)) +
  labs(
    #title = "Employed–Unemployed Callback Difference by Unemployment Duration",
    x = xtitle,
    y = ytitle
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "UGent Panno Text"),
    axis.title.x = element_markdown(family = "UGent Panno Text",
                                    hjust = .5,
                                    margin = margin(t = 8, r = 0, b = 0, l =0),
                                    colour = "gray20"),
    axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l =0),
                                colour = "gray20"),
    panel.grid.major = element_line(linewidth = .25,
                                    colour = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "none"
  )
