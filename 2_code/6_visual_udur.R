if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               ggplot2,
               colorspace,
               ggtext,
               ggnewscale,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "2_metareg.R"))
source(file.path(here(), "2_code", "3_prediction_udur.R"))

data.visual.regplot <-
  regplot(mreg %>%
            robust(cluster = cluster, adjust = FALSE, clubSandwich = FALSE),
          mod = "unemployment_duration_months",
          transf = exp)

data.visual.udur <-
  tibble(author = data$authors,
         x1 = data$unemployment_duration_months,
         x2 = data$unemployment_rate,
         ur_cat = data$unemployment_rate_categorical6,
         y_exp = exp(ma$TE)-1,
         y_exp_alt = mreg.ur.pred.gen$pred,
         w = ma$w.random,
         w_alt = data.visual.regplot$psize)

mreg.ur.pred <-
  mreg.ur.low.pred %>%
  rbind(mreg.ur.high.pred) %>%
  mutate(ur_cat = factor(ur_cat) %>% fct_rev())

ribbon.lm <- list()
ribbon.pred <- list()
ribbon <- list()
for(urc in levels(data.visual.udur$ur_cat)){
  ribbon.lm[[urc]] <- lm(y_exp_alt ~ x1,
                         data = data.visual.udur %>% filter(ur_cat == urc),
                         weights = w)
  ribbon.pred[[urc]] <- predict(ribbon.lm[[urc]], se = TRUE)
  
  ribbon[[urc]] <-
    tibble(fit = ribbon.pred[[urc]]$fit,
           ci_low = ribbon.pred[[urc]]$fit - qnorm(0.975) * ribbon.pred[[urc]]$se.fit,
           ci_high = ribbon.pred[[urc]]$fit + qnorm(0.975) * ribbon.pred[[urc]]$se.fit,
           x1 = data %>%
             filter(unemployment_rate_categorical6 == urc) %>%
             select(unemployment_duration_months) %>%
             pull(),
           ur_cat = urc)
}
ribbon <- do.call(rbind, ribbon)

blue <- "#1E64C8"
orange <- "#C78D1E"
lblue <- lighten(blue, .35)
dblue <- darken(blue, .35)
lorange <- lighten(orange, .15)
dorange <- darken(orange, .15)

xtitle <- "Unemployment duration<br>(in months)"
ytitle <- "Difference in positive callbacks"

# Points are original estimates based on underlying studies
# Regression curve based on meta-regression specification, fully controlled

data.visual.udur %>%
  ggplot(mapping = aes(colour = ur_cat, fill = ur_cat)) +
  geom_hline(yintercept = 0,
             linewidth = .25,
             linetype = "dashed",
             colour = "gray65") +
  geom_point(mapping = aes(x = x1, y = y_exp, size = w),
             alpha = .5) +
  geom_ribbon(data = mreg.ur.pred,
              mapping = aes(x = ud, y = pred-1,
                            ymin = ci.lb-1, ymax = ci.ub-1),
              alpha = .15,
              linewidth = .15) +
  geom_line(data = mreg.ur.pred,
            mapping = aes(x = ud, y = pred-1),
            linewidth = 1) +
  scale_colour_manual(values = c(lblue, dblue),
                      name = "Unemployment rate:",
                      guide = guide_legend(reverse = FALSE)) +
  scale_fill_manual(values = c(lblue, dblue),
                    name = "Unemployment rate:",
                    guide = guide_legend(reverse = FALSE)) +
  guides(size = "none") +
  #new_scale_colour() +
  geom_smooth(mapping = aes(x = x1, y = y_exp_alt-1,
                            weight = w, colour = ur_cat),
              method = "lm",
              formula = "y ~ x",
              linetype = "longdash",
              fill = NA,
              linewidth = .5,
              alpha = 0.2,
              level = 0.95,
              span = .85,
              show.legend = FALSE) +
  #scale_colour_manual(values = c(lorange, dorange)) +
  scale_y_continuous(limits = c(-1, 1.245),
                     breaks = seq(-1, 1.25, .25),
                     labels = scales::label_percent(
                       style_positive = "plus",
                       style_negative = "minus")) +
  scale_x_continuous(limits = c(0, 36),
                     breaks = seq(0, 36, 3)) +
  labs(
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
    legend.text = element_text(size = 8,
                               colour = "gray20"),
    legend.title = element_text(size = 8,
                                colour = "gray20"),
    legend.position = "top"
  )

for(d in c("png", "tiff")){
  ggsave(
    filename = paste0("mreg-udur", ".", d),
    path = file.path(here(), "3_figures"),
    device = d,
    width = 15,
    height = 11.5,
    units = "cm",
    dpi = 1000,
    bg = "white"
  )
}
