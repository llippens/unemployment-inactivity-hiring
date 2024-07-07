if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               tidyr,
               ggplot2, ggtext,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "4_pubbias.R"))

pcurve.plot.data <-
  pcurve.plot.data %>%
  pivot_longer(cols = c("observed_blue", "power_33_percent_green", "flat_red"), 
               names_to = "curve",
               values_to = "value") %>%
  mutate(curve = case_when(curve == "observed_blue" ~ "observed",
                           curve == "power_33_percent_green" ~ "power33",
                           curve == "flat_red" ~ "flat"),
         curve = factor(curve,
                        levels = c("observed", "power33", "flat")),
         model = factor(model,
                        levels = c("data", "data.0m6m", "data.6m12m",
                                   "data.12m18m", "data.18m36m")))

xtitle <- "p-value"
ytitle <- "Proportion"
xlabels <- c("<0.01", "<0.02", "<0.03", "<0.04", "<0.05")
striplabels <-
  as_labeller(c(data = "Overall",
                data.0m6m = "1 to 6 months",
                data.6m12m = "7 to 12 months",
                data.12m18m = "13 to 18 months",
                data.18m36m = "19 to 36 months"))

pcurve.plot.data %>%
  ggplot(mapping = aes(x = p_value, y = value,
                       colour = curve, fill = curve,
                       linetype = curve)) +
  geom_bar(data = pcurve.plot.data %>%
             filter(curve == "observed"),
           stat = "identity",
           colour = NA,
           fill = "gray80",
           width = .005) +
  geom_line(mapping = aes(size = curve)) +
  scale_x_continuous(labels = xlabels,
                     breaks = seq(.01, .05, .01)) +
  scale_y_continuous(labels = scales::label_percent(scale = 1, accuracy = 1)) +
  scale_colour_manual(values = c("gray20", "gray50", "gray50")) +
  scale_linetype_manual(values = c("solid", "dashed", "dotted")) +
  scale_size_manual(values = c(.75, .5, .5)) +
  facet_wrap(facets = "model",
             ncol = 3,
             labeller = striplabels) +
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
    strip.background = element_rect(fill = "gray90", colour = NA),
    strip.text = element_text(colour = "gray20"),
    legend.position = "none"
  )

for(d in c("png", "tiff")){
  ggsave(
    filename = paste0("pcurve.", d),
    path = file.path(here(), "3_figures", "appendix"),
    device = d,
    width = 15,
    height = 10,
    units = "cm",
    dpi = 1000,
    bg = "white"
  )
}
