if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               ggplot2,
               ggtext,
               ggpubr,
               install = TRUE,
               update = FALSE)

extrafont::loadfonts("all", quiet = TRUE)

source(file.path(here(), "2_code", "2_metareg.R"))
source(file.path(here(), "2_code", "2_metareg-ext.R"))
source(file.path(here(), "2_code", "4_heterogeneity.R"))

blue <- "#1E64C8"

main.dur <- mreg.dur.nc.emm %>% as_tibble()
main.ctl <- mreg.emm %>% as_tibble()
main.dual <- mreg.out.adj.emm %>% as_tibble()

l6m.ctl <- mreg.0m6m.emm %>% as_tibble()
l6m.dual <- mreg.0m6m.out.adj.emm %>% as_tibble()

l12m.ctl <- mreg.6m12m.emm %>% as_tibble()
l12m.dual <- mreg.6m12m.out.adj.emm %>% as_tibble()

l18m.ctl <- mreg.12m18m.emm %>% as_tibble()
l18m.dual <- mreg.12m18m.out.adj.emm %>% as_tibble()

l36m.ctl <- mreg.18m36m.emm %>% as_tibble()
l36m.dual <- mreg.18m36m.out.adj.emm %>% as_tibble()

data.visual.averages <-
  tribble(
    ~model_id, ~te, ~te_lb, ~te_ub, ~duration, ~model,
    
    "main", exp(ma$TE.random)-1, exp(ma$lower.random)-1,
      exp(ma$upper.random)-1, "1 to 36 months",
      "Unadjusted",
    #"main.dur", exp(main.dur$emmean)-1, exp(main.dur$lower.CL)-1,
      #exp(main.dur$upper.CL)-1, "1 to 36 months",
      #"Duration-adjusted",
    "main.ctl", exp(main.ctl$emmean)-1, exp(main.ctl$lower.CL)-1,
      exp(main.ctl$upper.CL)-1, "1 to 36 months",
      "Covariate-adjusted",
    "main.out", exp(ma.out.adj$TE.random)-1, exp(ma.out.adj$lower.random)-1,
      exp(ma.out.adj$upper.random)-1, "1 to 36 months",
      "Outlier-adjusted",
    "main.dual", exp(main.dual$emmean)-1, exp(main.dual$lower.CL)-1,
      exp(main.dual$upper.CL)-1, "1 to 36 months",
      "Dual-adjusted",
    
    "l6m", exp(ma.0m6m$TE.random)-1, exp(ma.0m6m$lower.random)-1,
      exp(ma.0m6m$upper.random)-1, "1 to 6 months",
      "Unadjusted",
    "l6m.ctl", exp(l6m.ctl$emmean)-1, exp(l6m.ctl$lower.CL)-1,
      exp(l6m.ctl$upper.CL)-1, "1 to 6 months",
      "Covariate-adjusted",
    "l6m.out", exp(ma.0m6m.out.adj$TE.random)-1, exp(ma.0m6m.out.adj$lower.random)-1,
      exp(ma.0m6m.out.adj$upper.random)-1, "1 to 6 months",
      "Outlier-adjusted",
    "l6m.dual", exp(l6m.dual$emmean)-1, exp(l6m.dual$lower.CL)-1,
      exp(l6m.dual$upper.CL)-1, "1 to 6 months",
      "Dual-adjusted",
    
    "l12m", exp(ma.6m12m$TE.random)-1, exp(ma.6m12m$lower.random)-1,
      exp(ma.6m12m$upper.random)-1, "7 to 12 months",
      "Unadjusted",
    "l12m.ctl", exp(l12m.ctl$emmean)-1, exp(l12m.ctl$lower.CL)-1,
      exp(l12m.ctl$upper.CL)-1, "7 to 12 months",
      "Covariate-adjusted",
    "l12m.out", exp(ma.6m12m.out.adj$TE.random)-1, exp(ma.6m12m.out.adj$lower.random)-1,
      exp(ma.6m12m.out.adj$upper.random)-1, "7 to 12 months",
      "Outlier-adjusted",
    "l12m.dual", exp(l12m.dual$emmean)-1, exp(l12m.dual$lower.CL)-1,
      exp(l12m.dual$upper.CL)-1, "7 to 12 months",
      "Dual-adjusted",
    
    "l18m", exp(ma.12m18m$TE.random)-1, exp(ma.12m18m$lower.random)-1,
      exp(ma.12m18m$upper.random)-1, "13 to 18 months",
      "Unadjusted",
    "l18m.ctl", exp(l18m.ctl$emmean)-1, exp(l18m.ctl$lower.CL)-1,
      exp(l18m.ctl$upper.CL)-1, "13 to 18 months",
      "Covariate-adjusted",
    #"l18m.out", exp(ma.12m18m.out.adj$TE.random)-1, exp(ma.12m18m.out.adj$lower.random)-1,
      #exp(ma.12m18m.out.adj$upper.random)-1, "13 to 18 months",
      #"Outlier-adjusted",
    #"l18m.dual", exp(l18m.dual$emmean)-1, exp(l18m.dual$lower.CL)-1,
      #exp(l18m.dual$upper.CL)-1, "13 to 18 months",
      #"Dual-adjusted",
    
    "l36m", exp(ma.18m36m$TE.random)-1, exp(ma.18m36m$lower.random)-1,
      exp(ma.18m36m$upper.random)-1, "19 to 36 months",
      "Unadjusted",
    "l36m.ctl", exp(l36m.ctl$emmean)-1, exp(l36m.ctl$lower.CL)-1,
      exp(l36m.ctl$upper.CL)-1, "19 to 36 months",
      "Covariate-adjusted",
    "l36m.out", exp(ma.18m36m.out.adj$TE.random)-1, exp(ma.18m36m.out.adj$lower.random)-1,
      exp(ma.18m36m.out.adj$upper.random)-1, "19 to 36 months",
      "Outlier-adjusted",
    "l36m.dual", exp(l36m.dual$emmean)-1, exp(l36m.dual$lower.CL)-1,
      exp(l36m.dual$upper.CL)-1, "19 to 36 months",
      "Dual-adjusted"
  ) %>%
  relocate(model, .after = model_id) %>%
  mutate(duration =
           factor(duration,
                  levels =
                    c("1 to 36 months",
                      "1 to 6 months",
                      "7 to 12 months",
                      "13 to 18 months",
                      "19 to 36 months")),
         model =
           factor(model,
                  levels =
                    c("Unadjusted",
                      #"Duration-adjusted",
                      "Covariate-adjusted",
                      "Outlier-adjusted",
                      "Dual-adjusted")),
         sig = case_when(sign(te_lb) == sign(te_ub) ~ "Yes",
                         TRUE ~ "No"))

data.visual.averages

title <- paste0("Aggregated unemployed–employed differences ",
                "in positive callbacks")
xtitle <- "Difference in positive callbacks"
ytitle <- "Unemployment duration"

p1 <- ggplot(
  data = data.visual.averages %>%
    filter(str_detect(model_id, "main")),
  mapping = aes(x = te, y = fct_rev(model),
                colour = sig)) +
  geom_vline(mapping = aes(xintercept = 0),
             colour = "grey65",
             linetype = "dashed",
             linewidth = .25) +
  geom_errorbar(mapping = aes(xmin = te_lb, xmax = te_ub),
                linewidth = .5,
                width = .25) +
  #geom_text(data = data.visual.averages %>%
  #filter(duration %in% c("1 to 36 months", "1 to 6 months")),
  #mapping = aes(label = model, x = te_ub + .02),
  #family = "UGent Panno Text",
  #colour = "grey40",
  #size = 2,
  #hjust = 0) +
  geom_point(shape = 18,
             size = 2) +
  scale_x_continuous(limits = c(-.45, .30),
                     breaks = seq(-1, 1, .1),
                     labels = scales::label_percent(
                       style_positive = "plus",
                       style_negative = "minus")) +
  scale_colour_manual(values = c("grey60", blue)) +
  facet_grid(duration~.,
             #scales = "free_y",
             space = "free_y",
             switch = "y") +
  labs(
    #title = title,
    x = "",
    y = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "UGent Panno Text"),
    plot.title = element_text(hjust = .5),
    plot.title.position = "plot",
    panel.grid.major = element_line(linewidth = .25,
                                    colour = "gray90"),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    axis.title.x = element_text(margin = margin(t = -5, r = 0, b = 0, l =0),
                                colour = "gray20"),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l =0),
                                colour = "gray20"),
    axis.text.y = element_text(size =7),
    strip.text.y.left = element_blank(),
    strip.placement = "outside",
    panel.spacing = unit(10, "points"),
    legend.position = "none"
  )

p2 <- ggplot(
  data = data.visual.averages %>%
    filter(!str_detect(model_id, "main")),
  mapping = aes(x = te, y = fct_rev(model),
                colour = sig)) +
  geom_vline(mapping = aes(xintercept = 0),
             colour = "grey65",
             linetype = "dashed",
             linewidth = .25) +
  geom_errorbar(mapping = aes(xmin = te_lb, xmax = te_ub),
                linewidth = .5,
                width = .25) +
  geom_point(shape = 18,
             size = 2) +
  scale_x_continuous(limits = c(-.45, .30),
                     breaks = seq(-1, 1, .1),
                     labels = scales::label_percent(
                       style_positive = "plus",
                       style_negative = "minus")) +
  scale_colour_manual(values = c("grey60", blue)) +
  facet_grid(duration~.,
             scales = "free_y",
             space = "free_y",
             switch = "y") +
  labs(
    #title = title,
    x = xtitle,
    y = ""
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "UGent Panno Text"),
    plot.title = element_text(hjust = .5),
    plot.title.position = "plot",
    panel.grid.major = element_line(linewidth = .25,
                                    colour = "gray90"),
    #panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l =0),
                                colour = "gray20"),
    axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l =0),
                                colour = "gray20"),
    axis.text.y = element_text(size =7),
    strip.text.y.left = element_text(angle = 0,
                                     margin = margin(l = 0),
                                     hjust = 0.5),
    strip.placement = "outside",
    panel.spacing = unit(10, "points"),
    legend.position = "none"
  )


p.arranged <-
  ggarrange(
  p1 +
    theme(plot.margin = margin(t = 11, r = 11*1.5, b = 11, l = 0)),
  p2 +
    theme(plot.margin = margin(t = 11, r = 11, b = 11, l = 11*1.5)),
  align = "v",
  ncol = 1,
  nrow = 2,
  heights = c(1.2, 4),
  labels = c("A. OVERALL", "B. BY UNEMPLOYMENT DURATION"),
  hjust = 0,
  vjust = 1.3,
  font.label = list(size = 8,
                    color = "gray20",
                    face = "bold",
                    family = "helvetica")
)

print(p.arranged)

for(d in c("png", "tiff")){
  ggsave(
    filename = paste0("mreg-averages.", d),
    path = file.path(here(), "3_figures"),
    device = d,
    width = 12,
    height = 12,
    units = "cm",
    dpi = 1000,
    bg = "white"
  )
}
