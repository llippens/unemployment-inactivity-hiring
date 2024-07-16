if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               meta,
               ggplot2, ggrepel,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "2_metareg-ext.R"))

# Set parameters
family <- "UGent Panno Text"
extrafont::loadfonts(device = "all", quiet = TRUE)
blue <- "#1E64C8"

# Predefine constants
z90 <- qnorm(.95)
z95 <- qnorm(.975)
z95dot <- round(z95, 1)
z99 <- qnorm(.995)
z99dot <- round(z99, 1)

# List of meta-analysis objects
ma.subs <- list(
  ma = ma,
  ma.0m6m = ma.0m6m,
  ma.6m12m = ma.6m12m,
  ma.12m18m = ma.12m18m,
  ma.18m36m = ma.18m36m
)

# Iterate over each meta-analysis object in the list
for (ma_name in names(ma.subs)) {
  ma <- ma.subs[[ma_name]]
  
  se <- ma$seTE %>% max() %>% round(digits = 1)
  p99l <- min(exp(ma$TE - se * z99), exp(0 - se * z99dot), min(exp(ma$TE)))
  p99r <- max(exp(ma$TE + se * z99), exp(0 + se * z99dot), max(exp(ma$TE)))
  ylim <- max(se, max(ma$seTE)+.01)
  
  triangle90p <- tibble(group = c(1,1,1),
                        polygon.x = c(exp(0 + z90*se), exp(0 - z90*se), exp(0)),
                        polygon.y = c(ylim, ylim, 0))
  triangle95p <- tibble(group = c(1,1,1),
                        polygon.x = c(exp(0 + z95*se), exp(0 - z95*se), exp(0)),
                        polygon.y = c(ylim, ylim, 0))
  triangle99p <- tibble(group = c(1,1,1),
                        polygon.x = c(exp(0 + z99*se), exp(0 - z99*se), exp(0)),
                        polygon.y = c(ylim, ylim, 0))
  
  line95l <- tibble(x = c(exp(ma$TE.random), exp(ma$TE.random - z95*se)),
                    y = c(0, ylim))
  line95r <- tibble(x = c(exp(ma$TE.random), exp(ma$TE.random + z95*se)),
                    y = c(0, ylim))
  line99l <- tibble(x = c(exp(ma$TE.random), exp(ma$TE.random - z99*se)),
                    y = c(0, ylim))
  line99r <- tibble(x = c(exp(ma$TE.random), exp(ma$TE.random + z99*se)),
                    y = c(0, ylim))
  linese <- tibble(x = c(exp(ma$TE.random), exp(ma$TE.random)),
                   y = c(0, ylim))
  
  biaspoints <- tibble(te = ma$TE %>% exp(),
                       sete = ma$seTE,
                       w = ma$w.random,
                       ter = exp(ma$TE)) %>%
    mutate(setel = exp(ma$TE.random - z95*sete),
           pl = exp(0 - z95*sete),
           seter = exp(ma$TE.random + z95*sete),
           pr = exp(0 + z95*sete)) %>%
    mutate(minl = case_when(setel < pl ~ setel,
                            setel >= pl ~ pl),
           maxr = case_when(seter < pr ~ pr,
                            seter >= pr ~ seter)) %>%
    mutate(label = case_when(te < minl ~ ma$data$uid,
                             te > maxr ~ ma$data$uid,
                             te >= minl & te <= maxr ~ "NA"))
  
  biaspoints[biaspoints == "NA"] <- NA
  
  pubbiasplot <- ggplot() +
    geom_polygon(data = triangle99p,
                 mapping = aes(x = polygon.x, y = polygon.y, group=group),
                 fill = "gray70") +
    geom_polygon(data = triangle95p,
                 mapping = aes(x = polygon.x, y = polygon.y, group=group),
                 fill = "gray85") +
    geom_polygon(data = triangle90p,
                 mapping = aes(x = polygon.x, y = polygon.y, group=group),
                 fill = "white") +
    geom_point(data = biaspoints,
               mapping = aes(x = te, y = sete, size = w),
               alpha = .5, fill = "gray20", colour = "gray20") +
    scale_size_continuous(range = c(.75, 3)) +
    geom_line(data = line95l,
              mapping = aes(x = x, y = y),
              colour = "gray20", linetype = "dotted") +
    geom_line(data = line95r,
              mapping = aes(x = x, y = y),
              colour = "gray20", linetype = "dotted") +
    geom_line(data = linese,
              mapping = aes(x = x, y = y),
              colour = "gray20", linetype = "dotted") +
    geom_line(data = line99l,
              mapping = aes(x = x, y = y),
              colour = "gray20", linetype = "dashed") +
    geom_line(data = line99r,
              mapping = aes(x = x, y = y),
              colour = "gray20", linetype = "dashed") +
    scale_y_reverse(breaks = seq(0, ylim, ylim/8),
                    labels = scales::comma_format(accuracy = .01)) +
    scale_x_log10(limits = c(min(floor(triangle99p$polygon.x[[2]]*10)/10,
                                 line99l$x[[2]],
                                 biaspoints$te),
                             max(round_half_up(triangle99p$polygon.x[[1]]*10)/10,
                                 line99r$x[[2]],
                                 biaspoints$te)),
                  breaks = c(biaspoints$te %>% min(),
                             line99l$x[[2]],
                             line95l$x[[2]],
                             exp(ma$TE.random),
                             line95r$x[[2]],
                             line99r$x[[2]]),
                  labels = scales::comma_format(accuracy = .01)) +
    geom_text_repel(data = biaspoints %>%
                      filter(!is.na(label)),
                    mapping = aes(x = te, y = sete, label = label),
                    size = 2,
                    max.overlaps = 20,
                    min.segment.length = 2,
                    colour = "gray20") +
    theme_minimal() +
    theme(title = element_text(family = family),
          text = element_text(family = family),
          panel.background = element_rect(fill = "gray95",
                                          colour = NA),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          #axis.ticks = element_line(colour = "gray90", linewidth = .3),
          axis.title.y = element_text(margin = margin(t = 0, r = 8, b = 0, l = 0),
                                      angle = 90,
                                      hjust = .5,
                                      colour = "gray20"),
          axis.title.x = element_text(margin = margin(t = 8, r = 0, b = 0, l =0),
                                      colour = "gray20"),
          plot.title.position = "plot",
          panel.spacing = unit(10, "points"),
          legend.position = "none") +
    labs(x = "Callback ratio", y = "Standard error")
  
  # Save the plot
  for(d in c("png", "tiff")){
    ggsave(
      filename = paste0("pubbias-", ma_name, ".", d),
      path = file.path(here(), "3_figures", "appendix", "funnel"),
      plot = pubbiasplot,
      device = d,
      width = 15,
      height = 12,
      units = "cm",
      dpi = 1000,
      bg = "white"
    )
  }
  
  # Print the plot
  print(pubbiasplot)
}