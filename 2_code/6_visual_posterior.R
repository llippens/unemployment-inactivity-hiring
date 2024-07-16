if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               dplyr, purrr,
               ggplot2,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "4_bayesian.R"))

process_list <- function(list_name, list_data) {
  list_data$data %>%
    mutate(list_name = list_name)
}

combined_data <-
  map2_df(names(ppc), ppc, process_list) %>%
  mutate(
    list_name = case_when(list_name == "data" ~ "Overall",
                          list_name == "data.out.adj" ~ "Overall (out.-adj.)",
                          list_name == "data.0m6m" ~ "1 to 6 months",
                          list_name == "data.6m12m" ~ "7 to 12 months",
                          list_name == "data.12m18m" ~ "13 to 18 months",
                          list_name == "data.18m36m" ~ "19 to 36 months",
                          list_name == "data.0m6m.out.adj" ~ "1 to 6 months (out.-adj.)",
                          list_name == "data.6m12m.out.adj" ~ "7 to 12 months (out.-adj.)",
                          list_name == "data.18m36m.out.adj" ~ "19 to 36 months (out.-adj.)",
                          TRUE ~ "Other") %>%
      factor(levels = c(
        "Overall", 
        "Overall (out.-adj.)",
        "1 to 6 months", 
        "1 to 6 months (out.-adj.)", 
        "7 to 12 months", 
        "7 to 12 months (out.-adj.)", 
        "13 to 18 months", 
        "19 to 36 months", 
        "19 to 36 months (out.-adj.)"))
  )

xtitle <- "Difference in positive callbacks"
ytitle <- "Density"

combined_data %>%
  ggplot(mapping = aes(x = exp(value)-1, group = rep_id)) +
  geom_density(mapping = aes(colour = is_y, size = is_y),
               fill = NA) +
  scale_colour_manual(values = c("gray70", "gray20")) +
  scale_size_manual(values = c(.35, .75)) +
  scale_x_continuous(
    limits = c(-1, 2)
  ) +
  facet_wrap(facets = "list_name",
             ncol = 3) +
  labs(
    x = xtitle,
    y = ytitle
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "UGent Panno Text"),
    axis.title.x = element_text(hjust = .5,
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
    filename = paste0("ppc-posterior.", d),
    path = file.path(here(), "3_figures", "appendix"),
    device = d,
    width = 15,
    height = 15,
    units = "cm",
    dpi = 1000,
    bg = "white"
  )
}
