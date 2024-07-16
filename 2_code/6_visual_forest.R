if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               ggplot2, grid, gridExtra,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "2_metareg-ext.R"))

blue <- "#1E64C8"

ma.subs <- list(
  ma.0m6m = ma.0m6m,
  ma.6m12m = ma.6m12m,
  ma.12m18m = ma.12m18m,
  ma.18m36m = ma.18m36m
)

for(model_name in names(ma.subs)) {
  model <- ma.subs[[model_name]]
  
  meta::forest(model, 
               sortvar = TE,
               prediction = TRUE,
               fontsize = 9,
               fontfamily = "UGent Panno Text",
               big.mark = ",",
               studlab = model$data$uid,
               leftcols = c("studlab", "event.e", "n.e", "event.c", "n.c"),
               col.diamond = blue,
               col.predict = "grey50")
  
  plot <- grid.grab()
  
  h <- nrow(model$data) * .5 + 4.5
  
  for(d in c("png", "tiff")){
    ggsave(
      filename = paste0("forest-", model_name, ".", d),
      path = file.path(here(), "3_figures", "appendix", "forest"),
      plot = gridExtra::grid.arrange(plot),
      device = d,
      width = 19.5,
      height = h,
      units = "cm",
      dpi = 1000,
      bg = "white"
    )
  }
}