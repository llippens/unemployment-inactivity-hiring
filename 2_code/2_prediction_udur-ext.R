source(file.path(here(), "2_code", "3_heterogeneity.R"))

data.visual.udur.out <-
  tibble(author = mreg.ur.out.pred$author,
         x1 = data.out.adj$unemployment_duration_months,
         x2 = data.out.adj$unemployment_rate,
         x2_cat = data.out.adj$unemployment_rate_categorical,
         y_exp = exp(ma.out.adj$TE)-1,
         y_pred = mreg.ur.out.pred$pred,
         w = ma.out.adj$w.random)