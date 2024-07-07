if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "2_metareg.R"))



mreg.0m6m.f <-
  rma(
    yi = ma.0m6m$TE,
    sei = ma.0m6m$seTE,
    data = data.0m6m, 
    method = "REML",
    mods =
      ~ response_type +
      unemployment_rate +
      region_agg + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.0m6m.pred <- predict(mreg.0m6m.f, transf = exp) %>% as_tibble()

mreg.0m6m <-
  rma(
    yi = ma.0m6m$TE,
    sei = ma.0m6m$seTE,
    data = data.0m6m, 
    method = "REML",
    mods =
      ~ response_type +
      unemployment_rate +
      region_agg + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.0m6m.emm <-
  emmprep(mreg.0m6m) %>%
  emmeans(specs = "1",
          weights = "proportional")



mreg.6m12m.f <-
  rma(
    yi = ma.6m12m$TE,
    sei = ma.6m12m$seTE,
    data = data.6m12m, 
    method = "REML",
    mods =
      ~ response_type +
      unemployment_rate +
      region_agg + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.6m12m.pred <- predict(mreg.6m12m.f, transf = exp) %>% as_tibble()

mreg.6m12m <-
  rma(
    yi = ma.6m12m$TE,
    sei = ma.6m12m$seTE,
    data = data.6m12m, 
    method = "REML",
    mods =
      ~ response_type +
      unemployment_rate + 
      region_agg + years_average +
      age_categorical,
    test = "knha"
  )

mreg.6m12m.emm <-
  emmprep(mreg.6m12m) %>%
  emmeans(specs = "1",
          weights = "proportional")



mreg.12m18m.f <-
  rma(
    yi = ma.12m18m$TE,
    sei = ma.12m18m$seTE,
    data = data.12m18m,
    method = "REML",
    mods =
      ~ response_type +
      unemployment_rate +
      region_agg + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.12m18m.pred <- predict(mreg.12m18m.f, transf = exp) %>% as_tibble()

mreg.12m18m <-
  rma(
    yi = ma.12m18m$TE,
    sei = ma.12m18m$seTE,
    data = data.12m18m,
    method = "REML",
    mods =
      ~ response_type +
      years_average +
      gender,
    test = "knha"
  )

mreg.12m18m.emm <-
  emmprep(mreg.12m18m) %>%
  emmeans(specs = "1",
          weights = "proportional")



mreg.18m36m.f <-
  rma(
    yi = ma.18m36m$TE,
    sei = ma.18m36m$seTE,
    data = data.18m36m,
    method = "REML",
    mods =
      ~ response_type +
      unemployment_rate +
      region_agg + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.18m36m.pred <- predict(mreg.18m36m.f, transf = exp) %>% as_tibble()

mreg.18m36m <-
  rma(
    yi = ma.18m36m$TE,
    sei = ma.18m36m$seTE,
    data = data.18m36m,
    method = "REML",
    mods =
      ~ response_type +
      unemployment_rate + 
      years_average,
    test = "knha"
  )

mreg.18m36m.emm <-
  emmprep(mreg.18m36m) %>%
  emmeans(specs = "1",
          weights = "proportional")



list(le6m = mreg.0m6m.f,
     le12m = mreg.6m12m.f,
     le18m = mreg.12m18m.f,
     le36m = mreg.18m36m.f) %>%
  lapply(function(x) robust(x,
                            cluster = cluster,
                            adjust = TRUE,
                            clubSandwich = TRUE)) %>%
  msummary_cstm()
