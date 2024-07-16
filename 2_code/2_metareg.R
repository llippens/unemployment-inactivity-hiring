if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               meta, metafor,
               sandwich, clubSandwich,
               emmeans,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "1_wrangling.R"))
source(file.path(here(), "2_code", "functions", "f_metabin.R"))
source(file.path(here(), "2_code", "functions", "f_msummary.R"))
source(file.path(here(), "2_code", "functions", "f_fround.R"))



ma <- metabin_cstm(data = data)
ma.0m6m <- metabin_cstm(data = data.0m6m)
ma.6m12m <- metabin_cstm(data = data.6m12m)
ma.12m18m <- metabin_cstm(data = data.12m18m)
ma.18m36m <- metabin_cstm(data = data.18m36m)

ma_objects <-
  list(
    ma = ma,
    ma.0m6m = ma.0m6m,
    ma.6m12m = ma.6m12m,
    ma.12m18m = ma.12m18m,
    ma.18m36m = ma.18m36m
  )

ma.tibble <-
  tibble(
    model = character(),
    est = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    t = numeric(),
    p = numeric(),
    I2 = numeric()
  )

for(model in names(ma_objects)){
  ma.tibble <-
    ma.tibble %>%
    add_row(
      model = model,
      est = exp(ma_objects[[model]]$TE.random)-1,
      se = exp(ma_objects[[model]]$TE.random) * ma_objects[[model]]$seTE.random,
      ci_lower = exp(ma_objects[[model]]$lower.random)-1,
      ci_upper = exp(ma_objects[[model]]$upper.random)-1,
      t = ma_objects[[model]]$statistic.random,
      p = ma_objects[[model]]$pval.random,
      I2 = ma_objects[[model]]$I2
    )
}

ma.tibble <- 
  ma.tibble %>%
  mutate(across(.cols = c(est, se, ci_lower, ci_upper, I2),
                .fns = ~fround(., 4)),
         t = fround(t, 2),
         p = fround(p, 3),
         across(everything(),
                .fns = ~str_replace_all(., "-", "–")))



mreg.dur.nc <-
  rma(
    yi = ma$TE,
    sei = ma$seTE,
    data = data,
    method = "REML",
    mods =
      ~ unemployment_duration_months,
    test = "knha"
  )

mreg.dur.nc.pred.gen <- predict(mreg.dur.nc, transf = exp) %>% as_tibble()

mreg.dur.nc.emm <-
  emmprep(mreg.dur.nc) %>%
  emmeans(specs = "1",
          weights = "proportional")



mreg <-
  rma(
    yi = ma$TE,
    sei = ma$seTE,
    data = data,
    method = "REML",
    mods =
      ~ unemployment_duration_months +
      unemployment_rate +
      response_type +
      country + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.pred.gen <-
  predict(mreg %>%
            robust(cluster = cluster, adjust = TRUE, clubSandwich = TRUE),
          transf = exp) %>%
  as_tibble() %>%
  mutate(x = data$unemployment_duration_months)



mreg.region <-
  rma(
    yi = ma$TE,
    sei = ma$seTE,
    data = data,
    method = "REML",
    mods =
      ~ unemployment_duration_months +
      unemployment_rate +
      response_type +
      region + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.emm <-
  emmprep(mreg.region) %>%
  emmeans(specs = "1",
          weights = "proportional")

mreg.region.pred.gen <-
  predict(mreg.region %>%
            robust(cluster = cluster, adjust = TRUE, clubSandwich = TRUE),
          transf = exp) %>%
  as_tibble() %>%
  mutate(x = data$unemployment_duration_months)



mreg.ur <-
  rma(
    yi = ma$TE,
    sei = ma$seTE,
    data = data,
    method = "REML",
    mods =
      ~ unemployment_duration_months * unemployment_rate +
      response_type +
      region + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.ur.emm <-
  emmprep(mreg.ur) %>%
  emmeans(specs = "1",
          weights = "proportional")

mreg.ur.pred.gen <-
  predict(mreg.ur, transf = exp) %>%
  as_tibble() %>%
  mutate(x1 = data$unemployment_duration_months,
         x2 = data$unemployment_rate,
         author = data$authors)



list(dur = mreg.dur.nc,
     reg = mreg.region,
     urx = mreg.ur) %>%
  lapply(function(x) robust(x,
                            cluster = cluster,
                            adjust = TRUE,
                            clubSandwich = TRUE)) %>%
  msummary_cstm()
