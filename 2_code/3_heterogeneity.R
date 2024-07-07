if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               meta,
               dmetar,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "2_metareg.R"))

# Prediction interval
paste0("Prediction interval: [",
       round(ma$lower.predict, 2),
       "; ",
       round(ma$upper.predict, 2),
       "]")


# Outlier analysis
## Outlier-adjusted results
### Main model
ma.out.adj <-
  metabin_cstm(data = data.out.adj)

ma.out.adj

mreg.dur.out.adj <-
  rma(
    yi = ma.out.adj$TE,
    sei = ma.out.adj$seTE,
    data = data.out.adj,
    method = "REML",
    mods =
      ~ unemployment_duration_months,
    test = "knha"
  )

mreg.dur.out.adj.emm <-
  emmprep(mreg.dur.out.adj) %>%
  emmeans(specs = "1",
          weights = "proportional")

mreg.out.adj <-
  rma(
    yi = ma.out.adj$TE,
    sei = ma.out.adj$seTE,
    data = data.out.adj,
    method = "REML",
    mods =
      ~ unemployment_duration_months +
      unemployment_rate +
      response_type +
      region + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.out.adj.emm <-
  emmprep(mreg.out.adj) %>%
  emmeans(specs = "1",
          weights = "proportional")

mreg.ur.out.adj <-
  rma(
    yi = ma.out.adj$TE,
    sei = ma.out.adj$seTE,
    data = data.out.adj,
    method = "REML",
    mods =
      ~ unemployment_duration_months * unemployment_rate +
      response_type +
      region + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.ur.out.pred <-
  predict(mreg.ur.out.adj, transf = exp) %>%
  as_tibble() %>%
  mutate(x1 = data.out.adj$unemployment_duration_months,
         x2 = data.out.adj$unemployment_rate,
         author = data.out.adj$authors)

mreg.ur.out.adj.emm <-
  emmprep(mreg.ur.out.adj) %>%
  emmeans(specs = "1",
          weights = "proportional")



list(dur = mreg.dur.out.adj,
     reg = mreg.out.adj,
     urx = mreg.ur.out.adj) %>%
  lapply(function(x) robust(x,
                            cluster = cluster,
                            adjust = TRUE,
                            clubSandwich = TRUE)) %>%
  msummary_cstm()



### Sub-models
ma.0m6m.out.adj <-
  metabin_cstm(data = data.0m6m.out.adj)
ma.0m6m.out.adj

mreg.0m6m.out.adj <-
  rma(
    yi = ma.0m6m.out.adj$TE,
    sei = ma.0m6m.out.adj$seTE,
    data = data.0m6m.out.adj, 
    method = "REML",
    mods =
      ~ unemployment_rate +
      response_type +
      region_agg + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.0m6m.out.adj.emm <-
  emmprep(mreg.0m6m.out.adj) %>%
  emmeans(specs = "1",
          weights = "proportional")


ma.6m12m.out.adj <-
  metabin_cstm(data = data.6m12m.out.adj)
ma.6m12m.out.adj

mreg.6m12m.out.adj <-
  rma(
    yi = ma.6m12m.out.adj$TE,
    sei = ma.6m12m.out.adj$seTE,
    data = data.6m12m.out.adj, 
    method = "REML",
    mods =
      ~ unemployment_rate +
      response_type +
      region_agg +
      age_categorical,
    test = "knha"
  )

mreg.6m12m.out.adj.emm <-
  emmprep(mreg.6m12m.out.adj) %>%
  emmeans(specs = "1",
          weights = "proportional")


ma.12m18m.out.adj <-
  metabin_cstm(data = data.12m18m.out.adj)
ma.12m18m.out.adj

mreg.12m18m.out.adj.f <-
  rma(
    yi = ma.12m18m.out.adj$TE,
    sei = ma.12m18m.out.adj$seTE,
    data = data.12m18m.out.adj,
    method = "REML",
    mods =
      ~ unemployment_rate +
      response_type +
      region_agg + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.12m18m.out.adj <-
  rma(
    yi = ma.12m18m.out.adj$TE,
    sei = ma.12m18m.out.adj$seTE,
    data = data.12m18m.out.adj,
    method = "REML",
    mods =
      ~ unemployment_rate +
      response_type +
      gender,
    test = "knha"
  )

mreg.12m18m.out.adj.emm <-
  emmprep(mreg.12m18m.out.adj) %>%
  emmeans(specs = "1",
          weights = "proportional")


ma.18m36m.out.adj <-
  metabin_cstm(data = data.18m36m.out.adj)
ma.18m36m.out.adj

mreg.18m36m.out.adj.f <-
  rma(
    yi = ma.18m36m.out.adj$TE,
    sei = ma.18m36m.out.adj$seTE,
    data = data.18m36m.out.adj,
    method = "REML",
    mods =
      ~ unemployment_rate +
      response_type +
      region_agg + years_average +
      gender + age_categorical,
    test = "knha"
  )

mreg.18m36m.out.adj <-
  rma(
    yi = ma.18m36m.out.adj$TE,
    sei = ma.18m36m.out.adj$seTE,
    data = data.18m36m.out.adj,
    method = "REML",
    mods =
      ~ unemployment_rate +
      response_type,
    test = "knha"
  )

mreg.18m36m.out.adj.emm <-
  emmprep(mreg.18m36m.out.adj) %>%
  emmeans(specs = "1",
          weights = "proportional")

mreg.18m36m.out.adj.emm



ma_out_objects <-
  list(
    ma.out.adj = ma.out.adj,
    ma.0m6m.out.adj = ma.0m6m.out.adj,
    ma.6m12m.out.adj = ma.6m12m.out.adj,
    ma.12m18m.out.adj = ma.12m18m.out.adj,
    ma.18m36m.out.adj = ma.18m36m.out.adj
  )

ma.out.tibble <-
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

for(model in names(ma_out_objects)){
  ma.out.tibble <-
    ma.out.tibble %>%
    add_row(
      model = model,
      est = exp(ma_out_objects[[model]]$TE.random)-1,
      se = exp(ma_out_objects[[model]]$TE.random) * ma_out_objects[[model]]$seTE.random,
      ci_lower = exp(ma_out_objects[[model]]$lower.random)-1,
      ci_upper = exp(ma_out_objects[[model]]$upper.random)-1,
      t = ma_out_objects[[model]]$statistic.random,
      p = ma_out_objects[[model]]$pval.random,
      I2 = ma_out_objects[[model]]$I2
    )
}

ma.out.tibble <- 
  ma.out.tibble %>%
  mutate(across(.cols = c(est, se, ci_lower, ci_upper, I2),
                .fns = ~fround(., 4)),
         t = fround(t, 2),
         p = fround(p, 3),
         across(everything(),
                .fns = ~str_replace_all(., "-", "–")))



list(le6m = mreg.0m6m.out.adj,
     le12m = mreg.6m12m.out.adj,
     le18m = mreg.12m18m.out.adj.f,
     le36m = mreg.18m36m.out.adj.f) %>%
  lapply(function(x) robust(x,
                            cluster = cluster,
                            adjust = TRUE,
                            clubSandwich = TRUE)) %>%
  msummary_cstm()


# Influence analysis
## Leave-1-out
l1o <-
  leave1out(rma(
  yi = ma$TE,
  sei = ma$seTE,
  data = data,
  method = "REML",
  test = "knha")
)

## Outliers detected through dmetar::find.outliers()
## Influence through dmetar::InfluenceAnalysis() where influence on heterogeneity > 10 or influence on result > 8
