ma.ur.low <-
  metabin_cstm(data = data %>%
                 filter(unemployment_rate_categorical == "Low [3%, 7%)"))

mreg.ur.low <-
  rma(
    yi = ma.ur.low$TE,
    sei = ma.ur.low$seTE,
    data = data %>%
      filter(unemployment_rate_categorical == "Low [3%, 7%)"),
    method = "REML",
    mods =
      ~ unemployment_duration_months +
      response_type +
      region + years_average +
      age_categorical,
    test = "knha"
  )

mreg.ur.low.emm <-
  emmprep(mreg.ur.low) %>%
  emmeans(specs = "1",
          type = "response",
          weights = "proportional")

mreg.ur.low.emm %>%
  as_tibble() %>%
  mutate(SE = exp(emmean)*SE,
         across(.cols = c(emmean, lower.CL, upper.CL),
                .fns = ~exp(.)-1))



ma.ur.high <-
  metabin_cstm(data = data %>%
                 filter(unemployment_rate_categorical != "Low [3%, 7%)"))

mreg.ur.high <-
  rma(
    yi = ma.ur.high$TE,
    sei = ma.ur.high$seTE,
    data = data %>%
      filter(unemployment_rate_categorical != "Low [3%, 7%)"),
    method = "REML",
    mods =
      ~ unemployment_duration_months +
      response_type +
      region,
    test = "knha"
  )

mreg.ur.high.emm <-
  emmprep(mreg.ur.high) %>%
  emmeans(specs = "1",
          type = "response",
          weights = "proportional")

mreg.ur.high.emm %>%
  as_tibble() %>%
  mutate(SE = exp(emmean)*SE,
         across(.cols = c(emmean, lower.CL, upper.CL),
                .fns = ~exp(.)-1))
