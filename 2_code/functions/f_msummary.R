if (!require("pacman")) install.packages("pacman")
pacman::p_load(modelsummary, sandwich, install = TRUE, update = FALSE)

msummary_cstm <-
  function(
    model,
    out = "default",
    sig.dig = 4,
    p.dig = 3,
    est = "{estimate}{stars} ({std.error})",
    stat = NULL,
    ...
  ) {
    
    gof_custom <-
      tibble::tribble(
      ~raw, ~clean, ~fmt,
      "aic", "AIC", 2,
      "bic", "BIC", 2,
      "i2", "I^2", 4
    )
    
    modelsummary(
      model,
      output = out,
      fmt = fmt_statistic(
        "estimate" = sig.dig,
        "std.error" = sig.dig,
        "p.value" = p.dig
      ),
      estimate = est,
      statistic = stat,
      shape = term + statistic ~ model,
      gof_map = gof_custom,
      ...
    )
  }

msummary_brms <-
  function(
    model,
    out = "default",
    sig.dig = 4,
    p.dig = 3,
    est = "{estimate} ({std.error}) [{conf.low}, {conf.high}]",
    stat = NULL,
    ...
  ) {
    modelsummary(
      model,
      output = out,
      fmt = fmt_statistic(
        "estimate" = sig.dig,
        "std.error" = sig.dig,
        "conf.low" = sig.dig,
        "conf.high" = sig.dig,
        "p.value" = p.dig
      ),
      estimate = est,
      statistic = stat,
      shape = term + statistic ~ model,
      ...
    )
  }
