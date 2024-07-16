if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               meta,
               metasens,
               dmetar,
               puniform,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "2_metareg.R"))
source(file.path(here(), "2_code", "2_metareg-ext.R"))

apply_metabin_cstm_uc <- function(data) {
  result <- metabin_cstm_uc(data)
  return(result)
}

data_objects <-
  list(
    data = data,
    data.0m6m = data.0m6m,
    data.6m12m = data.6m12m,
    data.12m18m = data.12m18m,
    data.18m36m = data.18m36m,
    #data.out = data.out.adj,
    #data.0m6m.out = data.0m6m.out.adj,
    #data.6m12m.out = data.6m12m.out.adj,
    #data.12m18m.out = data.12m18m.out.adj, # no outliers
    data.18m36m.out = data.18m36m.out.adj
  )

ma_objects <-
  list(
    ma = ma,
    ma.0m6m = ma.0m6m,
    ma.6m12m = ma.6m12m,
    ma.12m18m = ma.12m18m,
    ma.18m36m = ma.18m36m
  )

ma.uc <- lapply(data_objects, apply_metabin_cstm_uc)

# Small-study methods
## Eggers-Pustejovsky-Peters test
metabias.results <- list()
metabias.tibble <-
  tibble(
    test = character(),
    model = character(),
    est = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stat = numeric(),
    p = numeric()
  )

for(test in c("Egger", "Pustejovsky", "Peters")){
  for (name in names(ma.uc)[1:5]){
    metabias.results[[test]][[name]] <- metabias(ma.uc[[name]], method.bias = test)
    
    metabias.tibble <- 
      metabias.tibble %>%
      add_row(
        test = test,
        model = name,
        est = metabias.results[[test]][[name]]$estimate[[1]],
        se = metabias.results[[test]][[name]]$estimate[[2]],
        ci_lower = metabias.results[[test]][[name]]$estimate[[1]] -
          1.96 * metabias.results[[test]][[name]]$estimate[[2]],
        ci_upper = metabias.results[[test]][[name]]$estimate[[1]] +
          1.96 * metabias.results[[test]][[name]]$estimate[[2]],
        stat = metabias.results[[test]][[name]]$statistic,
        p = metabias.results[[test]][[name]]$p.value
      )
  }
}

metabias.tibble <-
  metabias.tibble %>%
  mutate(across(.cols = c(est, se, ci_lower, ci_upper, stat),
                .fns = ~fround(., 4)),
         p = fround(p, 3),
         across(everything(),
                .fns = ~str_replace_all(., "-", "–")))

## PET-PEESE
petpeese_data_prep <- function(ma_obj) {
  data.pp <- 
    tibble(
      TE = ma_obj$TE,
      seTE_c = sqrt((ma_obj$n.e + ma_obj$n.c)/(ma_obj$n.e * ma_obj$n.c))
    ) %>%
    mutate(
      seTE_c2 = seTE_c^2,
      w_k = 1/seTE_c2
    )
  
  pet.ma <- lm(TE ~ seTE_c, weights = w_k, data = data.pp)
  pet.coefficients <- summary(pet.ma)$coefficients
  
  peese.ma <- lm(TE ~ seTE_c2, weights = w_k, data = data.pp)
  peese.coefficients <- summary(peese.ma)$coefficients
  
  return(list(pet = pet.coefficients,
              peese = peese.coefficients))
}

petpeese_data_prep_rma <- function(ma_obj, data_obj){
  data <-
    data_obj %>%
    mutate(seTE_c = sqrt((ma_obj$n.e + ma_obj$n.c)/(ma_obj$n.e * ma_obj$n.c)),
           seTE_c2 = seTE_c^2)
  
  pet.rma <-
    rma.uni(
      yi = ma_obj$TE,
      sei = ma_obj$seTE,
      data = data,
      mods = ~seTE_c,
      method = "FE"
    )
  
  peese.rma <-
    rma.uni(
      yi = ma_obj$TE,
      sei = ma_obj$seTE,
      data = data,
      mods = ~seTE_c2,
      method = "FE"
    )
  
  return(list(pet_summary = summary(pet.rma), peese_summary = summary(peese.rma)))
}

pp.rma.results <- mapply(petpeese_data_prep_rma, ma_objects, data_objects[1:5], SIMPLIFY = FALSE)

pp.rma.tibble <-
  tibble(
    method = character(),
    model = character(),
    est = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stat = numeric(),
    p = numeric()
  )

for(method in c("pet_summary", "peese_summary")){
  for (model in names(ma_objects)){
    pp.rma.tibble <- 
      pp.rma.tibble %>%
      add_row(
        method = method,
        model = model,
        est = pp.rma.results[[model]][[method]]$b[[1]],
        se = pp.rma.results[[model]][[method]]$se[[1]],
        ci_lower = pp.rma.results[[model]][[method]]$ci.lb[[1]],
        ci_upper = pp.rma.results[[model]][[method]]$ci.ub[[1]],
        stat = pp.rma.results[[model]][[method]]$zval[[1]],
        p = pp.rma.results[[model]][[method]]$pval[[1]]
      )
  }
}

pp.rma.tibble <-
  pp.rma.tibble %>%
  mutate(across(.cols = c(est, se, ci_lower, ci_upper, stat),
                .fns = ~fround(., 4)),
         p = fround(p, 3),
         across(everything(),
                .fns = ~str_replace_all(., "-", "–")))

## Limit meta-analysis
limitmeta.results <- list()
limitmeta.tibble <-
  tibble(
    model = character(),
    est_adj = numeric(),
    se_adj = numeric(),
    ci_lower_adj = numeric(),
    ci_upper_adj = numeric(),
    stat_adj = numeric(),
    p_adj = numeric(),
    stat_ss = numeric()
  )

for (name in c(names(ma.uc)[1:4], names(ma.uc)[[6]])){
  limitmeta.results[[name]] <- limitmeta(ma.uc[[name]])
  
  limitmeta.tibble <-
    limitmeta.tibble %>%
    add_row(
      model = name,
      est_adj = exp(limitmeta.results[[name]]$TE.adjust)-1,
      se_adj = exp(limitmeta.results[[name]]$TE.adjust)*
        limitmeta.results[[name]]$seTE.adjust,
      ci_lower_adj = exp(limitmeta.results[[name]]$lower.adjust)-1,
      ci_upper_adj = exp(limitmeta.results[[name]]$upper.adjust)-1,
      stat_adj = limitmeta.results[[name]]$statistic.adjust,
      p_adj = limitmeta.results[[name]]$pval.adjust,
      stat_ss = limitmeta.results[[name]]$Q.small
    )
}

limitmeta.tibble <-
  limitmeta.tibble %>%
  mutate(across(.cols = c(est_adj, se_adj, ci_lower_adj, ci_upper_adj,
                          stat_adj, stat_ss),
                .fns = ~fround(., 4)),
         p_adj = fround(p_adj, 3),
         across(everything(),
                .fns = ~str_replace_all(., "-", "–")))

## P-curve
pcurve.results <- list()
pcurve.tibble <-
  tibble(
    model = character(),
    rskew_p_bn = numeric(),
    rskew_z_f = numeric(),
    rskew_p_f = numeric(),
    rskew_z_h = numeric(),
    rskew_p_h = numeric(),
    flat_p_bn = numeric(),
    flat_z_f = numeric(),
    flat_p_f = numeric(),
    flat_z_h = numeric(),
    flat_p_h = numeric(),
    ev_prs = character(),
    ev_abs = character()
  )
pcurve.plot.data <- tibble()

for (name in names(ma.uc)[1:5]) {
  pcurve.results[[name]] <- pcurve(ma.uc[[name]])
  
  pcurve.tibble <-
    pcurve.tibble %>%
    add_row(
      model = name,
      rskew_p_bn = pcurve.results[[name]]$pcurveResults[[1]],
      rskew_z_f = pcurve.results[[name]]$pcurveResults[[3]],
      rskew_p_f = pcurve.results[[name]]$pcurveResults[[5]],
      rskew_z_h = pcurve.results[[name]]$pcurveResults[[7]],
      rskew_p_h = pcurve.results[[name]]$pcurveResults[[9]],
      flat_p_bn = pcurve.results[[name]]$pcurveResults[[2]],
      flat_z_f = pcurve.results[[name]]$pcurveResults[[4]],
      flat_p_f = pcurve.results[[name]]$pcurveResults[[6]],
      flat_z_h = pcurve.results[[name]]$pcurveResults[[8]],
      flat_p_h = pcurve.results[[name]]$pcurveResults[[10]],
      ev_prs = pcurve.results[[name]]$EvidencePresent,
      ev_abs = pcurve.results[[name]]$EvidenceAbsent
    )
  
  pcurve.plot.data <-
    pcurve.plot.data %>%
    rbind(
      janitor::clean_names(as_tibble(pcurve.results[[name]]$PlotData)) %>%
        mutate(model = name) %>%
        relocate(model)
    )
}

## P-uniform*
punistar.results <- list()

punistar_cstm <-
  function(ma_obj){
    puni_star(tobs = ma_obj$statistic,
              ni = ma_obj$n.e + ma_obj$n.c,
              alpha = 0.05,
              side = "left",
              method = "ML")
  }

for(name in names(ma.uc)) {
  punistar.results[[name]] <- punistar_cstm(ma_obj = ma.uc[[name]])
}

## Three-parameter selection model
selmod.p05 <- list()
selmod.p10 <- list()
selmod.p05.tibble <-
  tibble(
    model = character(),
    alpha_twosided = numeric(),
    est = numeric(),
    se = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    p = numeric(),
    lrt = numeric(),
    lrt_p = numeric()
  )
selmod.p10.tibble <- selmod.p05.tibble

for(model in names(ma_objects[1:5])){
  rma.int <-
    rma(
      yi = ma_objects[[model]]$TE,
      sei = ma_objects[[model]]$seTE,
      data = data,
      method = "REML",
      test = "knha"
    )
  
  selmod.p05[[model]] <-
    rma.int %>%
    selmodel(
      type = "stepfun",
      steps = 0.025
    )
  
  selmod.p05.tibble <-
    selmod.p05.tibble %>%
    add_row(
      model = model,
      alpha_twosided = 0.05,
      est = selmod.p05[[model]]$b[[1]],
      se = selmod.p05[[model]]$se,
      p = selmod.p05[[model]]$pval,
      ci_lower = selmod.p05[[model]]$ci.lb,
      ci_upper = selmod.p05[[model]]$ci.ub,
      lrt = selmod.p05[[model]]$LRT,
      lrt_p = selmod.p05[[model]]$LRTp
    )
  
  selmod.p10[[model]] <-
    rma.int %>%
    selmodel(
      type = "stepfun",
      steps = 0.05
    )
  
  selmod.p10.tibble <-
    selmod.p10.tibble %>%
    add_row(
      model = model,
      alpha_twosided = 0.10,
      est = selmod.p10[[model]]$b[[1]],
      se = selmod.p05[[model]]$se,
      p = selmod.p10[[model]]$pval,
      ci_lower = selmod.p10[[model]]$ci.lb,
      ci_upper = selmod.p10[[model]]$ci.ub,
      lrt = selmod.p10[[model]]$LRT,
      lrt_p = selmod.p10[[model]]$LRTp
    )
}

selmod.tibble <-
  selmod.p05.tibble %>%
  rbind(selmod.p10.tibble) %>%
  mutate(across(.cols = c(est, se, ci_lower, ci_upper, lrt),
                .fns = ~fround(., 4)),
         across(.cols = c(p, lrt_p),
                .fns = ~fround(., 3)),
         across(everything(),
                .fns = ~str_replace_all(., "-", "–")))
