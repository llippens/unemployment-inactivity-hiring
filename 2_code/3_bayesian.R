if (!require("pacman")) install.packages("pacman")
pacman::p_load(here,
               brms, rstanarm,
               broom.mixed,
               install = TRUE,
               update = FALSE)

source(file.path(here(), "2_code", "1_wrangling.R"))
source(file.path(here(), "2_code", "functions", "f_msummary.R"))

# Priors
priors <- c(prior(normal(0, 1), class = Intercept), # mu: class -> fixed population-effect
            prior(cauchy(0, 0.5), class = sd)) # tau: class -> variance

# Function
brms_fit_model <- function(data) {
  m.brm <-
    brm(logrr | se(selogrr) ~ 1 + (1 | cluster),
        data = data,
        prior = priors,
        iter = 5000,
        future = TRUE,
        seed = 8888)
  
  brms_mod_summary <-
    list(
      tidy = tidy(m.brm,
                  effects = "fixed",
                  conf.level = 0.95) %>%
        mutate(std.error = exp(estimate) * std.error, # Approximate back-transformation of logserr via Delta method
               estimate = exp(estimate)-1,
               conf.low = exp(conf.low)-1,
               conf.high = exp(conf.high)-1),
      glance = NULL
    )
  class(brms_mod_summary) <- "modelsummary_list"
  
  return(list(summary = brms_mod_summary, model = m.brm))
}

# Full model
data.full.objects <-
  list(
    data = data,
    data.out.adj = data.out.adj
  )

m.brm.full <- list()
m.brm.full.m <- list()

for(object in names(data.full.objects)){
  fit_result <- brms_fit_model(data.full.objects[[object]])
  m.brm.full[[object]] <- fit_result$summary
  m.brm.full.m[[object]] <- fit_result$model
}

msummary_brms(m.brm.full)

# Submodels by unemployment duration
data.sub.objects <-
  list(
    data.0m6m = data.0m6m,
    data.6m12m = data.6m12m,
    data.12m18m = data.12m18m,
    data.18m36m = data.18m36m,
    data.0m6m.out.adj = data.0m6m.out.adj,
    data.6m12m.out.adj = data.6m12m.out.adj,
    #data.12m18m.out.adj = data.12m18m.out.adj,
    data.18m36m.out.adj = data.18m36m.out.adj
  )

m.brm.sub <- list()
m.brm.sub.m <- list()

for(object in names(data.sub.objects)){
  fit_result <- brms_fit_model(data.sub.objects[[object]])
  m.brm.sub[[object]] <- fit_result$summary
  m.brm.sub.m[[object]] <- fit_result$model
}

msummary_brms(m.brm.sub)

# Posterior predictive check

ppc <- list()

for(model in names(m.brm.full.m)){
  ppc[[model]] <- rstanarm::pp_check(m.brm.full.m[[model]],
                                     nreps = 10,
                                     seed = 8888)
}

for(model in names(m.brm.sub.m)){
  ppc[[model]] <- rstanarm::pp_check(m.brm.sub.m[[model]],
                                     nreps = 10,
                                     seed = 8888)
}
