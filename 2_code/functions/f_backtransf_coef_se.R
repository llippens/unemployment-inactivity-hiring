backtransf_coef_se <- function(model) {
  se_exp <- function(estimate, se) {
    exp_estimate <- exp(estimate)
    exp_estimate * se
  }
  
  model$se <- se_exp(model$beta, model$se)
  model$beta <- exp(model$beta)-1
  model$b <- exp(model$b)-1
  model$ci.lb <- exp(model$ci.lb)-1
  model$ci.ub <- exp(model$ci.ub)-1
  
  return(model)
}