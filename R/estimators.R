gcomp <- function(data, tci = FALSE) {
  if (tci) {
    f <- as.formula(paste0("y~t", "*(", paste(grep("^x_", names(data), value = TRUE), collapse = "+"), ")"))
  } else {
    f <- reformulate(c("t", grep("^x_", names(data), value = TRUE)), "y")
  }
  
  fit <- marginaleffects::comparisons(glm(f, data = data, family = "binomial"), 
                                      variables = list(t = 0:1))
  list(psi = summary(fit)$estimate, 
       conf.low = summary(fit)$conf.low, 
       conf.high = summary(fit)$conf.high)
}

cbps <- function(data) {
  f <- reformulate(grep("^x_", names(data), value = TRUE), "t")
  prop <- WeightIt::weightit(f, data = data, method = "cbps", estimand = "ATE")
  d.w <- survey::svydesign(~ 1, weights = prop$weights, data = data)
  fit <- survey::svyglm(y ~ t, design = d.w, data = data)
  ci <- confint(fit)
  list(psi = as.numeric(coef(fit)[2]), 
       conf.low = ci[2, 1], 
       conf.high = ci[2, 2])
}

bart <- function(data) {
  fit <- bartCause::bartc(
    data$y, 
    data$t, 
    data[, grep("^x_", names(data), value = TRUE), with = FALSE], 
    estimand = "ate",
    n.chains = 1L, 
    n.burn = 250L, 
    n.samples = 250L
  )

  list(psi = summary(fit)$estimates$estimate, 
       conf.low = summary(fit)$estimates$ci.lower, 
       conf.high = summary(fit)$estimates$ci.upper)
}
