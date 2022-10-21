# tmle <- function(data, V, learners) {
#   t_1 <- lmtp::lmtp_tmle(
#     as.data.frame(data), "t", "y", 
#     grep("^x_", names(data), value = TRUE),
#     shift = lmtp::static_binary_on, 
#     learners_outcome = learners, 
#     learners_trt = learners, 
#     folds = V
#   )
#   
#   t_0 <- lmtp::lmtp_tmle(
#     as.data.frame(data), "t", "y", 
#     grep("^x_", names(data), value = TRUE),
#     shift = lmtp::static_binary_off,
#     learners_outcome = learners, 
#     learners_trt = learners, 
#     folds = V
#   )
#   
#   lmtp::lmtp_contrast(t_1, ref = t_0)$vals$theta
# }

gcomp <- function(data) {
  # d1 <- data.table::copy(data)
  # d0 <- data.table::copy(data)
  # d1[, t := 1]
  # d0[, t := 0]
  f <- reformulate(c("t", grep("^x_", names(data), value = TRUE)), "y")
  fit <- marginaleffects::comparisons(glm(f, data = data, family = "binomial"), 
                                      variables = list(t = 0:1))
  # mean(
  #   predict(fit, newdata = d1, type = "response") - 
  #     predict(fit, newdata = d0, type = "response")
  # )
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
  # prop <- CBPS::CBPS(f, data = data, ATT = 0)$fitted.values
  # coef(lm(y ~ t, data = data, weights = (data$t / prop) + ((1 - data$t) / (1 - prop))))[2]
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
