predict_coxph <- function(fit, time, newdata) {
  bh <- suppressWarnings(survival::basehaz(fit, centered = FALSE))
  bh <- bh[bh$time == time, ][1, 1]
  cf <- fit$coef
  mm <- model.matrix(fit$formula[-2], newdata)
  mm <- mm[, 2:ncol(mm)]
  exp(-bh)^exp(mm %*% cf)
}
