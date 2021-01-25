inform_prior <- function(ip, trt, cnf, size) {
  df <- data.table(trt, cnf)
  mm <- model.matrix(~ trt*(.), df)
  logodds <- reduce(map(ip, ~ mm %*% .x), cbind)
  map_dbl(prob_category(split(logodds, seq(nrow(logodds)))), ~ rcat(1, .x) - 1)
}

gen_inform_prior <- function(size, n_cnf, seed) {
  n_int <- size - 1
  n_coef <- 1 + 2*n_cnf
  set.seed(seed)
  coef <- runif(n_coef, -0.5, .5)
  intercepts <- runif(n_int, -.5, .5)
  intercepts <- intercepts[order(intercepts, decreasing = TRUE)]
  map(intercepts, ~ c(.x, coef))
}

prob_category <- function(logodds) {
  map(logodds, ~ prob_subtract(plogis(.x)))
}

prob_subtract <- function(probs) {
  n <- length(probs) + 1
  out <- rep(NA, n)
  for (i in seq_along(probs)) {
    if (i == 1) {
      out[i] <- 1 - probs[i]
    } else {
      out[i] <- probs[i - 1] - probs[i]
    }
  }
  out[n] <- probs[i]
  out
}
