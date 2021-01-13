ip <- function(trt, cnf, size) {
  df <- data.table(trt, cnf)
  mm <- model.matrix(~ trt*(.), df)
  set.seed(63742)
  ip <- gen_ip(size, ncol(cnf))
  # probs_i <- list()
  logodds <- reduce(map(ip, ~ mm %*% .x), cbind)
  # for (i in 1:nrow(mm)) {
  #   probs_k <- rep(NA, length(ip))
  #   for (k in seq_along(ip)) {
  #     x <- 0
  #     for (j in seq_along(ip[[k]])) {
  #       x <- x + mm[i, j] * ip[[k]][j]
  #     }
  #     probs_k[k] <- x
  #   }
  #   probs_i[[i]] <- probs_k
  # }
  map_dbl(prob_category(split(logodds, seq(nrow(logodds)))), 
          ~ sample(0:(size - 1), 1, prob = .x))
}

gen_ip <- function(size, n_cnf) {
  n_int <- (size - 1)
  n_coef <- 1 + 2*n_cnf
  # mu <- rep(0.8, n_coef)
  # sigma <- diag(runif(n_coef, 0.4, 0.7))
  set.seed(6523734)
  # coef <- MASS::mvrnorm(1, mu, sigma)
  coef <- runif(n_coef, -0.2, 0.1)
  intercepts <- runif(n_int, -2, 2)
  intercepts <- intercepts[order(intercepts, decreasing = TRUE)]
  # ip[1:(size - 1)]
  # coef <- setdiff(ip, intercepts)
  # intercepts <- intercepts[order(intercepts, decreasing = TRUE)]
  map(intercepts, ~ c(.x, coef))
}

prob_category <- function(x) {
  map(x, ~ prob_subtract(plogis(.x)))
}

prob_subtract <- function(probs) {
  out <- rep(NA, length(probs))
  for (i in seq_along(probs)) {
    if (i == 1) {
      out[i] <- 1 - probs[i]
    } else {
      out[i] <- probs[i - 1] - probs[i]
    }
  }
  c(out, probs[i])
}
