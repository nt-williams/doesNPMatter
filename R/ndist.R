#' @export
gendata <- function(dist, n, reps, tte = FALSE) {
  distc <- copy(dist)
  distc[, `:=`(prob_y = NULL, prob_w = NULL, prob_trt = NULL)]
  out <- replicate(reps, {
    out <- distc[sample(1:nrow(dist), size = n, replace = TRUE, prob = distc$prob), ]
    out[, outcome := y]
    if (!tte) {
      return(out[, `:=`(prob = NULL, y = NULL)])
    }
    out$outcome <- out$outcome + 1
    cens <- sample.int(n, n * 0.05)
    time <- round(purrr::map_dbl(out[cens]$outcome, ~ runif(1, 0, .x - 1)))
    out[cens, `:=`(status = TRUE, time = time)]
    out[, `:=`(time = fcoalesce(time, outcome),
               status = fifelse(is.na(status), 1, 0))]
    out[, `:=`(prob = NULL, y = NULL)]
  }, simplify = FALSE)
  return(out)
}

#' @export
create_dist <- function(binary_cnf, cont_cnf, size = 2, randomized = FALSE, seed) {
  set.seed(seed)
  if (cont_cnf == 0) {
    cont_cnf <- 1
  }
  ncats <- size * 2 * 2^binary_cnf * cont_cnf
  meta_data <- 1:ncats
  dist <- map_data(meta_data, size, binary_cnf, cont_cnf)
  nms_cnf <- grep("^cnf", names(dist), value = TRUE)

  ## hyper_param <- runif(ncats, 0, max = runif(ncats, 0, max = runif(ncats, 0, 20)))
  hyper_param <- rchisq(1, 4) * rep(1 / ncats^(1/3), ncats)

  dist[, prob := rdirichlet(1, hyper_param)[1,]]

  dist <- dist[prob > .Machine$double.eps]

  ## correcting for positivity violations
  dist[, prob := prob / sum(prob)]

  dist[, prob_w := sum(prob), by = nms_cnf]
  dist[, prob_trt := sum(trt * prob) / sum(prob), by = nms_cnf]
  dist[, prob_y := sum(y * prob) / sum(prob), by = c('trt', nms_cnf)]

  dist <- dist[prob_trt > .Machine$double.eps]
  dist <- dist[prob_trt < 1 - .Machine$double.eps]
  ## reassinging probability

  dist[, prob := prob / sum(prob)]

}

map_data <- function(meta_data, size, binary_cnf, cont_cnf) {
  y   <- alt_01(meta_data, 2, size)
  trt <- alt_01(meta_data, 1)
  cnf_meta <- alt_01(meta_data, 4, 2^binary_cnf * cont_cnf) + 1
  cnf_bin <- {
    if (binary_cnf > 0)
      sapply(1:binary_cnf, function(x) alt_01(cnf_meta, cont_cnf * 2^(x - 1)))
    else
      NULL
  }
  cnf_cont <- {
    if (cont_cnf > 1)
      mod_op(cnf_meta, cont_cnf)
    else
      NULL
  }
  cnf <- cbind(cnf_cont, cnf_bin)
  colnames(cnf) <- paste0("cnf", 1:(ncol(cnf)))
  data.table(trt, cnf, y)
}

truth_binary <- function(dist) {
    nms_cnf <- grep("^cnf", names(dist), value = TRUE)
    dist[, prob_w := sum(prob), by = nms_cnf]
    dist[, prob_y := sum(y * prob) / sum(prob), by = c('trt', nms_cnf)]
    out <- dist[y == 1, .(p = sum(prob_y * prob_w, na.rm = TRUE)), by = 'trt']
    return(out[trt == 1, ]$p - out[trt == 0, ]$p)
}

truth_ordinal <- function(dist) {
    nms_cnf <- grep("^cnf", names(dist), value = TRUE)
    dist[, prob_trt_w := sum(prob), by = c('trt', nms_cnf)]
    dist[, prob_y := prob / prob_trt_w]
    dist[, prob_w := sum(prob), by = nms_cnf]
    p1 <- dist[trt == 1, .(prob = sum(prob_y * prob_w)), by = 'y']
    p0 <- dist[trt == 0, .(prob = sum(prob_y * prob_w)), by = 'y']
    p1 <- cumsum(p1$prob)[-length(p1$prob)]
    p0 <- cumsum(p0$prob)[-length(p0$prob)]
    return(mean(log(p1 / (1 - p1) * (1 - p0) / p0)))
}

truth_tte <- function(dist, tau) {
  dat <- dist[, unique(.SD[, .(y1 = y1 + 1, y0 = y0 + 1, prob_y)])]
  p1 <- dat[, .(prob = sum(prob_y)), y1]
  p0 <- dat[, .(prob = sum(prob_y)), y0]
  S1 <- 1 - cumsum(p1$prob)
  S0 <- 1 - cumsum(p0$prob)
  return((S1 - S0)[tau])
}

positivity <- function(dist) {
  cnf <- grep("^cnf", names(dist), value = TRUE)
  probs <- dist[, .(prob_trt = weighted.mean(trt, prob),
                    prob_w = sum(prob)), cnf]
  data.table::setorderv(probs, cnf)
  prob_a <- with(probs, weighted.mean(prob_trt, prob_w))
  p1 <- probs[, max(prob_w / prob_trt)]
  p0 <- probs[, max(prob_w / (1 - prob_trt))]
  max(p1, p0)
}

variation_norm <- function(dist) {
    dist[, prob_y := sum(y * prob) / sum(prob), by = c('trt', 'cnf1')]
    vn1 <- mean(abs(diff(dist$prob_y[dist$trt == 1])))
    vn0 <- mean(abs(diff(dist$prob_y[dist$trt == 0])))
    vn_cate <-  mean(abs(diff(dist$prob_y[dist$trt == 1] - dist$prob_y[dist$trt == 0])))
    list(vn1 = vn1, vn0 = vn0, vn_cate = vn_cate)
}

alt_01 <- function(meta, x, cats = 2) {
  ((meta - 1) %/% x) %% cats
}
mod_op <- function(meta, x){
  (meta - 1) %% x + 1
}
