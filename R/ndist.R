#' @export
gendata <- function(dist, n, reps, tte = FALSE) {
  distc <- copy(dist)
  distc[, `:=`(prob_y = NULL, prob_w = NULL, prob_a = NULL)]
  out <- replicate(reps, {
    out <- distc[sample(1:nrow(dist), size = n, replace = TRUE, prob = distc$prob), ]
    out[, outcome := trt * y1 + (1 - trt) * y0][, `:=`(y0 = NULL, y1 = NULL, prob = NULL)]
    if (!tte) {
      return(out)
    }
    out$outcome <- out$outcome + 1
    cens <- sample.int(n, n * 0.05)
    time <- round(purrr::map_dbl(out[cens]$outcome, ~ runif(1, 0, .x - 1)))
    out[cens, `:=`(status = TRUE, time = time)]
    out[, `:=`(time = fcoalesce(time, outcome),
               status = fifelse(is.na(status), 1, 0))]
    out[]
  }, simplify = FALSE)
  return(out)
}

#' @export
create_dist <- function(binary_cnf, cont_cnf, size = 2, randomized = FALSE, seed) {
  set.seed(seed)
  if (cont_cnf == 0) {
    cont_cnf <- 1
  }
  meta_data <- 1:(size^2 * 2 * 2^binary_cnf * cont_cnf)
  all_obs <- map_data(meta_data, size, binary_cnf, cont_cnf)
  nms_cnf <- grep("^cnf", names(all_obs), value = TRUE)
  
  unique_pos <- all_obs[, unique(.SD[, .(y1, y0)])]
  unique_cnf <- all_obs[, unique(.SD[, nms_cnf, with = FALSE])]

  ## generate P(Y_1, Y_0)
  dist_y <- cbind(unique_pos, prob_y = rdirichlet(1, rep(1, size^2))[1, ])
  
  ## generate P(W | Y_1, Y_0) and append
  df_w <- runif(1, 0, 20)
  dist_w <- setNames(data.table(matrix(nrow = 0, ncol = 4 + length(nms_cnf))), c("y1", "y0", "prob_y", nms_cnf, "prob_w"))
  for (i in 1:nrow(unique_pos)) {
    dist_w <- rbind(dist_w, cbind(dist_y[i, ], unique_cnf, prob_w = rdirichlet(1, rchisq(2^binary_cnf * cont_cnf, df_w))[1, ]))
  }
  
  ## generate P(A | W)
  df_a <- runif(1, 0, 20)
  dist_a <- setNames(data.table(matrix(nrow = 0, ncol = 2 + length(nms_cnf))), c(nms_cnf, "trt", "prob_a"))
  for (j in 1:nrow(unique_cnf)) {
    if (!randomized) {
      prob_a <- rdirichlet(1, rchisq(2, df_a))[1, ]
    } else {
      prob_a <- rep(1/2, 2)
    }
    dist_a <- rbind(dist_a, cbind(unique_cnf[j, ], trt = c(0, 1), prob_a = prob_a))
  }

  ## merge probabilities
  probs <- merge(dist_w, dist_a, all = TRUE, allow.cartesian = TRUE)
  probs[, prob := prob_y * prob_w * prob_a][]
}

map_data <- function(meta_data, size, binary_cnf, cont_cnf) {
  y0 <- alt_01(meta_data, size * 2, size)
  y1 <- alt_01(meta_data, 2, size)
  trt <- alt_01(meta_data, 1)
  cnf_meta <- alt_01(meta_data, 1, 2^binary_cnf * cont_cnf) + 1
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
  data.table(trt, cnf, y0, y1)
}

truth_binary <- function(dist) {
  dat <- dist[, unique(.SD[, .(y1, y0, prob_y)])]
  p1 <- sum(dat[y1 == 1, prob_y])
  p0 <- sum(dat[y0 == 1, prob_y])
  return(p1 - p0)
}

truth_ordinal <- function(dist) {
  dat <- dist[, unique(.SD[, .(y1, y0, prob_y)])]
  p1 <- dat[, .(prob = sum(prob_y)), y1]
  p0 <- dat[, .(prob = sum(prob_y)), y0]
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
  p1 <- probs[, max(prob_a / prob_trt)]
  p0 <- probs[, max((1 - prob_a) / (1 - prob_trt))]
  max(p1, p0)
}

variation_norm <- function(dist) {
  cont_cnf <- dist$cnf1
  probs <- dist[, .(prob_y1 = weighted.mean(y1, prob), 
                    prob_y0 = weighted.mean(y0, prob)), cnf1][order(cnf1)]
  vn1 <- mean(abs(diff(probs$prob_y1)))
  vn0 <- mean(abs(diff(probs$prob_y0)))
  vn_cate <-  mean(abs(diff(probs$prob_y1 - probs$prob_y0)))
  list(vn1 = vn1, vn0 = vn0, vn_cate = vn_cate)
}

alt_01 <- function(meta, x, cats = 2) {
  ((meta - 1) %/% x) %% cats
}
mod_op <- function(meta, x){
  (meta - 1) %% x + 1
}
