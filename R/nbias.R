box::use(DGP = ./dgp, stats[...], data.table[...])

#' @param dgp a DGP created with \code{dgp}.
#' @param n number of observations to draw from a DGP.
#' @param reps number of datasets to generate.
#' @param method the estimation method used to estimate the ATE.
#' 
#' @export
simulate <- function(dgp, n, reps = 1, 
                     method = c("gcomp", "ptmle", "tmle", "cvtmle")) {
  lookup <- DGP$dgp_lookup(dgp)
  estims <- estimate_ATE(lookup, n, reps, match.arg(method))
  data.table(
    xs = dgp$xs, 
    truth = dgp$truth, 
    unadj = dgp$unadj, 
    confounding_bias = dgp$bias, 
    estimator_bias = mean(estims) - dgp$truth
  )
}

estimate_ATE <- function(dgp, n, reps = 1, 
                         method = c("gcomp", "ptmle", "tmle", "cvtmle")) {
  .data <- replicate(reps, DGP$sample_dgp(dgp, n), simplify = FALSE)
  switch(match.arg(method), 
         gcomp = gcomputation(.data), 
         tmle = tmle(.data),
         ptmle = ptmle(.data),
         cvtmle = cvtmle(.data))
}

#' Estimate the ATE using G-computation with main-effects GLMs
#' 
#' @param data Data created from \code{sample_dgp}.
gcomputation <- function(data) {
  out <- lapply(data, function(d) {
    on <- copy(d)
    off <- copy(d)
    on[, trt := 1]
    off[, trt := 0]
    pf <- glm(y ~ trt + ., data = d, family = "binomial")
    mean(predict(pf, newdata = on, type = "response") -
           predict(pf, newdata = off, type = "response"))
  })
  unlist(out)
}

#' Estimate the ATE using TMLE with main-effects GLMs
#' 
#' @param data Data created from \code{sample_dgp}.
ptmle <- function(data) {
  out <- lapply(data, function(d) {
    nms_cnf <- grep("^cnf", names(d), value = TRUE)
    Qform <- paste0("Y~A+", paste(nms_cnf, collapse = "+"))
    gform <- paste0("A~", paste(nms_cnf, collapse = "+"))
    tmle::tmle(
      d$y, 
      d$trt, 
      d[, nms_cnf, with = FALSE], 
      family = "binomial",
      Qform = Qform, 
      gform = gform
    )$estimates$ATE$psi
  })
  unlist(out)
}

#' Estimate the ATE using TMLE with SuperLearner, no cross-fitting
#' 
#' @param data Data created from \code{sample_dgp}.
tmle <- function(data) {
  SL_lib <- c("SL.ranger", "SL.xgboost", "SL.glm", "SL.earth")
  out <- lapply(data, function(d) {
    nms_cnf <- grep("^cnf", names(d), value = TRUE)
    tmle::tmle(
      d$y, 
      d$trt, 
      d[, nms_cnf, with = FALSE], 
      family = "binomial",
      Q.SL.library = SL_lib,
      g.SL.library = SL_lib,
    )$estimates$ATE$psi
  })
  unlist(out)
}

#' Estimate the ATE using TMLE with SuperLearner and cross-fitting
#' 
#' @param data Data created from \code{sample_dgp}.
cvtmle <- function(data) {
  SL_lib <- c("SL.ranger", "SL.xgboost", "SL.glm", "SL.earth")
  out <- lapply(data, function(d) {
    n <- nrow(d)
    if (n > 1000) {
      folds <- 2
    } else {
      folds <- 10
    }
    on <- lmtp::lmtp_tmle(
      as.data.frame(d), "trt", "y", 
      grep("^cnf", names(d), value = TRUE),
      shift = lmtp::static_binary_on, 
      learners_outcome = SL_lib, 
      learners_trt = SL_lib
    )
    off <- lmtp::lmtp_tmle(
      as.data.frame(d), "trt", "y", 
      grep("^cnf", names(d), value = TRUE),
      shift = lmtp::static_binary_off,
      learners_outcome = SL_lib, 
      learners_trt = SL_lib
    )
    lmtp::lmtp_contrast(on, ref = off)$vals$theta
  })
  unlist(out)
}
