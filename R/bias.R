#' Simulate the bias of TMLE and parametric G-computation.
#'
#' @param context Outcome type, valid options are "binary", "ordinal", and "tte."
#' @param seed Seed for reproducibility.
#' @param n The sample size.
#' @param reps How many datasets should be generated from the DGM.
#' @param size Number of categories in Y, ignored if context is "binary".
#' @param binary_cnf Number of binary confounders.
#' @param cont_cnf Size of a continuous confounder.
#' @param randomized Simulate an randomized controlled trial (i.e., probability of treatment is 0.5)?
#' @param parametric Estimate TMLE using just GLM? Only used with `context = "binary"`.
#'
#' @return A list of the true value, the result from parametric G-computation, 
#'   and the result from TMLE.
#' 
#' @author Nicholas Williams and Iván Díaz
bias <- function(context = c("binary", "ordinal", "tte"), seed, n, 
                 reps, size, binary_cnf, cont_cnf, 
                 randomized = FALSE, parametric = FALSE) {
  dist <- create_dist(binary_cnf, cont_cnf, if (match.arg(context) == "binary") 2 else size, randomized, seed)
  data <- gendata(dist, n, reps, if (match.arg(context) == "tte") TRUE else FALSE)
  truth <- 
    switch(match.arg(context), 
           binary = truth_binary(dist), 
           ordinal = truth_ordinal(dist), 
           tte = truth_tte(dist))
  vnorm <- {
    if (cont_cnf > 0 && match.arg(context) == "binary")
      variation_norm(dist)
    else 
      NULL
  }
  estims <- 
    switch(match.arg(context), 
           binary = binary(data, parametric || randomized), 
           ordinal = ordinal(data), 
           tte = bias_tte(data, randomized))
  c(list(dist = dist, pos = positivity(dist), truth = truth), vnorm, estims)
}

binary <- function(data, parametric) {
  list(param = binary_param(data),
       tmle = binary_tmle(data, parametric))
}

binary_param <- function(data) {
  out <- lapply(data, function(d) {
    on <- copy(d)
    off <- copy(d)
    on[, trt := 1]
    off[, trt := 0]
    pf <- glm(outcome ~ trt*(.), data = d, family = "binomial")
    mean(predict(pf, newdata = on, type = "response") - 
           predict(pf, newdata = off, type = "response"))
  })
  unlist(out)
}

binary_tmle <- function(data, parametric) {
  out <- lapply(data, function(d) tmle(d, parametric = parametric))
  unlist(out)
}

ordinal <- function(data) {
  list(param = ordinal_param(data),
       tmle = ordinal_tmle(data))
}

ordinal_param <- function(data) {
  out <- lapply(data, function(d) {
    on <- copy(d)
    off <- copy(d)
    on[, trt := 1]
    off[, trt := 0]
    d[, outcome := factor(outcome, ordered = TRUE)]
    pf <- ordinal::clm(outcome ~ trt*(.), data = d)
    on_pred <- predict(pf, newdata = on[, -"outcome", with = FALSE], type = "prob")$fit
    on_prob <- cumsum(colMeans(on_pred))[-ncol(on_pred)]
    off_pred <- predict(pf, newdata = off[, -"outcome", with = FALSE], type = "prob")$fit
    off_prob <- cumsum(colMeans(off_pred))[-ncol(off_pred)]
    mean(qlogis(on_prob) - qlogis(off_prob))
  })
  unlist(out)
}

ordinal_tmle <- function(data) {
  out <- lapply(data, function(d) {
    Y <- d$outcome
    A <- d$trt
    cnf <- grep("^cnf", names(d), value = TRUE)
    W <- as.data.frame(d[, cnf, with = FALSE])
    drord(out = d$outcome, treat = d$trt,
          covar = d[, cnf, with = FALSE],
          treat_form = ".",
          out_form = ".",
          out_model = "clm",
          ci = 'wald', 
          est_dist = FALSE)$log_odds$est[3]
  })
  unlist(out)
}

tte <- function(data, randomized) {
  list(param = tte_param(data),
       tmle = tte_tmle(data, randomized))
}

tte_param <- function(data) {
  out <- list()
  for (i in 1:length(data)) {
    use <- copy(data[[i]])
    use[, outcome := NULL]
    on <- copy(use)
    off <- copy(use)
    on[, trt := 1]
    off[, trt := 0]
    pf <- survival::coxph(survival::Surv(time, status) ~ trt*(.), data = use)
    out[[i]] <- mean(predict_coxph(pf, 12, on)) - mean(predict_coxph(pf, 12, off))
  } 
  unlist(out)
}

tte_tmle <- function(data, randomized) {
  lrnrs <- {
    if (randomized)
      "SL.glm.interaction"
    else 
      c("SL.glm", "SL.gam", "SL.earth")
  }
  cnf <- grep("^cnf", names(data[[1]]), value = TRUE)
  out <- lapply(data, function(x) {
    x[, time := time + 1]
    tml <- survtmle::survtmle(
      ftime = x$time,
      ftype = x$status,
      trt = x$trt,
      adjustVars = x[, ..cnf],
      SL.trt = lrnrs,
      SL.ftime = lrnrs,
      SL.ctime = lrnrs,
      method = "mean",
      t0 = 13,
      returnModels = FALSE
    )
    as.vector((1 - tml$est[2, 1]) - (1 - tml$est[1, 1]))
  })
  unlist(out)
}
