#' Simulate the bias of TMLE and parametric G-computation.
#'
#' @param context Outcome type, valid options are "binary", "ordinal", and "tte."
#' @param seed Seed for reproducibility.
#' @param n The sample size.
#' @param reps How many datasets should be generated from the DGM.
#' @param size Number of categories in Y, default is 2.
#' @param rho Logistic distribution informative prior, default is 0.
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
                 reps, size, rho, binary_cnf, cont_cnf, 
                 randomized = FALSE, parametric = FALSE) {
  set.seed(seed)
  data <- gendata(n, reps, size, rho, binary_cnf, cont_cnf, randomized, seed)
  if (n == 1e5 && reps == 1) {
    asymp <- data[[1]]
  } else {
    set.seed(seed)
    asymp <- gendata(1e5, 1, size, rho, binary_cnf, cont_cnf, randomized, seed)[[1]]
  }

  switch(match.arg(context), 
         binary = bias_binary(data, asymp, cont_cnf, parametric), 
         ordinal = bias_ordinal(data, asymp, cont_cnf), 
         tte = bias_tte(data))
}

bias_binary <- function(data, asymp, cont_cnf, parametric) {
  list(truth = bias_binary_truth(asymp, cont_cnf), 
       param = bias_binary_param(data),
       tmle = bias_binary_tmle(data, parametric))
}

bias_binary_truth <- function(data, cont_cnf) {
  cnf <- grep("^cnf", names(data), value = TRUE)
  ncnf <- length(cnf) + 2
  on <- copy(data)
  off <- copy(data)
  on[, trt := 1]
  off[, trt := 0]
  if (cont_cnf > 0) {
    tform <- as.formula(paste0(paste(c("outcome~trt*(as.factor(cnf1)", cnf[-1]), collapse = "+"), ")^", ncnf))
  } else {
    tform <- as.formula(paste0("outcome~(.)^", ncnf))
  }
  tf <- suppressWarnings(speedglm::speedglm(tform, data = data))
  mean(predict(tf, newdata = on) - predict(tf, newdata = off))
}

bias_binary_param <- function(data) {
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

bias_binary_tmle <- function(data, parametric) {
  out <- lapply(data, function(d) tmle(d, parametric = parametric))
  unlist(out)
}

bias_ordinal <- function(data, asymp, cont_cnf) {
  list(truth = bias_ordinal_truth(asymp, cont_cnf), 
       param = bias_ordinal_param(data),
       tmle = bias_ordinal_tmle(data))
}

bias_ordinal_truth <- function(data, cont_cnf) {
  cnf <- grep("^cnf", names(data), value = TRUE)
  ncnf <- length(cnf) + 2
  on <- copy(data)
  off <- copy(data)
  on[, trt := 1]
  off[, trt := 0]
  data[, outcome := factor(outcome, ordered = TRUE)]
  if (cont_cnf > 0) {
    tform <- as.formula(paste0(paste(c("outcome~trt*(as.factor(cnf1)", cnf[-1]), collapse = "+"), ")^", ncnf))
  } else {
    tform <- as.formula(paste0("outcome~(.)^", ncnf))
  }
  tf1 <- ordinal::clm(tform, data = data[trt == 1, ])
  tf0 <- ordinal::clm(tform, data = data[trt == 0, ])
  on_pred <- predict(tf1, newdata = on[, -"outcome", with = FALSE], type = "prob")$fit
  on_prob <- cumsum(colMeans(on_pred))[-ncol(on_pred)]
  off_pred <- predict(tf0, newdata = off[, -"outcome", with = FALSE], type = 'prob')$fit
  off_prob <- cumsum(colMeans(off_pred))[-ncol(off_pred)]
  mean(qlogis(on_prob) - qlogis(off_prob))
}

bias_ordinal_param <- function(data) {
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

bias_ordinal_tmle <- function(data) {
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
