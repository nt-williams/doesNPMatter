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
#' @param mu 
#' @param sigma 
#' @param randomized Simulate an randomized controlled trial (i.e., probability of treatment is 0.5)?
#'
#' @return A list of the true value, the result from parametric G-computation, 
#'   and the result from TMLE.
#' 
#' @author Nicholas Williams and Iván Díaz
bias <- function(context = c("binary", "ordinal", "tte"), seed, n, 
                 reps, size, rho, binary_cnf, cont_cnf, 
                 mu = NULL, sigma = NULL, randomized = FALSE) {
  set.seed(seed)
  data <- gendata(n, reps, size, rho, binary_cnf, 
                  cont_cnf, mu, sigma, randomized)
  switch(match.arg(context), 
         binary = bias_binary(data), 
         ordinal = bias_ordinal(data), 
         tte = bias_tte(data))
}

bias_binary <- function(data) {
  out <- lapply(data, function(d) {
    Y <- d$outcome
    A <- d$trt
    cnf <- grep("^cnf", names(d), value = TRUE)
    W <- as.matrix(d[, cnf, with = FALSE])
    
    on <- copy(d) # not factoring the continuous variable for now.
    off <- copy(d)
    on[, trt := 1]
    off[, trt := 0]
    
    ncnf <- length(cnf) + 2
    tform <- as.formula(paste0("outcome~(.)^", ncnf))
    pform <- as.formula(paste0("outcome~trt*(", paste(cnf, collapse = "+"), ")"))
    
    tf <- lm(tform, d = d)
    true <- mean(predict(tf, newd = on) - predict(tf, newd = off))
    
    pf <- glm(pform, family = "binomial", d = d)
    param <- mean(predict(pf, newd = on, type = "response") - 
                    predict(pf, newd = off, type = "response"))
    
    tml <- tmle(d)
    list(truth = true, param = param, tmle = tml)
  })
  if (length(out) == 1) {
    return(out[[1]])
  }
  out
}

bias_ordinal <- function(data) {
  out <- lapply(data, function(d) {
    Y <- d$outcome
    A <- d$trt
    cnf <- grep("^cnf", names(d), value = TRUE)
    cols <- grep("[^trt]", names(d), value = TRUE)
    W <- as.data.frame(d[, cnf, with = FALSE])
    
    on <- copy(d) # not factoring the continuous variable for now.
    off <- copy(d)
    on[, trt := 1]
    off[, trt := 0]
    
    ncnf <- length(cnf) + 2
    tform <- as.formula(paste0("factor(outcome, ordered = TRUE)~(", paste(cnf, collapse = "+"), ")^", ncnf))
    
    # calculation of the truth
    tf1 <- polr(tform, d = d[trt == 1, ]) # need to ask why we don't also stratify when calculating g-comp
    tf0 <- polr(tform, d = d[trt == 0, ])
    
    on_pred <- predict(tf1, newd = on, type = 'prob')
    on_prob <- cumsum(colMeans(on_pred))[-ncol(on_pred)]
    
    off_pred <- predict(tf0, newd = off, type = 'prob')
    off_prob <- cumsum(colMeans(off_pred))[-ncol(off_pred)]
    
    true <- mean(qlogis(on_prob) - qlogis(off_prob))
    
    # parametric calculation
    pf <- polr(factor(outcome, ordered = TRUE) ~ ., d = d)
    
    on_pred <- predict(pf, newd = on, type = 'prob')
    on_prob <- cumsum(colMeans(on_pred))[-ncol(on_pred)]
    
    off_pred <- predict(pf, newd = off, type = 'prob')
    off_prob <- cumsum(colMeans(off_pred))[-ncol(off_pred)]
    
    param <- mean(qlogis(on_prob) - qlogis(off_prob))
    
    # tmle calculation
    tml <- drord(out = d$outcome, treat = d$trt,
                 covar = d[, cnf, with = FALSE],
                 treat_form = ".",
                 out_form = ".",
                 out_model = 'polr',
                 ci = 'wald', 
                 est_dist = FALSE)
    
    list(truth = true, param = param, tmle = tml$log_odds$est[3])
  })
  
  if (length(out) == 1) {
    return(out[[1]])
  }
  out
}

bias_tte <- function(data) {
  out <- lapply(data, function(d) {
    
  })
  
  if (length(out) == 1) {
    return(out[[1]])
  }
  out
}
