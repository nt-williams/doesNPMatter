#' Simulate the bias of TMLE and parametric G-computation.
#'
#' @param context Outcome type, valid options are "binary", "ordinal", and "tte."
#' @param seed Seed for reproducibility.
#' @param n The sample size.
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
                 size, rho, binary_cnf, cont_cnf, 
                 mu = NULL, sigma = NULL, randomized = FALSE) {
  set.seed(seed)
  data <- gendata(n, size, rho, binary_cnf, cont_cnf, 
                  mu, sigma, randomized)
  switch(match.arg(context), 
         binary = bias_binary(data), 
         ordinal = bias_ordinal(data), 
         tte = bias_tte(data))
}

bias_binary <- function(data) {
  Y <- data$outcome
  A <- data$trt
  cnf <- grep("^cnf", names(data), value = TRUE)
  W <- as.matrix(data[, cnf, with = FALSE])
  
  on <- copy(data) # not factoring the continuous variable for now.
  off <- copy(data)
  on[, trt := 1]
  off[, trt := 0]
  
  ncnf <- length(cnf) + 2
  tform <- as.formula(paste0("outcome~(.)^", ncnf))
  pform <- as.formula(paste0("outcome~trt*(", paste(cnf, collapse = "+"), ")"))
  
  tf <- lm(tform, data = data)
  true <- mean(predict(tf, newdata = on) - predict(tf, newdata = off))
  
  pf <- glm(pform, family = "binomial", data = data)
  param <- mean(predict(pf, newdata = on, type = "response") - 
                  predict(pf, newdata = off, type = "response"))
  
  tml <- tmle(data)
  list(truth = true, param = param, tmle = tml)
}

bias_ordinal <- function(data) {
  Y <- data$outcome
  A <- data$trt
  cnf <- grep("^cnf", names(data), value = TRUE)
  cols <- grep("[^trt]", names(data), value = TRUE)
  W <- as.data.frame(data[, cnf, with = FALSE])
  
  on <- copy(data) # not factoring the continuous variable for now.
  off <- copy(data)
  on[, trt := 1]
  off[, trt := 0]
  
  ncnf <- length(cnf) + 2
  tform <- as.formula(paste0("factor(outcome, ordered = TRUE)~(.)^", ncnf))

  # calculation of the truth
  tf <- polr(tform, data = data)
  
  on_pred <- predict(tf, newdata = on, type = 'prob')
  on_prob <- cumsum(colMeans(on_pred))[-ncol(on_pred)]
  
  off_pred <- predict(tf, newdata = off, type = 'prob')
  off_prob <- cumsum(colMeans(off_pred))[-ncol(off_pred)]
  
  true <- mean(qlogis(on_prob) - qlogis(off_prob))
  
  # parametric calculation
  pf <- polr(factor(outcome, ordered = TRUE) ~ ., data = data)
  
  on_pred <- predict(pf, newdata = on, type = 'prob')
  on_prob <- cumsum(colMeans(on_pred))[-ncol(on_pred)]
  
  off_pred <- predict(pf, newdata = off, type = 'prob')
  off_prob <- cumsum(colMeans(off_pred))[-ncol(off_pred)]
  
  param <- mean(qlogis(on_prob) - qlogis(off_prob))
  
  # tmle calculation
  tml <- drord(out = data$outcome, treat = data$trt,
               covar = data[, cnf, with = FALSE],
               treat_form = "1",
               out_form = ".",
               out_model = 'polr',
               ci = 'wald')
  
  list(truth = true, param = param, tmle = tml$log_odds$est[3])
}

bias_tte <- function(data) {
  
}
