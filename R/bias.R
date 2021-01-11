bias <- function(context, seed, n, size, rho, binary_cnf, cont_cnf, 
                 mu = NULL, sigma = NULL, randomized = FALSE) {
  set.seed(seed)
  data <- gendata(n, size, rho, binary_cnf, cont_cnf, 
                  mu, sigma, randomized)
  switch(context, 
         binary ~ bias_binary(data), 
         ordinal ~ bias_ordinal(data), 
         tte ~ bias_tte(data))
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
  
}

bias_tte <- function(data) {
  
}
