tmle2 <- function(data) {
  # f <- reformulate(c("t", grep("^x_", names(data), value = TRUE)), "y")
  # y_fit <- dbarts::bart2(formula = f, data = data, n.samples = 250L, n.burn = 250L, 
  #                        n.chains = 5L, verbose = FALSE, keepTrees = TRUE)
  t_fit <- SuperLearner::SuperLearner(Y = data$t, 
                                      X = as.data.frame(data)[, grep("^x_", names(data), value = TRUE)], 
                                      family = binomial(), 
                                      SL.library = c("SL.glm", "SL.lightgbm", "SL.bart2", "SL.earth"), 
                                      env = environment(SuperLearner::SuperLearner))

  g <- predict(t_fit, data)$pred[, 1]
  
  y_fit <- SuperLearner::SuperLearner(Y = data$y, 
                                      X = as.data.frame(data)[, c("t", grep("^x_", names(data), value = TRUE))], 
                                      family = binomial(), 
                                      SL.library = c("SL.glm", "SL.lightgbm", "SL.bart2", "SL.earth"), 
                                      env = environment(SuperLearner::SuperLearner))
  
  d1 <- data.table::copy(data)
  d0 <- data.table::copy(data)
  d1[, t := 1]
  d0[, t := 0]
  
  Q_A <- predict(y_fit, as.data.frame(data)[, c("t", grep("^x_", names(data), value = TRUE))])$pred[, 1]
  Q_1 <- predict(y_fit, as.data.frame(d1)[, c("t", grep("^x_", names(data), value = TRUE))])$pred[, 1]
  Q_0 <- predict(y_fit, as.data.frame(d0)[, c("t", grep("^x_", names(data), value = TRUE))])$pred[, 1]
  
  # Q_A <- fitted(y_fit)
  # Q_1 <- predict_bart(y_fit, d1)
  # Q_0 <- predict_bart(y_fit, d0)
  
  # f <- reformulate(grep("^x_", names(data), value = TRUE), "t")
  # t_fit <- dbarts::bart2(formula = f, data = data, n.samples = 250L, n.burn = 250L, 
  #                        n.chains = 5L, verbose = FALSE, keepTrees = TRUE)
  # g <- fitted(t_fit)


  H_1 <- (1 / g)
  H_0 <- -1 / (1 - g)
  H_A <- (data$t * H_1) + ((1 - data$t) * H_0)
  
  tmle_data <- data.frame(y = data$y, 
                          Q_A = Q_A, 
                          H_A = H_A)
  
  fluc <- glm(y ~ -1 + offset(qlogis(Q_A)) + H_A, data = tmle_data, family = binomial)
  eps <- coef(fluc)
  
  Q_1_eps <- plogis(qlogis(Q_1) + eps*H_1)
  Q_0_eps <- plogis(qlogis(Q_0) + eps*H_0)
  mean(Q_1_eps - Q_0_eps)
}

predict_bart <- function(object, newdata) {
  mat <- predict(object, newdata, type = "response")
  apply(mat, 2, mean)
}
