tmle_mlr3 <- function(data, folds = 1) {
  covar <- grep("^x", names(data), value = TRUE)
  
  folded <- origami::make_folds(data, V = folds)
  if (folds == 1) {
    folded[[1]]$training_set <- folded[[1]]$validation_set
  }
  
  H_1 <- matrix(nrow = nrow(data), ncol = 1)
  H_0 <- matrix(nrow = nrow(data), ncol = 1)
  H_A <- matrix(nrow = nrow(data), ncol = 1)
  
  Q_1_eps <- matrix(nrow = nrow(data), ncol = 1)
  Q_0_eps <- matrix(nrow = nrow(data), ncol = 1)
  Q_A_eps <- matrix(nrow = nrow(data), ncol = 1)
  
  for (i in 1:folds) {
    train <- data[folded[[i]]$training_set, ]
    valid <- data[folded[[i]]$validation_set, ]
    
    valid_t_1 <- data.table::copy(valid)
    valid_t_0 <- data.table::copy(valid)
    valid_t_1$t <- 1
    valid_t_0$t <- 0
    
    # creating a resampling task
    resampling <- rsmp("cv", folds = 5)
    
    # creating a mlr3 task for the propensity score
    task_propensity <- as_task_classif(train[, !"y", with = FALSE], 
                                       target = "t", 
                                       id = "propensity")
    
    # initiating the base learners
    bart_learner <- lrn("classif.bart", predict_type = "prob", 
                        verbose = FALSE, nskip = 250L, ntree = 75)
    lgb_learner <- lrn("classif.lightgbm", predict_type = "prob", 
                       min_data_in_leaf = 2, num_iterations = 1000, 
                       max_depth = -1, learning_rate = 0.1)
    glm_learner <- lrn("classif.log_reg", predict_type = "prob")
    earth_learner <- lrn("classif.earth", predict_type = "prob")
    
    # get CV predictions for training the super learner
    rr_bart <- resample(task_propensity, bart_learner, resampling)
    rr_lgb <- resample(task_propensity, lgb_learner, resampling)
    rr_glm <- resample(task_propensity, glm_learner, resampling)
    rr_earth <- resample(task_propensity, earth_learner, resampling)
    
    # train base learners on the full data
    bart_learner$train(task_propensity)
    lgb_learner$train(task_propensity)
    glm_learner$train(task_propensity)
    earth_learner$train(task_propensity)
    
    sl_propensity <- compute_super_learner_weights(rr_bart, rr_lgb, rr_glm, rr_earth,
                                                   y = train$t)
    
    # super learner propensity scores
    g_train <- mlr3_ensemble_predict(bart_learner, lgb_learner, glm_learner, earth_learner,
                               weights = sl_propensity, 
                               newdata = train[, covar, with = FALSE])
    
    g <- mlr3_ensemble_predict(bart_learner, lgb_learner, glm_learner, earth_learner,
                               weights = sl_propensity, 
                               newdata = valid[, covar, with = FALSE])
    
    # creating a mlr3 task for the outcome mechanism
    task_outcome <- as_task_classif(train, 
                                    target = "y", 
                                    id = "outcome_mech")
    
    # get CV predictions for training the super learner
    rr_bart <- resample(task_outcome, bart_learner, resampling)
    rr_lgb <- resample(task_outcome, lgb_learner, resampling)
    rr_glm <- resample(task_outcome, glm_learner, resampling)
    rr_earth <- resample(task_outcome, earth_learner, resampling)
    
    # train base learners on the full data
    bart_learner$train(task_outcome)
    lgb_learner$train(task_outcome)
    glm_learner$train(task_outcome)
    earth_learner$train(task_outcome)
    
    sl_outcome <- compute_super_learner_weights(rr_bart, rr_lgb, rr_glm, rr_earth,
                                                y = train$y)
    
    Q_A_train <- mlr3_ensemble_predict(bart_learner, lgb_learner, glm_learner, earth_learner,
                                       weights = sl_outcome, 
                                       newdata = train[, c(covar, "t"), with = FALSE])
    
    Q_A <- mlr3_ensemble_predict(bart_learner, lgb_learner, glm_learner, earth_learner,
                                 weights = sl_outcome, 
                                 newdata = valid[, c(covar, "t"), with = FALSE])
    Q_1 <- mlr3_ensemble_predict(bart_learner, lgb_learner, glm_learner, earth_learner,
                                 weights = sl_outcome, 
                                 newdata = valid_t_1[, c(covar, "t"), with = FALSE])
    Q_0 <- mlr3_ensemble_predict(bart_learner, lgb_learner, glm_learner, earth_learner,
                                 weights = sl_outcome, 
                                 newdata = valid_t_0[, c(covar, "t"), with = FALSE])
    
    # calculate tmle
    H_1_train <- (1 / g_train)
    H_0_train <- -1 / (1 - g_train)
    H_A_train <- (train$t * H_1_train) + ((1 - train$t) * H_0_train)
    
    tmle_data <- data.frame(y = train$y, 
                            Q_A = Q_A_train, 
                            H_A = H_A_train)
    
    fluc <- glm(y ~ -1 + offset(qlogis(Q_A)) + H_A, data = tmle_data, family = binomial)
    eps <- coef(fluc)
    
    H_1[folded[[i]]$validation_set, 1] <- (1 / g)
    H_0[folded[[i]]$validation_set, 1] <- -1 / (1 - g)
    H_A[folded[[i]]$validation_set, 1] <- (valid$t * H_1[folded[[i]]$validation_set, 1]) + 
      ((1 - valid$t) * H_0[folded[[i]]$validation_set, 1])

    Q_1_eps[folded[[i]]$validation_set, 1] <- plogis(qlogis(Q_1) + eps*H_1[folded[[i]]$validation_set, 1])
    Q_0_eps[folded[[i]]$validation_set, 1] <- plogis(qlogis(Q_0) + eps*H_0[folded[[i]]$validation_set, 1])
    Q_A_eps[folded[[i]]$validation_set, 1] <- plogis(qlogis(Q_A) + eps*H_A[folded[[i]]$validation_set, 1])
  }
  
  psi <- mean(Q_1_eps[, 1] - Q_0_eps[, 1])
  eic <- (data$y - Q_A_eps[, 1]) * H_A[, 1] + Q_1_eps[, 1] - Q_0_eps[, 1] - psi
  se <- sqrt(var(eic) / nrow(data))
  ci <- psi + c(-1, 1) * qnorm(0.975) * se
  
  list(psi = psi, 
       conf.low = ci[1], 
       conf.high = ci[2])
}

compute_super_learner_weights <- function(..., y) {
  learners <- list(...)
  x <- lapply(learners, 
              function(x) {
                data.table::as.data.table(x$prediction())[order(row_ids)]$prob.1
              })
  x <- matrix(Reduce(`c`, x), ncol = length(learners))
  ids <- unlist(lapply(learners, function(x) x$learner$id))
  SuperLearner::method.NNLS()$computeCoef(x, y, ids, FALSE, 1)
}

mlr3_ensemble_predict <- function(..., weights, newdata) {
  learners <- list(...)
  z <- lapply(learners, 
              function(x) x$predict_newdata(newdata)$prob[, 2])
  z <- matrix(Reduce(`c`, z), ncol = length(learners))
  SuperLearner::method.NNLS()$computePred(z, weights$coef)[, 1]
}
