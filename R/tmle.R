bound <- function(x, p = 0.0001) {
  as.vector(pmax(pmin(x, 1 - p), p))
}

hyper_params <- list(
  ntrees = c(50, 100),
  sample_rate = c(0.7, 0.8, 0.9, 1.0),
  col_sample_rate = c(0.7, 0.8, 0.9, 1.0),
  learn_rate = 0.5,
  max_depth = c(8, 9, 10)
)

set.seed(1)
pg <- expand.grid(hyper_params)
pg <- lapply((as.list(pg[sample(nrow(pg), 4),])),
             function(x) {
               attributes(x) <- NULL
               x
             })

xgb1 <- create.Learner("SL.xgboost", detailed_names = TRUE,
                       tune = lapply(pg, function(x) x[1]))
xgb2 <- create.Learner("SL.xgboost", detailed_names = TRUE,
                       tune = lapply(pg, function(x) x[2]))
xgb3 <- create.Learner("SL.xgboost", detailed_names = TRUE,
                       tune = lapply(pg, function(x) x[3]))
xgb4 <- create.Learner("SL.xgboost", detailed_names = TRUE,
                       tune = lapply(pg, function(x) x[4]))

SL_lib <- c("SL.glm.interaction", "SL.gam", "SL.earth",
            xgb1$names, xgb2$names, xgb3$names, xgb4$names)
SL_lib_param <- c("SL.glm.interaction")

fit_otcr <- function(data, parametric = FALSE) {
  cnf <- grep("^cnf", names(data), value = TRUE)
  n <- nrow(data)
  V <- ifelse(n < 1000, 10L, 2L)

  if (parametric) {
    SL <- SL_lib_param
  } else {
    SL <- SL_lib
  }

  sl <- SuperLearner(
    Y = data$outcome,
    X = as.data.frame(data[, c(cnf, "trt"), with = FALSE]),
    SL.library = SL,
    family = binomial(),
    method = "method.NNloglik",
    cvControl = SuperLearner.CV.control(V = V)
  )

  y <- as.data.table(data)$outcome
  y.org <- bound(predict(sl, data[, c(cnf, "trt"), with = FALSE], onlySL = TRUE)$pred)

  data[, "trt"] <- 1
  y.on <- bound(predict(sl, data[, c(cnf, "trt"), with = FALSE], onlySL = TRUE)$pred)

  data[, "trt"] <- 0
  y.off <- bound(predict(sl, data[, c(cnf, "trt"), with = FALSE], onlySL = TRUE)$pred)

  list(Y = y,
       Y.org = y.org,
       Y.on  = y.on,
       Y.off = y.off)
}

fit_prop <- function(data, parametric = FALSE) {
  cnf <- grep("^cnf", names(data), value = TRUE)
  n <- nrow(data)
  a <- as.data.table(data)$trt
  V <- ifelse(n < 1000, 10L, 2L)

  if (parametric) {
    SL <- SL_lib_param
  } else {
    SL <- SL_lib
  }

  sl <- SuperLearner(
    Y = data$trt,
    X = as.data.frame(data[, cnf, with = FALSE]),
    SL.library = SL,
    family = binomial(),
    cvControl = SuperLearner.CV.control(V = V)
  )

  prob <- bound(predict(sl, data[, cnf, with = FALSE], onlySL = TRUE)$pred)
  list(prob.on  = prob,
       prob.off = 1 - prob,
       clever = (a == 1)*(1 / prob) + (a == 0)*(1 / (1 - prob)))
}

fit_tilt <- function(otcr, prop) {
  new <- data.table(Y = otcr$Y, clever = prop$clever, Y.org = otcr$Y.org)
  coef(glm2::glm2(Y ~ -1 + clever + offset(qlogis(Y.org)), data = new, family = "binomial"))
}

tmle <- function(data, parametric) {
  otcr <- fit_otcr(data, parametric)
  prop <- fit_prop(data, parametric)
  eps <- fit_tilt(otcr, prop)
  mean(plogis(qlogis(otcr$Y.on) + eps*prop$prob.on)) -
    mean(plogis(qlogis(otcr$Y.off) + eps*prop$prob.off))
}
