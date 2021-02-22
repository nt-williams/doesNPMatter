library(lmtp)
library(sl3)
library(progressr)

devtools::load_all()

with_progress({
  b <- bias("binary", 435654, 1e5, 1, 2, 4, 0)
})

set.seed(435654)
meta <- gendata(1e5, 1, 2, 4, 0, seed = 435654)
.data <- meta$data[[1]]

tmle::tmle(.data$outcome, .data$trt, .data[, .(cnf1, cnf2, cnf3, cnf4)], 
           Q.SL.library = c("SL.glm.interaction"), 
           g.SL.library = c("SL.glm.interaction"))

with_progress({
  t1 <- lmtp_tmle(as.data.frame(.data), "trt", "outcome", grep("^cnf", names(.data), value = TRUE), 
                  outcome_type = "binomial", 
                  shift = static_binary_on, folds = 2, .SL_folds = 2,
                  learners_outcome = make_learner_stack(Lrnr_glm_fast), 
                  learners_trt = make_learner_stack(Lrnr_glm_fast))
})

t0 <- lmtp_tmle(as.data.frame(.data), "trt", "outcome", paste0("cnf", 1:4), 
                outcome_type = "binomial", 
                shift = static_binary_off, folds = 2, .SL_folds = 2,
                learners_outcome = make_learner_stack(Lrnr_glm_fast), 
                learners_trt = make_learner_stack(Lrnr_glm_fast))

lmtp_contrast(t1, ref = t0)
