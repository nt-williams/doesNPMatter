# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

box::use(data.table[...], ggplot2[...])

ao10_0 <- readRDS("./data/res/binary_asymp_10_0_FALSE_FALSE_FALSE.rds")
f1o10_0 <- readRDS("./data/res/binary_finite1_10_0_FALSE_FALSE_FALSE.rds")
f2o10_0 <- readRDS("./data/res/binary_finite2_10_0_FALSE_FALSE_FALSE.rds")
f3o10_0 <- readRDS("./data/res/binary_finite3_10_0_FALSE_FALSE_FALSE.rds")

aop10_0 <- readRDS("./data/res/binary_asymp_10_0_FALSE_TRUE_FALSE.rds")
f1op10_0 <- readRDS("./data/res/binary_finite1_10_0_FALSE_TRUE_FALSE.rds")
f2op10_0 <- readRDS("./data/res/binary_finite2_10_0_FALSE_TRUE_FALSE.rds")
f3op10_0 <- readRDS("./data/res/binary_finite3_10_0_FALSE_TRUE_FALSE.rds")

aoc10_0 <- readRDS("./data/res/binary_asymp_10_0_FALSE_FALSE_TRUE.rds")
f1oc10_0 <- readRDS("./data/res/binary_finite1_10_0_FALSE_FALSE_TRUE.rds")
f2oc10_0 <- readRDS("./data/res/binary_finite2_10_0_FALSE_FALSE_TRUE.rds")
f3oc10_0 <- readRDS("./data/res/binary_finite3_10_0_FALSE_FALSE_TRUE.rds")

for (dt in list(aop10_0, f1op10_0, f2op10_0, f3op10_0)) {
  setnames(dt, c("tmle", "tmle_bias", "tmle_mse", "tmle_estimates"), 
           c("tmlep", "tmlep_bias", "tmlep_mse", "tmlep_estimates"), 
           skip_absent = TRUE)
}

for (dt in list(aoc10_0, f1oc10_0, f2oc10_0, f3oc10_0)) {
  setnames(dt, c("tmle", "tmle_bias", "tmle_mse", "tmle_estimates"), 
           c("tmlec", "tmlec_bias", "tmlec_mse", "tmlec_estimates"), 
           skip_absent = TRUE)
}

ao10_0 <- merge(ao10_0, aop10_0[, .(truth, tmlep, tmlep_bias)], by = "truth", all = TRUE)
f1o10_0 <- merge(f1o10_0, f1op10_0[, .(truth, tmlep, tmlep_bias, tmlep_mse)], by = "truth", all = TRUE)
f1o10_0 <- merge(f1o10_0, f1oc10_0[, .(truth, tmlec, tmlec_bias, tmlec_mse)], by = "truth", all = TRUE)
f2o10_0 <- merge(f2o10_0, f2op10_0[, .(truth, tmlep, tmlep_bias, tmlep_mse)], by = "truth", all = TRUE)
f2o10_0 <- merge(f2o10_0, f2oc10_0[, .(truth, tmlec, tmlec_bias, tmlec_mse)], by = "truth", all = TRUE)
f3o10_0 <- merge(f3o10_0, f3op10_0[, .(truth, tmlep, tmlep_bias, tmlep_mse)], by = "truth", all = TRUE)
f3o10_0 <- merge(f3o10_0, f3oc10_0[, .(truth, tmlec, tmlec_bias, tmlec_mse)], by = "truth", all = TRUE)

o10_0 <- rbindlist(list(`100000` = ao10_0, `2500` = f1o10_0, `500` = f2o10_0, `250` = f3o10_0), 
                   fill = TRUE, id = "n")

o10_0[, n := as.numeric(n)]
o10_0[, `:=`(rel_bias_tmle = abs(tmle - truth) / max(abs(tmle), abs(truth), na.rm = TRUE), 
             rel_bias_tmlep = abs(tmlep - truth) / max(abs(tmlep), abs(truth), na.rm = TRUE), 
             rel_bias_param = abs(param - truth) / max(abs(param), abs(truth), na.rm = TRUE), 
             rel_bias_tmlec = abs(tmlec - truth) / max(abs(tmlec), abs(truth), na.rm = TRUE))]

ggplot(o10_0) +
  geom_line(aes(x = abs(tmle_bias), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
            alpha = 0.5) +
  geom_line(aes(x = abs(param_bias), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
            alpha = 0.5) + 
  geom_line(aes(x = abs(tmlep_bias), y = 1 - ..y.., color = "darkgreen"), stat = 'ecdf', 
            alpha = 0.5) + 
  geom_line(aes(x = abs(tmlec_bias), y = 1 - ..y.., color = "orange"), stat = 'ecdf', 
            alpha = 0.5) +
  scale_x_continuous(expand = c(0.025, 0), limits = c(0, 0.075)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  facet_grid(cols = vars(n)) + 
  labs(x = "|est - true| / max(|est|, |true|)",
       y = NULL, 
       color = NULL) + 
  scale_color_identity(breaks = c("blue", "red", "darkgreen", "orange"),
                       labels = c("TMLE+DA", "G-comp.", "TMLE+GLM", "CV-TMLE+DA"),
                       guide = "legend")
