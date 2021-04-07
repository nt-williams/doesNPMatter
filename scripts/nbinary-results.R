# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

library(data.table)
library(ggplot2)

devtools::load_all()

# observational
ao4_0 <- readRDS("./data/res/binary_asymp_obser_4_0.rds")
f1o4_0 <- readRDS("./data/res/binary_finite1_obser_4_0.rds")
f2o4_0 <- readRDS("./data/res/binary_finite2_obser_4_0.rds")
f3o4_0 <- readRDS("./data/res/binary_finite3_obser_4_0.rds")

aop4_0 <- readRDS("./data/res/binary_asymp_obserParam_4_0.rds")
f1op4_0 <- readRDS("./data/res/binary_finite1_obserParam_4_0.rds")
f2op4_0 <- readRDS("./data/res/binary_finite2_obserParam_4_0.rds")
f3op4_0 <- readRDS("./data/res/binary_finite3_obserParam_4_0.rds")

ao4_0 <- merge(ao4_0, aop4_0[, .(truth, tmlep = tmle, tmlep_bias = tmle_bias)], by = "truth")
f1o4_0 <- merge(f1o4_0, f1op4_0[, .(truth, tmlep = tmle, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")
f2o4_0 <- merge(f2o4_0, f2op4_0[, .(truth, tmlep = tmle, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")
f3o4_0 <- merge(f3o4_0, f3op4_0[, .(truth, tmlep = tmle, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")

o4_0 <- rbindlist(list(`100000` = ao4_0, `2500` = f1o4_0, 
                       `500` = f2o4_0, `250` = f3o4_0), 
                  fill = TRUE, id = "n")
o4_0[, n := as.numeric(n)]
o4_0[, `:=`(rel_bias_tmle = abs(tmle - truth) / max(abs(tmle), abs(truth)), 
            rel_bias_tmlep = abs(tmlep - truth) / max(abs(tmlep), abs(truth)), 
            rel_bias_param = abs(param - truth) / max(abs(param), abs(truth)))]

ao0_100 <- readRDS("./data/res/binary_asymp_obser_0_100.rds")
f1o0_100 <- readRDS("./data/res/binary_finite1_obser_0_100.rds")
f2o0_100 <- readRDS("./data/res/binary_finite2_obser_0_100.rds")
f3o0_100 <- readRDS("./data/res/binary_finite3_obser_0_100.rds")

aop0_100 <- readRDS("./data/res/binary_asymp_obserParam_0_100.rds")
f1op0_100 <- readRDS("./data/res/binary_finite1_obserParam_0_100.rds")
f2op0_100 <- readRDS("./data/res/binary_finite2_obserParam_0_100.rds")
f3op0_100 <- readRDS("./data/res/binary_finite3_obserParam_0_100.rds")

ao0_100 <- merge(ao0_100, aop0_100[, .(truth, tmlep = tmle, tmlep_bias = tmle_bias)], by = "truth")
f1o0_100 <- merge(f1o0_100, f1op0_100[, .(truth, tmlep = tmle, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")
f2o0_100 <- merge(f2o0_100, f2op0_100[, .(truth, tmlep = tmle, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")
f3o0_100 <- merge(f3o0_100, f3op0_100[, .(truth, tmlep = tmle, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")

o0_100 <- rbindlist(list(`100000` = ao0_100, `2500` = f1o0_100, 
                         `500` = f2o0_100, `250` = f3o0_100), 
                    fill = TRUE, id = "n")
o0_100[, n := as.numeric(n)]
o0_100[, `:=`(rel_bias_tmle = abs(tmle - truth) / max(abs(tmle), abs(truth)), 
            rel_bias_tmlep = abs(tmlep - truth) / max(abs(tmlep), abs(truth)), 
            rel_bias_param = abs(param - truth) / max(abs(param), abs(truth)))]

# RCT
ar4_0 <- readRDS("./data/res/binary_asymp_random_4_0.rds")
# f1r4_0 <- readRDS("./data/res/binary_finite1_random_4_0.rds")
f2r4_0 <- readRDS("./data/res/binary_finite2_random_4_0.rds")
f3r4_0 <- readRDS("./data/res/binary_finite3_random_4_0.rds")

r4_0 <- rbindlist(list(`100000` = ar4_0, # `2500` = f1r4_0, 
                       `500` = f2r4_0, `250` = f3r4_0), 
                  fill = TRUE, id = "n")
r4_0[, n := as.numeric(n)]

ar0_100 <- readRDS("./data/res/binary_asymp_random_0_100.rds")
f1r0_100 <- readRDS("./data/res/binary_finite1_random_0_100.rds")
f2r0_100 <- readRDS("./data/res/binary_finite2_random_0_100.rds")
f3r0_100 <- readRDS("./data/res/binary_finite3_random_0_100.rds")

r0_100 <- rbindlist(list(`100000` = ar0_100, `2500` = f1r0_100, 
                         `500` = f2r0_100, `250` = f3r0_100), 
                    fill = TRUE, id = "n")
r0_100[, n := as.numeric(n)]

# plots -------------------------------------------------------------------

ggplot(rbindlist(list("Scenario 1" = o0_100, "Scenario 2" = o4_0), 
                 fill = TRUE, id = "scen")) +
  geom_line(aes(x = abs(rel_bias_tmle), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
            alpha = 0.5) +
  geom_line(aes(x = abs(rel_bias_param), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
            alpha = 0.5) + 
  geom_line(aes(x = abs(rel_bias_tmlep), y = 1 - ..y.., color = "darkgreen"), stat = 'ecdf', 
            alpha = 0.5) + 
  scale_x_continuous(expand = c(0.025, 0), limits = c(0, 0.075)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  facet_grid(cols = vars(n), 
             rows = vars(scen)) + 
  labs(x = "|est - true| / max(|est|, |true|)",
       y = NULL, 
       color = NULL) + 
  scale_color_identity(breaks = c("blue", "red", "darkgreen"),
                       labels = c("TMLE+DA", "G-comp.", "TMLE+GLM"),
                       guide = "legend")

ggsave("./plots/bias_binary_obs.png", dpi = 600, width = 14, height = 5.5)


ggplot(rbindlist(
  list("Scenario 1" = o0_100, "Scenario 2" = o4_0), 
  fill = TRUE, id = "scen"
)[n != 1e5
  ][, `:=`(tmle_mse_scaled = tmle_mse * n, 
           tmlep_mse_scaled = tmlep_mse * n, 
           param_mse_scaled = param_mse * n)]) +
  geom_line(aes(x = abs(tmle_mse_scaled), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
            alpha = 0.5) +
  geom_line(aes(x = abs(param_mse_scaled), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
            alpha = 0.5) + 
  geom_line(aes(x = abs(tmlep_mse_scaled), y = 1 - ..y.., color = "darkgreen"), stat = 'ecdf', 
            alpha = 0.5) + 
  scale_x_continuous(expand = c(0.025, 0), limits = c(0, 5)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  facet_grid(cols = vars(n), 
             rows = vars(scen)) + 
  labs(x = expression(MSE %*% N),
       y = NULL, 
       color = NULL) + 
  scale_color_identity(breaks = c("blue", "red", "darkgreen"),
                       labels = c("TMLE+DA", "G-comp.", "TMLE+GLM"),
                       guide = "legend")

ggsave("./plots/mse_binary_obs.png", dpi = 600, width = 14, height = 5.5)

o0_100[, vn_cate_quart := 
         cut(vn_cate, 
             breaks = quantile(vn_cate, probs = seq(0, 1, by = 0.25), na.rm = TRUE), 
                                 include.lowest = TRUE, 
             labels = c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))]

ggplot(o0_100[!is.na(vn_cate_quart)]) +
  geom_line(aes(x = abs(rel_bias_tmle), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
            alpha = 0.5) +
  geom_line(aes(x = abs(rel_bias_param), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
            alpha = 0.5) + 
  geom_line(aes(x = abs(rel_bias_tmlep), y = 1 - ..y.., color = "darkgreen"), stat = 'ecdf', 
            alpha = 0.5) + 
  scale_x_continuous(expand = c(0.025, 0), limits = c(0, 0.075)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  facet_grid(cols = vars(vn_cate_quart),
             rows = vars(n)) + 
  labs(x = "|est - true| / max(|est|, |true|)",
       y = NULL, 
       color = NULL) + 
  scale_color_identity(breaks = c("blue", "red", "darkgreen"),
                       labels = c("TMLE+DA", "G-comp.", "TMLE+GLM"),
                       guide = "legend")

ggsave("./plots/bias_binary_vn_cate_obs.png", dpi = 600, width = 14, height = 10)

o0_100[, vn_max_trt := pmax(vn1, vn0, na.rm = TRUE)
       ][, vn_max_trt_quart := 
           cut(vn_max_trt, 
               breaks = quantile(vn_max_trt, probs = seq(0, 1, by = 0.25), na.rm = TRUE), 
               include.lowest = TRUE, 
               labels = c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))]

ggplot(o0_100[!is.na(vn_max_trt_quart)]) +
  geom_line(aes(x = abs(rel_bias_tmle), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
            alpha = 0.5) +
  geom_line(aes(x = abs(rel_bias_param), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
            alpha = 0.5) + 
  geom_line(aes(x = abs(rel_bias_tmlep), y = 1 - ..y.., color = "darkgreen"), stat = 'ecdf', 
            alpha = 0.5) + 
  scale_x_continuous(expand = c(0.025, 0), limits = c(0, 0.075)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  facet_grid(cols = vars(vn_max_trt_quart), 
             rows = vars(n)) + 
  labs(x = "|est - true| / max(|est|, |true|)",
       y = NULL, 
       color = NULL) + 
  scale_color_identity(breaks = c("blue", "red", "darkgreen"),
                       labels = c("TMLE+DA", "G-comp.", "TMLE+GLM"),
                       guide = "legend")

ggsave("./plots/bias_binary_max_vn_trt_obs.png", dpi = 600, width = 14, height = 10)

o0_100[, pos_quart := 
          cut(pos, 
              breaks = quantile(
                pos, probs = seq(0, 1, by = 0.25), na.rm = TRUE
                ), 
              include.lowest = TRUE, 
              labels = c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))]

o4_0[, pos_quart := 
          cut(pos, 
              breaks = quantile(
                pos, probs = seq(0, 1, by = 0.25), na.rm = TRUE
              ), 
              include.lowest = TRUE, 
              labels = c("1st Quartile", "2nd Quartile", "3rd Quartile", "4th Quartile"))]

ggplot(o0_100[!is.na(pos_quart)]) +
  geom_line(aes(x = abs(rel_bias_tmle), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
            alpha = 0.5) +
  geom_line(aes(x = abs(rel_bias_param), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
            alpha = 0.5) + 
  geom_line(aes(x = abs(rel_bias_tmlep), y = 1 - ..y.., color = "darkgreen"), stat = 'ecdf', 
            alpha = 0.5) + 
  scale_x_continuous(expand = c(0.025, 0), limits = c(0, 0.075)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  facet_grid(cols = vars(pos_quart), 
             rows = vars(n)) + 
  labs(x = "|est - true| / max(|est|, |true|)",
       y = NULL, 
       color = NULL) + 
  scale_color_identity(breaks = c("blue", "red", "darkgreen"),
                       labels = c("TMLE+DA", "G-comp.", "TMLE+GLM"),
                       guide = "legend")

ggsave("./plots/bias_binary_pos_scen1_obs.png", dpi = 600, width = 14, height = 5.5)

ggplot(o4_0[!is.na(pos_quart)]) +
  geom_line(aes(x = abs(rel_bias_tmle), y = 1 - ..y.., color = "blue"), stat = 'ecdf', 
            alpha = 0.5) +
  geom_line(aes(x = abs(rel_bias_param), y = 1 - ..y.., color = "red"), stat = 'ecdf', 
            alpha = 0.5) + 
  geom_line(aes(x = abs(rel_bias_tmlep), y = 1 - ..y.., color = "darkgreen"), stat = 'ecdf', 
            alpha = 0.5) + 
  scale_x_continuous(expand = c(0.025, 0), limits = c(0, 0.075)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  facet_grid(cols = vars(pos_quart), 
             rows = vars(n)) + 
  labs(x = "|est - true| / max(|est|, |true|)",
       y = NULL, 
       color = NULL) + 
  scale_color_identity(breaks = c("blue", "red", "darkgreen"),
                       labels = c("TMLE+DA", "G-comp.", "TMLE+GLM"),
                       guide = "legend")

ggsave("./plots/bias_binary_pos_scen2_obs.png", dpi = 600, width = 14, height = 5.5)

# absolute bias -----------------------------------------------------------

# observational
bias_cdf(ao4_0, limits = c(0, 0.3), "binary_ao_4_0")
bias_cdf(f1o4_0, limits = c(0, 0.3), "binary_f1o_4_0")
bias_cdf(f2o4_0, limits = c(0, 0.3), "binary_f2o_4_0")
bias_cdf(f3o4_0, limits = c(0, 0.3), "binary_f3o_4_0")

bias_cdf(ao0_100[vn_cate >= median(vn_cate, na.rm = TRUE)])

bias_cdf(ao0_100, limits = c(0, 0.1), "binary_ao_0_100")
bias_cdf(f1o0_100, limits = c(0, 0.1), "binary_f1o_0_100")
bias_cdf(f2o0_100, limits = c(0, 0.1), "binary_f2o_0_100")
bias_cdf(f3o0_100, limits = c(0, 0.1), "binary_f3o_0_100")

# rct
bias_cdf(ar4_0, limits = c(0, 0.05), "binary_ar_4_0")
bias_cdf(f1r4_0, limits = c(0, 0.05), "binary_f1r_4_0")
bias_cdf(f2r4_0, limits = c(0, 0.05), "binary_f2r_4_0")
bias_cdf(f3r4_0, limits = c(0, 0.05), "binary_f3r_4_0")

bias_cdf(ar0_100, limits = c(0, 0.025), "binary_ar_0_100")
bias_cdf(f1r0_100, limits = c(0, 0.025), "binary_f1r_0_100")
bias_cdf(f2r0_100, limits = c(0, 0.025), "binary_f2r_0_100")
bias_cdf(f3r0_100, limits = c(0, 0.025), "binary_f3r_0_100")

# mse ---------------------------------------------------------------------

mse_cdf(f1o4_0, limits = c(0, 0.025), "binary_f1o_4_0")
mse_cdf(f2o4_0, limits = c(0, 0.025), "binary_f2o_4_0")
mse_cdf(f3o4_0, limits = c(0, 0.025), "binary_f3o_4_0")

mse_cdf(f1o0_100, limits = c(0, 0.01), "binary_f1o_0_100")
mse_cdf(f2o0_100, limits = c(0, 0.01), "binary_f2o_0_100")
mse_cdf(f3o0_100, limits = c(0, 0.01), "binary_f3o_0_100")

mse_cdf(f1r4_0, limits = c(0, 0.01), "binary_f1r_4_0")
mse_cdf(f2r4_0, limits = c(0, 0.01), "binary_f2r_4_0")
mse_cdf(f3r4_0, limits = c(0, 0.01), "binary_f3r_4_0")

mse_cdf(f1r0_100, limits = c(0, 0.005), "binary_f1r_0_100")
mse_cdf(f2r0_100, limits = c(0, 0.005), "binary_f2r_0_100")
mse_cdf(f3r0_100, limits = c(0, 0.005), "binary_f3r_0_100")

