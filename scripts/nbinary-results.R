# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

library(data.table)

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

ao0_100 <- readRDS("./data/res/binary_asymp_obser_0_100.rds")
f1o0_100 <- readRDS("./data/res/binary_finite1_obser_0_100.rds")
f2o0_100 <- readRDS("./data/res/binary_finite2_obser_0_100.rds")
f3o0_100 <- readRDS("./data/res/binary_finite3_obser_0_100.rds")

aop0_100 <- readRDS("./data/res/binary_asymp_obserParam_0_100.rds")
f1op0_100 <- readRDS("./data/res/binary_finite1_obserParam_0_100.rds")
f2op0_100 <- readRDS("./data/res/binary_finite2_obserParam_0_100.rds")
f3op0_100 <- readRDS("./data/res/binary_finite3_obserParam_0_100.rds")

ao4_0 <- merge(ao4_0, aop4_0[, .(truth, tmlep_bias = tmle_bias)], by = "truth")
f1o4_0 <- merge(f1o4_0, f1op4_0[, .(truth, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")
f2o4_0 <- merge(f2o4_0, f2op4_0[, .(truth, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")
f3o4_0 <- merge(f3o4_0, f3op4_0[, .(truth, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")

ao0_100 <- merge(ao0_100, aop0_100[, .(truth, tmlep_bias = tmle_bias)], by = "truth")
f1o0_100 <- merge(f1o0_100, f1op0_100[, .(truth, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")
f2o0_100 <- merge(f2o0_100, f2op0_100[, .(truth, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")
f3o0_100 <- merge(f3o0_100, f3op0_100[, .(truth, tmlep_bias = tmle_bias, tmlep_mse = tmle_mse)], by = "truth")

# rct
ar4_0 <- readRDS("./data/res/binary_asymp_random_4_0.rds")
f1r4_0 <- readRDS("./data/res/binary_finite1_random_4_0.rds")
f2r4_0 <- readRDS("./data/res/binary_finite2_random_4_0.rds")
f3r4_0 <- readRDS("./data/res/binary_finite3_random_4_0.rds")

ar0_100 <- readRDS("./data/res/binary_asymp_random_0_100.rds")
f1r0_100 <- readRDS("./data/res/binary_finite1_random_0_100.rds")
f2r0_100 <- readRDS("./data/res/binary_finite2_random_0_100.rds")
f3r0_100 <- readRDS("./data/res/binary_finite3_random_0_100.rds")

# absolute bias -----------------------------------------------------------

# observational
bias_cdf(ao4_0, limits = c(0, 0.3), "binary_ao_4_0")
bias_cdf(f1o4_0, limits = c(0, 0.3), "binary_f1o_4_0")
bias_cdf(f2o4_0, limits = c(0, 0.3), "binary_f2o_4_0")
bias_cdf(f3o4_0, limits = c(0, 0.3), "binary_f3o_4_0")

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

