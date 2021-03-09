# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

library(purrr)
library(data.table)
library(ggplot2)

devtools::load_all()

ao4_0 <- read_results("binary", "asymp_[[:digit:]]+_4_0_FALSEFALSE*")
f1o4_0 <- read_results("binary", "finite1_[[:digit:]]+_4_0_FALSEFALSE*")
f2o4_0 <- read_results("binary", "finite2_[[:digit:]]+_4_0_FALSEFALSE*")
f3o4_0 <- read_results("binary", "finite3_[[:digit:]]+_4_0_FALSEFALSE*")

ao0_100 <- read_results("binary", "asymp_[[:digit:]]+_0_100_FALSEFALSE*")
f1o0_100 <- read_results("binary", "finite1_[[:digit:]]+_0_100_FALSEFALSE*")
f2o4_0 <- read_results("binary", "finite2_[[:digit:]]+_4_0_FALSEFALSE*")
f3o4_0 <- read_results("binary", "finite3_[[:digit:]]+_4_0_FALSEFALSE*")





ar4_0 <- read_results("binary", "asymp_[[:digit:]]+_4_0_TRUETRUE.rds")
f1r4_0 <- read_results("binary", "finite1_[[:digit:]]+_4_0_TRUETRUE.rds")
f2r4_0 <- find_files(binary, "finite2_[[:digit:]]+_4_0_TRUETRUE.rds")

bias_cdf(ao4_0, limits = c(0, 0.25))











mkdi











binary <- list.files(file.path(respath, "binary"))

# observational 
ao0_100 <- find_files(binary, "asymp[[:digit:]]+_0_100_FALSEFALSE.rds")
ao4_0 <- find_files(binary, "asymp[[:digit:]]+_4_0_FALSEFALSE.rds")
fo0_100 <- find_files(binary, "finite[[:digit:]]+_0_100_FALSE.rds")
fo1_4_0 <- find_files(binary, "finite1[1-5000]+_4_0_FALSEFALSE.rds")
f2o0_100 <- find_files(binary, "finite2[[:digit:]]+_0_100_FALSEFALSE.rds")
f2o4_0 <- find_files(binary, "finite2[[:digit:]]+_4_0_FALSEFALSE.rds")
fop0_100 <- find_files(binary, "finite1[[:digit:]]+_0_100_FALSETRUE.rds")
fop4_0 <- find_files(binary, "finite1[[:digit:]]+_4_0_FALSETRUE.rds")

res_ao0_100 <- read_results("binary", ao0_100)
res_ao4_0 <- read_results("binary", ao4_0)
res_fo0_100 <- read_results("binary", fo0_100, TRUE)
res_fo1_4_0 <- read_results("binary", fo1_4_0, TRUE)
res_f2o0_100 <- read_results("binary", f2o0_100, TRUE)
res_f2o4_0 <- read_results("binary", f2o4_0, TRUE)
res_fop0_100 <- read_results("binary", fop0_100, TRUE)
res_fop4_0 <- read_results("binary", fop4_0, TRUE)

res_fo0_100 <- 
  merge(res_fo0_100, res_fop0_100[, .(id, 
                                      tmlep_bias = tmle_bias, 
                                      tmlep_mse = tmle_mse)], 
        by = "id", all.x = TRUE, all.y = TRUE)

res_fo4_0 <- 
  merge(res_fo4_0, res_fop4_0[, .(id, 
                                  tmlep_bias = tmle_bias, 
                                  tmlep_mse = tmle_mse)], 
        by = "id", all.x = TRUE, all.y = TRUE)

# randomized
ar0_100 <- find_files(binary, "asymp[[:digit:]]+_0_100_TRUETRUE.rds")
ar4_0 <- find_files(binary, "asymp[[:digit:]]+_4_0_TRUETRUE.rds")
fr0_100 <- find_files(binary, "finite1[[:digit:]]+_0_100_TRUETRUE.rds")
fr4_0 <- find_files(binary, "finite1[[:digit:]]+_4_0_TRUETRUE.rds")
f2r0_100 <- find_files(binary, "finite2[[:digit:]]+_0_100_TRUETRUE.rds")
f2r4_0 <- find_files(binary, "finite2[[:digit:]]+_4_0_TRUETRUE.rds")

res_ar0_100 <- read_results("binary", ar0_100)
res_ar4_0 <- read_results("binary", ar4_0)
res_fr0_100 <- read_results("binary", fr0_100, TRUE)
res_fr4_0 <- read_results("binary", fr4_0, TRUE)
res_f2r0_100 <- read_results("binary", f2r0_100, TRUE)
res_f2r4_0 <- read_results("binary", f2r4_0, TRUE)

# absolute bias CDFs
bias_cdf(res_ao0_100, limits = c(0, 0.15), "binary_ao_0_100")   # asymptotic, 0 binary, 100 continuous
bias_cdf(res_ao4_0, limits = c(0, 0.15), "binary_ao_4_0")       # asymptotic, 4 binary, 0 continuous
bias_cdf(res_fo0_100, limits = c(0, 0.25), "binary_fo_0_100")   # finite (n = 500), 0 binary, 100 continuous
bias_cdf(res_fo1_4_0, limits = c(0, 0.25), "binary_fo1_4_0")       # finite (n = 500), 4 binary, 0 continuous
bias_cdf(res_f2o0_100, limits = c(0, 0.2), "binary_f2o_0_100")  # finite (n = 2500), 0 binary, 100 continuous
bias_cdf(res_f2o4_0, limits = c(0, 0.2), "binary_f2o_4_0")      # finite (n = 2500), 4 binary, 0 continuous 
bias_cdf(res_ar0_100, file = "binary_ar_0_100")                 # asymptotic, randomized, 0 binary, 100 continuous
bias_cdf(res_ar4_0, file = "binary_ar_4_0")                     # asymptotic, randomized, 4 binary, 0 continuous
bias_cdf(res_fr0_100, limit = c(0, 0.02), "binary_fr_0_100")    # finite (n = 500), randomized, 0 binary, 100 continuous
bias_cdf(res_fr4_0, limit = c(0, 0.02), "binary_fr_4_0")        # finite (n = 500), randomized, 4 binary, 0 continuous
bias_cdf(res_f2r0_100, limit = c(0, 0.02), "binary_f2r_0_100")  # finite (n = 2500), randomized, 0 binary, 100 continuous
bias_cdf(res_f2r4_0, limit = c(0, 0.02), "binary_f2r_4_0")      # finite (n = 2500), randomized, 4 binary, 0 continuous

# MSE CDFs
mse_cdf(res_fo0_100, limits = c(0, 0.06), "binary_fo_0_100")     # finite (n = 500), 0 binary, 100 continuous
mse_cdf(res_fo4_0, limits = c(0, 0.075), "binary_fo_4_0")        # finite (n = 500), 4 binary, 0 continuous
mse_cdf(res_f2o0_100, limits = c(0, 0.0175), "binary_f2o_0_100") # finite (n = 2500), 0 binary, 100 continuous
mse_cdf(res_f2o4_0, limits = c(0, 0.0175), "binary_f2o_4_0")     # finite (n = 2500), 4 binary, 0 continuous 
mse_cdf(res_fr0_100, file = "binary_fr_0_100")                   # finite (n = 500), randomized, 0 binary, 100 continuous
mse_cdf(res_fr4_0, file = "binary_fr_4_0")                       # finite (n = 500), randomized, 4 binary, 0 continuous
mse_cdf(res_f2r0_100, file = "binary_f2r_0_100")                 # finite (n = 2500), randomized, 0 binary, 100 continuous
mse_cdf(res_f2r4_0, file = "binary_f2r_4_0")                     # finite (n = 2500), randomized, 4 binary, 0 continuous

