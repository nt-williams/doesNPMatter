# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

library(purrr)
library(data.table)
library(ggplot2)

devtools::load_all()

binary <- list.files(file.path(respath, "binary"))

# observational 
ao0_100 <- find_files(binary, "asymp[[:digit:]]+_0_100_FALSE.rds")
ao4_0 <- find_files(binary, "asymp[[:digit:]]+_4_0_FALSE.rds")
fo0_100 <- find_files(binary, "finite[[:digit:]]+_0_100_FALSE.rds")
fo4_0 <- find_files(binary, "finite[[:digit:]]+_4_0_FALSE.rds")
f2o0_100 <- find_files(binary, "finite2[[:digit:]]+_0_100_FALSEFALSE.rds")
f2o4_0 <- find_files(binary, "finite2[[:digit:]]+_4_0_FALSEFALSE.rds")
fop0_100 <- find_files(binary, "finite1[[:digit:]]+_0_100_FALSETRUE.rds")
fop4_0 <- find_files(binary, "finite1[[:digit:]]+_4_0_FALSETRUE.rds")

res_ao0_100 <- read_results("binary", ao0_100)
res_ao4_0 <- read_results("binary", ao4_0)
res_fo0_100 <- read_results("binary", fo0_100, TRUE)
res_fo4_0 <- read_results("binary", fo4_0, TRUE)
res_f2o0_100 <- read_results("binary", f2o0_100, TRUE)
res_f2o4_0 <- read_results("binary", f2o4_0, TRUE)
res_fop0_100 <- read_results("binary", fop0_100, TRUE)
res_fop4_0 <- read_results("binary", fop4_0, TRUE)

# randomized
ar0_100 <- find_files(binary, "asymp[[:digit:]]+_0_100_TRUETRUE.rds")
ar4_0 <- find_files(binary, "asymp[[:digit:]]+_4_0_TRUETRUE.rds")
fr0_100 <- find_files(binary, "finite[[:digit:]]+_0_100_TRUE.rds")
fr4_0 <- find_files(binary, "finite[[:digit:]]+_4_0_TRUE.rds")

res_ar0_100 <- read_results("binary", ar0_100)
res_ar4_0 <- read_results("binary", ar4_0)
res_fr0_100 <- read_results("binary", fr0_100, TRUE)
res_fr4_0 <- read_results("binary", fr4_0, TRUE)

# absolute bias CDFs
result_cdf(res_ao0_100, limits = c(0, 0.15)) # asymptotic, 0 binary, 100 continuous
result_cdf(res_ao4_0, limits = c(0, 0.15)) # asymptotic, 4 binary, 0 continuous

result_cdf(res_fo0_100, limits = c(0, 0.25)) # finite (n = 500), 0 binary, 100 continuous
result_cdf(res_fo4_0, limits = c(0, 0.25)) # finite (n = 500), 4 binary, 0 continuous

result_cdf(res_fop0_100, limits = c(0, 0.15)) # finite (n = 500), 0 binary, 100 continuous, TMLE using only GLM
result_cdf(res_fop4_0, limits = c(0, 0.175)) # finite (n = 500), 4 binary, 0 continuous, TMLE using only GLM

result_cdf(res_f2o0_100, limits = c(0, 0.2)) # finite (n = 2500), 0 binary, 100 continuous
result_cdf(res_f2o4_0, limits = c(0, 0.2)) # finite (n = 2500), 4 binary, 0 continuous 

result_cdf(res_ar0_100) # asymptotic, randomized, 0 binary, 100 continuous
result_cdf(res_ar4_0) # asymptotic, randomized, 4 binary, 0 continuous

result_cdf(res_fr0_100) # finite (n = 500), randomized, 0 binary, 100 continuous
result_cdf(res_fr4_0) # finite (n = 500), randomized, 4 binary, 0 continuous
