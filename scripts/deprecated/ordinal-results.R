# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

library(purrr)
library(data.table)
library(ggplot2)

devtools::load_all()

ordinal <- list.files(file.path(respath, "ordinal"))

ar0_100 <- ordinal[grepl("asymp[[:digit:]]+_0_100_TRUE.rds", ordinal)]
ar4_0 <- ordinal[grepl("asymp[[:digit:]]+_4_0_TRUE.rds", ordinal)]
fr0_100 <- ordinal[grepl("finite[[:digit:]]+_0_100_TRUE.rds", ordinal)]
fr4_0 <- ordinal[grepl("finite[[:digit:]]+_4_0_TRUE.rds", ordinal)]

res_ar0_100 <- read_results("ordinal", ar0_100)
res_ar4_0 <- read_results("ordinal", ar4_0)
res_fr0_100 <- read_results("ordinal", fr0_100, TRUE)
res_fr4_0 <- read_results("ordinal", fr4_0, TRUE)

# absolute bias CDFs
bias_cdf(res_ar0_100, file = "ordinal_ar_0_100")
bias_cdf(res_ar4_0, file = "ordinal_ar_4_0")
bias_cdf(res_fr0_100, file = "ordinal_fr_0_100")
bias_cdf(res_fr4_0, file = "ordinal_fr_4_0") 

# MSE CDFs
mse_cdf(res_fr0_100, file = "ordinal_fr_0_100")
mse_cdf(res_fr4_0, file = "ordinal_fr_4_0")
