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

res_ar0_100[, `:=`(tmle_bias = tmle - truth, 
                   param_bias = param - truth)]

res_ar4_0[, `:=`(tmle_bias = tmle - truth, 
                 param_bias = param - truth)]

res_fr0_100[, `:=`(tmle_bias = tmle - truth, 
                   param_bias = param - truth)]

res_fr4_0[, `:=`(tmle_bias = tmle - truth, 
                 param_bias = param - truth)]

mean(res_ar0_100$tmle_bias)
mean(res_ar0_100$param_bias)

mean(res_ar4_0$tmle_bias)
mean(res_ar4_0$param_bias)

mean(res_fr0_100$tmle_bias)
mean(res_fr0_100$param_bias)

mean(res_fr4_0$tmle_bias)
mean(res_fr4_0$param_bias)

result_cdf(res_ar0_100)
result_cdf(res_ar4_0)
result_cdf(res_fr0_100)
result_cdf(res_fr4_0)
