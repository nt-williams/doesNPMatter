# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

library(purrr)
library(data.table)

respath <- here::here("data", "res", "ordinal")
res <- map_dfr(file.path(respath, list.files(respath)), ~ as.data.frame(readRDS(.x)))

setDT(res)

res[, `:=`(tmle_bias = tmle - truth, 
           param_bias = param - truth)]

mean(res$tmle_bias)
mean(res$param_bias)
