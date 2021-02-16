# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

library(purrr)
library(data.table)
library(ggplot2)

devtools::load_all()

tte <- list.files(file.path(respath, "tte"))

ar4_0 <- tte[grepl("asymp[[:digit:]]+_4_0_TRUETRUE.rds", tte)]

res_ar4_0 <- read_results("tte", ar4_0)

# absolute bias CDFs
bias_cdf(res_ar4_0, file = "tte_ar_4_0")
