# Nick Williams
# Research Biostatistician 
# Department of Population Health Sciences 
# Weill Cornell Medicine

.libPaths("/home/niw4001/R_local")

setwd("/home/niw4001/doesNPMatter")

devtools::load_all()

args <- commandArgs(trailingOnly = TRUE)

size <- args[1]
context <- args[2]
binary_cnf <- as.numeric(args[3])
cont_cnf <- as.numeric(args[4])
randomized <- as.logical(args[5])
parametric <- as.logical(args[6])
crossfit <- as.logical(args[7])

regex <- glue::glue("{size}_[[:digit:]]+_{binary_cnf}_{cont_cnf}_{randomized}_{parametric}_{crossfit}*")
op <- glue::glue("{context}_{size}_{binary_cnf}_{cont_cnf}_{randomized}_{parametric}_{crossfit}.rds")

out <- read_results(context, regex)

saveRDS(out, paste0("/home/niw4001/doesNPMatter/data/res/", op))

quit("no")