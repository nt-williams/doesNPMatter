# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

# This script is intended to be called with `Rscript` from the command line!

.libPaths("/home/niw4001/R_local")

setwd(here::here())

box::use(config[get], data.table[...])

# capture the name of the configuration passed to Rscript as a parameter
args <- commandArgs(trailingOnly = TRUE)

# load config file
task <- get(file = "./scripts/config.yml", config = args[1])

# find matching result files
files <- file.path("./data/res", list.files("./data/res", pattern = paste0("^", task$signature)))

out <- list()
for (i in seq_along(files)) {
  out[[i]] <- rbindlist(readRDS(files[i]))
}

out <- rbindlist(out)

saveRDS(out, file.path("./data/extracted", paste0(task$signature, ".rds")))
