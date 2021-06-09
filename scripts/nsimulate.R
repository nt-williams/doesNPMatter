# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

# This script is intended to be called with `Rscript` from the command line!
# It loads a configuration file, loads the corresponding DGMs, partitions
#  the computing cluster, and runs/saves simulation results.

.libPaths("/home/niw4001/R_local")

setwd(here::here())

box::use(./R/nbias[simulate], config[get], data.table[...])

# capture the name of the configuration passed to Rscript as a parameter
args <- commandArgs(trailingOnly = TRUE)
sim_config <- args[1]

# capture Slurm task id
slurm_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))

if (is.na(slurm_id)) {
  # Slurm id isn't created during tests locally
  slurm_id <- 1
}

# capture config file
task <- get(file = "./scripts/config.yml", config = sim_config)

# load the DGMs 
dgps <- readRDS(task$dgp_path)
machines <- task$machines
K <- length(dgps)

# based on the Slurm task, split up the DGMs to be evaluated by the machines
do <- (1:K)[(0:(K - 1)) %/% (K / machines) + 1 == slurm_id]

# for DGMs to be used, simulate data and estimate ATE using method specified in config
results <- list()
for (i in 1:length(do)) {
  results[[i]] <- try(
    simulate(dgps[[i]], task$nobs, reps = task$reps, method = task$method)
  )
}

# save results with a unique identifier for the configuration and Slurm task
signature <- paste0(task$signature, "_", slurm_id, ".rds")
saveRDS(results, file.path("./data/res/", signature))

cat("Exit status:", 0, "\n")

quit("no")
