source("R/dgp.R")
source("R/estimators.R")
source("R/utils.R")

# change depending on cluster...
# id <- Sys.getenv("SGE_TASK_ID")
id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
if (id == "undefined" || id == "") id <- 1

args <- commandArgs(trailingOnly = TRUE)

# FOR TESTING:
# args <- list(
#   dgp = "DGP_3_0_1_FALSE",
#   # n = 1000, 
#   K = 10
# )

DGP <- read_zipped_dgp(args[[1]], id)

# Should result in a data.frame of k*3 rows, and 5 columns
ans <- purrr::map_dfr(1:args[[2]], function(k) {
  purrr::map_dfr(c(100, 500, 1000), function(n) {
    dat <- sample_dgp(DGP, n)
    stack <- sl3::make_learner_stack(
      sl3::Lrnr_glm, 
      sl3::Lrnr_earth,
      sl3::Lrnr_lightgbm,
      sl3::Lrnr_dbarts
    )
    
    V <- ifelse(n >= 1000, 5, 10)
    
    data.frame(
      n = n,
      cbps = cbps(dat),
      gcomp = gcomp(dat),
      bart = bart(dat),
      cvtmle = tmle(dat, V, stack)
    )
  })
})

write.csv(ans, file.path("data/sims", args[[1]], paste0("sim_", args[[1]], "_", id, ".csv")), row.names = FALSE)

quit("no")
