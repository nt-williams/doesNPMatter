source("R/dgp.R")
source("R/estimators.R")
source("R/utils.R")
source("R/tmle_mlr3.R")

library(mlr3verse)

# change depending on cluster...
id <- Sys.getenv("SGE_TASK_ID")
if (id == "undefined" || id == "") id <- 1

# args <- commandArgs(trailingOnly = TRUE)

args <- list(
  dgp = "DGP_5_1_3_FALSE",
  K = 250
)

DGP <- try(read_dgp(args[[1]], as.numeric(id)))

if (inherits(DGP, "try-error")) quit("no")

# Should result in a data.frame of k*3 rows, and 5 columns
ans <- purrr::map_dfr(1:as.numeric(args[[2]]), function(k) {
  dat <- sample_dgp(DGP, 1000)
  
  data.frame(
    dgp = args[[1]], 
    id = id,
    n = 1000,
    cvtmle = tmle_mlr3(dat, folds = 5)
  )
})

if (!file.exists(file.path("data/sims", args[[1]]))) {
  dir.create(file.path("data/sims", args[[1]]))
}

write.csv(ans, file.path("data/sims", args[[1]], paste0("sim_cvtmle_", args[[1]], "_", id, ".csv")), row.names = FALSE)

quit("no")
