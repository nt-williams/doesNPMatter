source("R/dgp.R")
source("R/estimators.R")
source("R/utils.R")
source("R/tmle_mlr3.R")

library(mlr3verse)

# library(lmtp)

# change depending on cluster...
id <- Sys.getenv("SGE_TASK_ID")
if (id == "undefined" || id == "") id <- 1

args <- commandArgs(trailingOnly = TRUE)

# FOR TESTING:
args <- list(
  dgp = "DGP_5_1_3_FALSE",
  K = 1
)

DGP <- try(read_dgp(args[[1]], as.numeric(id)))

if (inherits(DGP, "try-error")) quit("no")

# Should result in a data.frame of k*3 rows, and 5 columns
ans <- purrr::map_dfr(1:as.numeric(args[[2]]), function(k) {
  purrr::map_dfr(c(100, 500, 1000), function(n) {
    dat <- sample_dgp(DGP, n)
    
    # Sl <- sl3::Lrnr_sl$new(
    #   learners = sl3::make_learner_stack(sl3::Lrnr_glm, sl3::Lrnr_lightgbm, sl3::Lrnr_earth), 
    #   metalearner = sl3::make_learner(sl3::Lrnr_nnls, convex = TRUE)
    # )
    
    # Sl <- c("SL.glm", "SL.sal", "SL.earth")
    
    # if (n == 1000) {
    #   V <- 2
    # } else {
    #   V <- 1
    # }
    
    # V <- 1
    
    data.frame(
      dgp = args[[1]], 
      id = id,
      n = n,
      cbps = cbps(dat),
      gcomp = gcomp(dat),
      bart = bart(dat),
      tmle = tmle_mlr3(dat)
      #cvtmle = tmle(dat, V, Sl)
    )
  })
})

if (!file.exists(file.path("data/sims", args[[1]]))) {
  dir.create(file.path("data/sims", args[[1]]))
}

write.csv(ans, file.path("data/sims", args[[1]], paste0("sim_", args[[1]], "_", id, ".csv")), row.names = FALSE)

quit("no")