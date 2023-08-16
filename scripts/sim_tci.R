source("R/dgp.R")
source("R/estimators.R")
source("R/utils.R")

id <- Sys.getenv("SGE_TASK_ID")
if (id == "undefined" || id == "") id <- 1

args <- commandArgs(trailingOnly = TRUE)

args <- list(
  dgp = "DGP_5_1_3_FALSE",
  K = 250
)

DGP <- try(read_dgp(args[[1]], as.numeric(id)))

if (inherits(DGP, "try-error")) quit("no")

ans <- purrr::map_dfr(1:as.numeric(args[[2]]), function(k) {
  purrr::map_dfr(c(100, 500, 1000), function(n) {
    dat <- sample_dgp(DGP, n)
    
    data.frame(
      dgp = args[[1]], 
      id = id,
      n = n,
      gcomp = gcomp(dat, TRUE)
    )
  })
})

if (!file.exists(file.path("data/sims", args[[1]]))) {
  dir.create(file.path("data/sims", args[[1]]))
}

write.csv(ans, file.path("data/sims", args[[1]], paste0("sim_tci_", args[[1]], "_", id, ".csv")), row.names = FALSE)

quit("no")
