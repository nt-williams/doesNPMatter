library(glue)

source("R/dgp.R")

id <- Sys.getenv("SGE_TASK_ID")

args <- commandArgs(trailingOnly = TRUE)

# FOR TESTING: 
# args <- list(
#   n_bin = 3,
#   n_num = 1,
#   inter_order = 2,
#   hte = TRUE
# )

n_bin <- as.numeric(args[[1]])
n_num <- as.numeric(args[[2]])
inter_order <- as.numeric(args[[3]])
hte <- as.logical(args[[4]])
conf_bias <- runif(1, -0.3, 0.3) # as.numeric(args[[5]])
pos_bound <- 1000 # as.numeric(args[[6]])
npoints <- 50 # as.numeric(args[[7]])
eta <- runif(1, 0.1, 10) # as.numeric(args[[8]])
rho <- runif(1, 0.1, 10) # as.numeric(args[[9]])

out <- try(dgp(
  n_bin = n_bin,
  n_num = n_num,
  inter_order = inter_order,
  hte = hte,
  conf_bias = conf_bias,
  pos_bound = pos_bound,
  npoints = npoints,
  eta = eta,
  rho = rho,
  tol = 0.01
))

if (class(out) == "try-error") quit("no")

saveRDS(out, glue("data/dgps/DGP_{n_bin}_{n_num}_{inter_order}_{hte}/{id}.rds"))

quit("no")
