source("R/dgp.R")

id <- Sys.getenv("SGE_TASK_ID")

args <- commandArgs(trailingOnly = TRUE)

# args <- list(
#   n_bin = 3,
#   n_num = 1,
#   inter_order = 2,
#   hte = TRUE,
#   conf_bias = 0.1,
#   pos_bound = 50,
#   npoints = 50,
#   eta = 4,
#   rho = 1,
#   tol = 0.01
# )

n_bin <- as.numeric(args[[1]])
n_num <- as.numeric(args[[2]])
inter_order <- as.numeric(args[[3]])
hte <- as.logical(args[[4]])
conf_bias <- as.numeric(args[[5]])
pos_bound <- as.numeric(args[[6]])
npoints <- as.numeric(args[[7]])
eta <- as.numeric(args[[8]])
rho <- as.numeric(args[[9]])

out <- try(dgp(
  n_bin = n_bin, n_num = n_num, 
  inter_order = inter_order, 
  hte = hte, conf_bias = conf_bias,
  pos_bound = pos_bound, npoints = npoints, 
  eta = eta, rho = rho, tol = 0.01
))

if (class(out) == "try-error") quit('no')

saveRDS(
  out, 
  glue::glue("data/sims/DGP_{n_bin}_{n_num}_{inter_order}_{hte}_{conf_bias}_{pos_bound}_{npoints}_{eta}_{rho}_{id}.rds")
)
