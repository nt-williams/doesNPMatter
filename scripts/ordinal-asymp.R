# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

.libPaths("/home/niw4001/R_local")

library(config)

devtools::load_all()

config <- get(file = here::here("scripts", "ordinal.yml"), 
              config = "asymp-testing")

set.seed(543262)

task <- list(
  id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),
  context = "ordinal", 
  machines = config$machines,
  n = config$nobs, 
  reps = config$reps, 
  m = config$reps, 
  size = config$size, 
  progfile = config$progfile,
  errfile = config$errfile,
  respath = config$respath,
  pkgs = config$pkgs, 
  mu = NULL, 
  sigma = NULL,
  ident = "asymp", 
  tasks = expand.grid(seeds = sample(109810836, config$m),
                      rho = 0, 
                      binary_cnf = 5, 
                      cont_cnf = 0, 
                      randomized = TRUE)
)

plan(multisession)

partition(task)

quit("no")
