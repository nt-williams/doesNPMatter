# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

# capture arguments from Rscript
args <- commandArgs(trailingOnly = TRUE)
sim_config <- args[1]
config_file <- paste0(args[2], ".yml")
randomized <- as.logical(args[3])

# library path for local R packages
.libPaths("/home/niw4001/R_local")

# source functions in R directory
devtools::load_all()

# capture config file
config <- config::get(file = here::here("tests", config_file), 
                      config = sim_config)

set.seed(543262)

# build task and run simulation
task <- list(
  id = as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID")),
  context = config$context, 
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
  ident = sim_config, 
  tasks = expand.grid(seeds = sample(109810836, config$m),
                      rho = 0, 
                      binary_cnf = 4, 
                      cont_cnf = 100, 
                      randomized = randomized)
)

plan(multisession)

partition(task)

quit("no")
