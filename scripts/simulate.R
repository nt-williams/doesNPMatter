# Nick Williams
# Research Biostatistician
# Department of Population Health Sciences
# Weill Cornell Medicine

# capture arguments from Rscript
args <- commandArgs(trailingOnly = TRUE)
sim_config <- args[1]
config_file <- paste0(args[2], ".yml")
binary_cnf <- as.numeric(args[3])
cont_cnf <- as.numeric(args[4])
randomized <- as.logical(args[5])
parametric <- as.logical(args[6])

# library path for local R packages
.libPaths("/home/niw4001/R_local")

# source functions in R directory
devtools::load_all()

# capture config file
config <- config::get(file = here::here("scripts", config_file), 
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
  ident = sim_config, 
  tasks = expand.grid(seeds = sample(109810836, config$m),
                      rho = 0, 
                      binary_cnf = binary_cnf, 
                      cont_cnf = cont_cnf, 
                      randomized = randomized, 
                      parametric = parametric)
)

plan(multisession)

partition(task)

quit("no")
