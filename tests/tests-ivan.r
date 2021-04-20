library(devtools)
## install_github('benkeser/drord')
## install.packages('extraDistr')
## install.packages('config')
## install.packages('doesNPMatter', repos = NULL, type = 'source')

sim_config <- 'finite1'
outcome_type <- 'binary'
config_file <- paste0(outcome_type, ".yml")
binary_cnf <- 10
cont_cnf <- 1
randomized <- FALSE
parametric <- TRUE
crossfit <- FALSE

devtools::load_all()

config <- config::get(file = here::here("scripts", config_file),
                      config = sim_config)
task <- list(
  id = 1,
  context = config$context,
  machines = config$machines,
  n = config$nobs,
  reps = config$reps,
  size = config$size,
  progfile = config$progfile,
  errfile = config$errfile,
  respath = config$respath,
  pkgs = config$pkgs,
  ident = sim_config,
  tasks = expand.grid(seeds = sample(36708876, config$m),
                      binary_cnf = binary_cnf,
                      cont_cnf = cont_cnf,
                      randomized = randomized,
                      parametric = parametric,
                      crossfit = crossfit)
)

partition(task)


library(purrr)
library(data.table)
library(ggplot2)

devtools::load_all()

res <- read_results(outcome_type, paste0(sim_config, '_[[:digit:]]+', '_', binary_cnf, '_',
                                         cont_cnf, '_', randomized, '_', parametric, '_',
                                         crossfit, '.rds'))
res
bias_cdf(res, limits = c(0, .1))