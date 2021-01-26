# "Does Nonparametric Matter?" Simulations

Nicholas Williams, Iván Díaz, Kara Rudolph

------------------------------------------------------------------------

This project is organized as an R package with functions for simulations stored in the R/ directory. R scripts are stored in the scripts/ directory.

Simulations can be executed from a linux terminal using Slurm. Navigate to the scripts/ directory and enter commands as: `sbatch simulate.sh {size} {context} {binary confounders} {continuous confounder} {randomized} {parametric}` . Valid `size` arguments are the configuration names found in YAML files (such as `asymp` or `finite`). Valid `context` arguments are `ordinal` , `binary`, and `tte` . Valid `randomized` and `parametric` arguments are `TRUE` or `FALSE` .

For example, the following command would run an asymptotic simulation for an ordinal outcome with 4 binary baseline prognostics variables in an RCT:

    sbatch simulate.sh asymp ordinal 4 0 TRUE TRUE

The following command would run a finite simulation for a binary outcome with no binary confounders, a continuous confounder that ranges from 0-100 in an observational study with TMLE performing estimation with the Super Learner:

    sbatch simulate.sh finite1 binary 0 100 FALSE FALSE

Results are saved as .rds files in the data/ directory.
