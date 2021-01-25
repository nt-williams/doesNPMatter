# "Does Nonparametric Matter?" Simulations

Nicholas Williams, Iván Díaz, Kara Rudolph

------------------------------------------------------------------------

Simulations can be executed from a linux terminal using Slurm with `sbatch simulate.sh {size} {context} {binary confounders} {continuous confounder} {randomized} {parametric}` . Valid `size` arguments are the configuration names found in YAML files (such as `asymp` or `finite`). Valid `context` arguments are `ordinal` , `binary`, and `tte` . Valid `randomized` arguments are `TRUE` or `FALSE` .

For example, the following command would run an asymptotic simulation for an ordinal outcome with 4 binary baseline prognostics variables in an RCT:

    sbatch simulate.sh asymp ordinal 4 0 TRUE TRUE
