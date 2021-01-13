# "Does Nonparametric Matter?"" Simulations

Nicholas Williams, Iván Díaz, Kara Rudolph

------------------------------------------------------------------------

Simulations can be executed from a linux terminal using Slurm with `sbatch simulate.sh {size} {context} {randomized}` . Valid `size` arguments are `asymp` or `finite` . Valid `context` arguments are `ordinal` , `binary`, and `tte` . Valid `randomized` arguments are `TRUE` or `FALSE` .

For example, the following command would run an asymptotic simulation for an ordinal outcome in an RCT:

    sbatch simulate.sh asymp ordinal TRUE
