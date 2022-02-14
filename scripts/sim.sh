#!/bin/bash
#SBATCH --job-name=doesNPMatter
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --partition=panda
#SBATCH --array=1-500
echo "$SLURM_ARRAY_TASK_ID"

source ~/.bashrc
spack load -r /bxc56dm
Rscript sim.R ${1} ${2}

exit 0
