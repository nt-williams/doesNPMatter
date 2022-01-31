#!/bin/bash
#SBATCH --job-name=doesNPMatter
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --partition=panda
#SBATCH --array=1-1000
echo "$SLURM_ARRAY_TASK_ID"

source ~/.bashrc
spack load -r /bxc56dm
Rscript nsimulate.R ${1}

exit 0
