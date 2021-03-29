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
Rscript simulate.R ${1} ${2} ${3} ${4} ${5} ${6} ${7}

exit 0
