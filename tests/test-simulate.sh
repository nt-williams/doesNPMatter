#!/bin/bash
#SBATCH --job-name=TestdoesNPMatter
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=24G
#SBATCH --partition=panda
#SBATCH --array=1-10
echo "$SLURM_ARRAY_TASK_ID"

source ~/.bashrc
spack load -r /bxc56dm
Rscript test-simulate.R ${1} ${2} ${3}

exit 0
