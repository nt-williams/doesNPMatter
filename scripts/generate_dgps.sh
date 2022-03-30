#!/bin/sh
#$ -l mem=4G,time=:20:
cd doesNPMatter
Rscript=/nfs/apps/R/4.0.3/bin/Rscript
export R_LIBS_USER=/ifs/home/msph/epi/ntw2117/R_4.0
${Rscript} scripts/generate_dgps.R $1 $2 $3 $4

exit 0
