#!/bin/bash
##SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --account=snap
#SBATCH -p main
#SBATCH --ntasks=64 #86 #64 #93 # one for each year
####SBATCH --ntasks=108
#SBATCH --job-name=alfExtractMain
#SBATCH --nodes=2 #3 #2 #3
#SBATCH --ntasks-per-node=32 #29 #32 #31

# Required arguments: modelIndex, domain, project, years, reps
# Optional arguments: rmpi, cru, repSample, locgroup
# Example useage: modelIndex=1, domain=\'akcan1km\', reps=1:200, years=1950:2013, locgroup="stringName"
# Constraints: domain must 'akcan1km' (SNAP standard CMIP5 data) or 'ak1km' (Statewide CMIP5 JFSP)
# Defaults: rmpi=TRUE and use_cru=FALSE. repSample and locgroup are ignored.
# Notes: rmpi=FALSE means use R's parallel package on a single node;
#   use_cru=TRUE means outputs are parsed identically on any given Alfresco "model", but are relabeled properly as historical CRU-based outputs afterward.
#   This is intended for running on known historical years only, e.g., 1900-2007 for IEM Alfresco outputs.
#   For known GCM-based years, leave useCRU=FALSE to avoid improper relabeling.
#   repSample, optional, e.g., repSample=30, means randomly sample 30 replicates from the full set

mpirun --oversubscribe -np 1 Rscript /atlas_scratch/mfleonawicz/projects/SNAPQAQC/code/alfresco/alfExtractMain.R $1 $2 $3 $4 $5 $6 $7 $8 $9 ${10} ${11} ${12} ${13}
