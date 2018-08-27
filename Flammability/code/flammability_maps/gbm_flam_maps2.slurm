#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas01
#SBATCH --job-name=gbmFlamMaps2
#SBATCH --account=snap
#SBATCH -p main

#### $1 period, passed as period=\'historical\', period=\'rcp60\', etc., required;
#### $2 model, passed as model=\'CRU32\', model=\'GFDL-CM3\', etc., required;
#### $3 optional unified single-GBM CAVM region as allcavm=TRUE, defaults to FALSE;
#### $4 samples=TRUE, optional, defaults to FALSE;
#### $5 sample size, e.g. n=100, should be known for input data, required if samples=TRUE;
Rscript /workspace/UA/mfleonawicz/projects/Flammability/code/flammability_maps/gbm_flam_maps2.R $1 $2 $3 $4 $5
