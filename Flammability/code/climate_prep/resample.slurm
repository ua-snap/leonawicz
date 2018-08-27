#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas09
#SBATCH --job-name=resamp2km1km
#SBATCH --account=snap
#SBATCH -p main

#### $1 is required period=\'historical\' or \'projected\'. Cannot be projected if cru=TRUE;
#### $2 is optional cru=TRUE, defaults to FALSE; if TRUE, must still specify period=\'historical\';

Rscript /workspace/UA/mfleonawicz/projects/Flammability/code/climate_prep/clim_resample_2km_1km.R $1 $2
