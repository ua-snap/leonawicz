#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas10
#SBATCH --job-name=tpvGCM
#SBATCH --account=snap
#SBATCH -p main

#### $1 optional argument, recommended, samples=TRUE, defaults to TRUE if unspecified;
#### $2 optional/required sample size argument, e.g., n=100, n is ignored if samples=FALSE;
####    Will throw exception if samples=TRUE and n not specified;
#### $3 optional argument, vc (vegetation classes), defaults to \'all\'  if unspecified, options are \'all\', \'cavm\', or \'none'\;
#### $4 optional argument, firemask, defaults to TRUE, applies 1950-2013 historical fire permiter mask;
Rscript /workspace/UA/mfleonawicz/projects/Flammability/code/tpByVeg/tpByVeg_CMIP5.R $1 $2 $3 $4
