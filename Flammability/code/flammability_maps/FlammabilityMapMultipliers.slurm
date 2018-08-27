#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas01
#SBATCH --job-name=FlamMultipliers
#SBATCH --account=snap
#SBATCH -p main

#### $1 period, passed as period=\'historical\', period=\'rcp60\', etc., required;
#### $2 model, passed as model=\'CRU32\', model=\'GFDL-CM3\', etc., required;
#### $3 samples, passed as samples=TRUE, defaults to TRUE, optional;
#### $4 mapset, passed as mapset=\'3m\' or mapset=\'5m\', etc., required;
#### $5 lightning, passed as lightning=TRUE, defaults to TRUE, optional;
#### $5 cp2scratch, passed as TRUE or FALSE, optional, defaults to TRUE;
#### $6 cp_originals, passed as TRUE or FALSE, optional, defaults to FALSE;
Rscript /workspace/UA/mfleonawicz/projects/Flammability/code/flammability_maps/FlammabilityMapMultipliers.R $1 $2 $3 $4 $5 $6 $7
