#!/bin/sh
#SBATCH --exclusive
#SBATCH --mail-type=END
#SBATCH --mail-user=mfleonawicz@alaska.edu
#SBATCH --ntasks=32
#SBATCH --nodes=1
####SBATCH --nodelist=atlas01
#SBATCH --job-name=FlamBackcast
#SBATCH --account=snap
#SBATCH -p main

#### $1 period, passed as period=\'historical\', period=\'rcp60\', etc., required;
#### $2 model, passed as model=\'CRU32\', model=\'GFDL-CM3\', etc., required;
#### $3 samples, passed as samples=TRUE, defaults to TRUE, optional;
#### $4 mapset, passed as mapset=\'3m\', etc., required;
#### $5 out, passed as out=\'3m_loop', etc., required, mapset and out may be the same;
#### $6 yearloop, passed as yearloop=1950:2013, etc., required;
#### $7 n, passed as n=40, etc., optional, defaults to 40, see code for details on use;
Rscript /workspace/UA/mfleonawicz/projects/Flammability/code/flammability_maps/duplicate_flam_maps.R $1 $2 $3 $4 $5 $6 $7
