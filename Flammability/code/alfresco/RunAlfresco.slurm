#!/bin/sh
#SBATCH --reservation=snap_9
####SBATCH --exclusive
#SBATCH --ntasks=32
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=32
#SBATCH --account=snap
#SBATCH -p main

####export PATH=/usr/lib64/openmpi/bin:$PATH
####export LD_LIBRARY_PATH=/usr/lib64/openmpi/lib:$LD_LIBRARY_PATH

#export PATH=/home/UA/apbennett/test/bin:/usr/lib64/openmpi/bin:$PATH
#export LD_LIBRARY_PATH=/home/UA/apbennett/test/lib:/home/UA/apbennett/lib:/usr/lib64/openmpi/lib:$LD_LIBRARY_PATH

export PATH=/home/UA/apbennett/alf-altfire/bin:/usr/lib64/openmpi/bin:$PATH
export LD_LIBRARY_PATH=/home/UA/apbennett/alf-altfire/lib:/home/UA/apbennett/lib:/usr/lib64/openmpi/lib:$LD_LIBRARY_PATH

echo $1
echo $2
echo $3
echo $4
echo $5
echo $6
echo $7
echo $8
echo $9
echo ${10}
echo ${11}
echo ${12}
echo ${13}
echo ${14}
echo ${15}
echo ${16}
echo ${17}
echo ${18}

if [ $7 -eq 1 ]
then
# Changes made by Bob to create a directory on fast_scratch
rmdir Maps
maps_location=/fast_scratch/shiny/`/bin/hostname -s`/`date +%m-%d-%Y-%H-%M`
mkdir -p $maps_location/Maps
ln -s $maps_location/Maps Maps
## End changes by Bob

####mpirun -np 32 /home/UA/apbennett/bin/fresco-mpi --fif $6
mpirun --oversubscribe -np ${17} /home/UA/apbennett/alf-altfire/bin/fresco-mpi --fif $6
fi

sbatch -w `/bin/hostname -s` -D $2 CompileData.slurm $1 $3 $4 $5 $6 $8 $9 ${10} ${11} ${12} ${13} ${14} ${15} ${16} ${17} ${18} $maps_location
