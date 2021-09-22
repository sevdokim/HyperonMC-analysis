#!/bin/bash
#PBS -l ncpus=1
#PBS -l mem=500mb
#PBS -l vmem=1000mb
#PBS -l cput=96:00:00
#PBS -l walltime=96:00:00
#PBS -q alice
#PBS -V

#process Hyperon MC production in 1 thread
#read previous setup if any
THIS_THREAD_PATH=$PBS_O_WORKDIR
if [ ! -z $THIS_THREAD_PATH ] ; then
    if ls $THIS_THREAD_PATH/env.sh >& /dev/null ; then
	for var in PRODUCTION_NAME PERIOD  MESON PERIOD_PRFX PRODUCTION_DIR WD ANDIR TGT_PRFX COMBINED_NAME
        do
            for val in $(grep "$var=" $THIS_THREAD_PATH/env.sh) ; do
                if [ ! -z $val ] ; then 
                    export $val
                    echo 'Using earlier defined variable ' $val
                fi
            done
        done
    fi
fi
#resubmit job by doing
#  $ cd MC_$COMBINED_NAME
#  $ qsub /path/to/analyse_production_1target_omgWidth.sh

echo 'original working directory is ' $WD
echo 'i create new directory ' $WD/MC_$COMBINED_NAME
cd $WD
mkdir -p MC_$COMBINED_NAME
cp analyse_production_1target_omgWidth.sh MC_$COMBINED_NAME
cp env_${COMBINED_NAME}.sh MC_${COMBINED_NAME}/env.sh
cp calibr.cards coeff_old.dat delta.dat e_cor_matrix.dat file_list_${COMBINED_NAME}.dat file_names.dat h_s_new.dat lgd2_cut.dat lossc.txt MC_$COMBINED_NAME
cd MC_$COMBINED_NAME
mv file_list_${COMBINED_NAME}.dat file_list.dat
ln -s ../Gener_dir Gener_dir 
../prog.sdv >& log_analysis                                                                                  
source /data3/users/evdokimov/hyperon/Hyperon_2007_2018_Rev/MC_05.02.2019/HypMC_RegGen/vmc_env.sh
h2root calibr.hbook >& log_converter                                                                       
cp calibr.hbook ../$COMBINED_NAME.hbook                                                            
cp calibr.root ../$COMBINED_NAME.root                                                              
