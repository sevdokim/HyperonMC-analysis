#!/bin/bash
#PBS -l ncpus=1
#PBS -l mem=500mb
#PBS -l vmem=2000mb
#PBS -V

#this needed to convert hbook to root.
VMC_ENV_SH=/afs/ihep.su/user/s/sevdokim/HyperonMC-factory/Hyp_RegGen/vmc_env.sh
source $VMC_ENV_SH

#process Hyperon MC production in 1 thread
#read previous setup if any
echo "THIS_THREAD_PATH = ${THIS_THREAD_PATH}"
if [ -z $THIS_THREAD_PATH ] ; then THIS_THREAD_PATH=$PBS_O_WORKDIR ; fi
if [ -z $THIS_THREAD_PATH ] ; then THIS_THREAD_PATH=. ; fi
if /bin/ls $THIS_THREAD_PATH/env.sh > /dev/null ; then
    for var in PRODUCTION_NAME PERIOD  MESON PERIOD_PRFX PRODUCTION_DIR WD ANDIR TGT_PRFX COMBINED_NAME AN_CONFIG_DIR THIS_THREAD_PATH HY_HBOOKS_DIR
    do
        for val in $(grep "$var=" $THIS_THREAD_PATH/env.sh) ; do
            if [ ! -z $val ] ; then 
                export $val
                echo 'Using earlier defined variable ' $val
            fi
        done
    done
else
    echo 'cannot find configuration. Stopping.'
    exit 1
fi

#resubmit job by doing
#  $ cd MC_$COMBINED_NAME
#  $ qsub /path/to/analyse_1target.sh

#============================== analyzer ===================================================
echo 'I shall start to work in' $THIS_THREAD_PATH
if cd $THIS_THREAD_PATH ; then echo 'I am in there.' ; else echo 'Cannot cd into' $THIS_THREAD_PATH ; echo 'Stopping...'; exit 1 ; fi
THIS_THREAD_PATH=$(pwd) 
if [ ! -f file_list.dat ] ; then echo 'No file_list.dat found. Stopping...'; exit 1; fi
cd $AN_CONFIG_DIR 
if cp calibr.cards coeff_old.dat delta.dat e_cor_matrix.dat file_names.dat h_s_new.dat lgd2_cut.dat lossc.txt $THIS_THREAD_PATH ; then echo 'I copied config files successfully' ; else echo 'Some config files are missing. Please check!' ; fi
cd $THIS_THREAD_PATH
if [ -e Gener_dir ] ; then rm -f Gener_dir ; fi
ln -s $PRODUCTION_DIR Gener_dir
#go to scratch for running
echo "Making new directory" /scratch/$(pwd) 
mkdir -p /scratch/$(pwd)
echo "Copying files from " $(pwd) into /scratch/$(pwd)
cp -ap * /scratch/$(pwd)
echo "cd-ing there"
cd /scratch/$(pwd)
echo "that is what we have:"
ls -lth
echo "starting analysis"
echo "3..2..1..go!!!"
echo "./prog.sdv > log_analysis"
if ./prog.sdv > log_analysis ; then 
    echo 'Success!!! Cutting logfile.'
    echo 'first 100 and last 100 lines of the log_analysis:'
    head -n 100 log_analysis > log_analysis_part
    echo '===================================== cutted here =============================================='>>log_analysis_part
    tail -n 100 log_analysis >> log_analysis_part
    rm -f log_analysis.bz2
    bzip2 -9 log_analysis
else 
    echo 'Something went wront with analyzer. Please check.'; 
    pwd
    ls -lth 
    exit 1 ;
fi
#=============================== converter ===================================================
echo "Converting: h2root calibr.hbook >& log_converter"
if h2root calibr.hbook >& log_converter ; then 
    echo 'Successfully converted! Removing log_converter.'
    rm log_converter
else
    echo 'Something went wront with analyzer. Please check.'; 
    pwd
    ls -lth
    exit 1 
fi
if /bin/ls $HY_HBOOKS_DIR > /dev/null ; then
    echo 'I archive .hbook and copy .hbook .root files to ' $HY_HBOOKS_DIR
    rm -f calibr.hbook.bz2
    bzip2 -9 calibr.hbook
    mv calibr.hbook $HY_HBOOKS_DIR/$COMBINED_NAME.hbook
    mv calibr.root $HY_HBOOKS_DIR/$COMBINED_NAME.root
    echo 'Job finished successfully!!! Superb!'
fi
echo "Here is what I have after all: "
pwd ; ls -lth; 
echo "Copying results to $THIS_THREAD_PATH"
echo "cp -ap log* *.root *.hbook *.bz2 $THIS_THREAD_PATH"
cp -ap log* *.root *.hbook *.bz2 $THIS_THREAD_PATH
