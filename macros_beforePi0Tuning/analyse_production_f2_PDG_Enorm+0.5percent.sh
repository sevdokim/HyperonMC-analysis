# this macro is for analysing Hyperon MC productions. 
# it combines productions of different mesons with same period, target and conditions in one and runs analysing program
# which produces hbook file and converts it into root format as well.
# you need to specify period, paths to productions and config files (coeff_old.dat and so on...) 
# script will do the rest, just look at the code. 

export PERIOD=2008-11                         # Hyperon Runs (2007-11, 2008-04, 2008-11, 2009-11, 2011-04 ... -- 15 runs in total) 
export PERIOD_PRFX=nov08_                     # Period prefix for files:  file_list.dat ==> file_list_nov08.dat  

export TGT_PRFX                        # Hyperon Tgts dlya formirovaniya imeni file_list_nov08.dat ==> file_list_nov08_be79mm.dat
                                                  # TGT_PRFX = be79mm, c78mm, al35mm, al17mm, cu3mm, cu7mm, sn5mm, pb3mm, ch80mm     
export PRODUCTION_NAME  # production name     
export MESON
export PRODUCTION_NAME

    #export PRODUCTION_DIR=$ANDIR/MC_05.02.2019/2008-11_Gener/
export PRODUCTION_DIR=/lustre/ihep.su/data/hyperon/HYPERON_MC/evdokimov/2008-11_Gener
    #export PRODUCTION_DIR=$ANDIR/sdv_MCruns/2008-11_Gener/
export AN_CONFIG_DIR=$ANDIR/2008-11_MC
export HY_HBOOKS_DIR=$AN_CONFIG_DIR/hbooks
rm -f Gener_dir
mkdir -p $HY_HBOOKS_DIR
export COMBINED_NAME
export WD=$(pwd)
#export prog_sdv=$ANDIR/24.01.2020_prog/calibr.x8664
export prog_sdv=$ANDIR/6gam_prog/calibr.x8664

#====================================================
#     cycle over targets
#====================================================
for TGTPRFX in be79mm c78mm al35mm cu7mm sn5mm pb3mm ch80mm
#for TGTPRFX in be79mm
do
    for cond in s4eff
    do
	for mass in 0
        do
	    export TGT_PRFX=$TGTPRFX
	    MESON=f2
	    PRODUCTION_NAME=$PERIOD_PRFX$TGT_PRFX  # production name
	    PRODUCTION_NAME=${PRODUCTION_NAME}_${MESON}
	    PRODUCTION_NAME=${PRODUCTION_NAME}_PDG
	    if [ ! -z $cond ] ; then PRODUCTION_NAME=${PRODUCTION_NAME}_$cond ; fi
	    cd $PRODUCTION_DIR
	    echo 'Checking' $(pwd)/${PRODUCTION_NAME}/MCruns/
	    if /bin/ls ${PRODUCTION_NAME}/MCruns/*.gz -1  > /tmp/hyp_runs ; then
		echo 'Some .gz files found. Putted in list' /tmp/hyp_runs
	    else 
		echo 'Did not find anything! Moving on...'
	    fi
	    
	    COMBINED_NAME=${PRODUCTION_NAME}_Enorm+0.5percent
	    echo 'I combined name:' ${COMBINED_NAME}
	    cd $WD
	    mkdir -p MC_$COMBINED_NAME
	    cd MC_$COMBINED_NAME
	    n=0
	    if [ -e file_list.dat ] ; then
		n=$(grep -c Run file_list.dat)
		#if [ $n = 10 ]; then  continue ; fi #all data are is analysed already.
		#so just skip it in order to save CPU time
	    fi
	    rm -f Gener_dir
	    ln -s $PRODUCTION_DIR Gener_dir
            #prepare filelist.dat
	    case "$TGT_PRFX" in 
		"be79mm")	echo "/ Distance mm: 3698; 79. 10100"  > file_list.dat	;;
		"c78mm")	echo "/ Distance mm: 3695; 78. 10368"  > file_list.dat	;;
		"al35mm")       echo "/ Distance mm: 3658; 35. 10062"  > file_list.dat	;;
		"al17mm")	echo "/	Distance mm: 3776; 17. 9811"   > file_list.dat  ;;
		"cu3mm")	echo "/ Distance mm: 3674; 3. 9921"    > file_list.dat	;;
		"cu7mm")	echo "/ Distance mm: 3672; 7. 9921"    > file_list.dat 	;;
		"sn5mm")	echo "/ Distance mm: 3673; 5. 9904"    > file_list.dat	;;
		"pb3mm")        echo "/ Distance mm: 3674; 3. 9837"    > file_list.dat  ;;
		"ch80mm")       echo "/ Distance mm: 3700; 80. 9830"   > file_list.dat	;;
		*)              echo "/ Distance mm: 3700; 80. 9781"   > file_list.dat  ;; #default value (Be)
	    esac
	    echo './Gener_dir/' >> file_list.dat
	    cat /tmp/hyp_runs >> file_list.dat
	    export THIS_THREAD_PATH=$(pwd)
	    printenv > env.sh
	    rm -f prog.sdv
	    ln -s $prog_sdv prog.sdv
	    echo "qsub $WD/analyse_1target.sh" > command
	    if [ $(grep -c Run file_list.dat) != 0 ] ; then
		#we have something to process, submit a job
		qsub -q ihep-short $WD/analyse_1target.sh
	    fi #else do not submit anything
	done
    done
done
cd $WD
