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

export PRODUCTION_DIR=/lustre/ihep.su/data/hyperon/HYPERON_MC/evdokimov/2008-11_Gener/
#export PRODUCTION_DIR=$HYLUSTREMC/2008-11_Gener
#export PRODUCTION_DIR=$ANDIR/sdv_MCruns/2008-11_Gener/
export AN_CONFIG_DIR=$ANDIR/2008-11_MC
export HY_HBOOKS_DIR=$AN_CONFIG_DIR/hbooks_omgWidth_s4
rm -f Gener_dir
mkdir -p $HY_HBOOKS_DIR
export COMBINED_NAME
export WD=$(pwd)
export prog_sdv=$ANDIR/24.01.2020_prog/calibr.x8664

#====================================================
#     cycle over targets
#====================================================
for TGTPRFX in be79mm c78mm al35mm cu7mm sn5mm pb3mm #ch80mm
do
    #for cond in t2 s4 #""
    for width in {5..16}
    do
	cond=width${width}MeV
	export TGT_PRFX=$TGTPRFX
	COMBINED_NAME=$TGTPRFX
	nmesons=0
	#======================================================
        #     cycle over mesons
        #======================================================
	rm -f /tmp/hyp_runs_*
	for MES in omg # eta omg pi0 K0 f2Mike omg 2pi0
	do
	    let nmesons+=1
	    MESON=$MES
	    COMBINED_NAME=${COMBINED_NAME}_${MESON}
	    PRODUCTION_NAME=$PERIOD_PRFX$TGT_PRFX  # production name
	    PRODUCTION_NAME=${PRODUCTION_NAME}_${MESON}
	    if [ ! -z $cond ] ; then PRODUCTION_NAME=${PRODUCTION_NAME}_$cond ; fi
	    cd $PRODUCTION_DIR
	    echo 'Checking' $PRODUCTION_DIR/${PRODUCTION_NAME}/MCruns/
	    if /bin/ls ${PRODUCTION_NAME}/MCruns/Run*.gz -1  > /tmp/hyp_runs_$nmesons ; then
		echo 'Some .gz files found. Putted in list' /tmp/hyp_runs_$nmesons
		echo $(wc -l /tmp/hyp_runs_$nmesons | awk -F' ' '{ print $1 }') > $HY_HBOOKS_DIR/${PRODUCTION_NAME}.stat 
	    else 
		echo 'Did not find anything! Moving on...'
		rm /tmp/hyp_runs_$nmesons
	    fi
	done
	if [ ! -z $cond ] ; then COMBINED_NAME=${COMBINED_NAME}_$cond ; fi
	echo 'I combined name:' ${COMBINED_NAME}
	cd $WD
	mkdir -p MC_$COMBINED_NAME
	cd MC_$COMBINED_NAME
	rm -f Gener_dir
	ln -s $PRODUCTION_DIR Gener_dir
        #make filelist.dat
	case "$TGT_PRFX" in 
	    "be79mm")	echo "/ Distance mm: 3698; 79. 10050"  > file_list.dat	;;
	    "c78mm")	echo "/ Distance mm: 3695; 78. 10316"  > file_list.dat	;;
	    "al35mm")   echo "/ Distance mm: 3658; 35. 10012"  > file_list.dat	;;
	    "al17mm")	echo "/	Distance mm: 3776; 17. 9726"   > file_list.dat  ;;
	    "cu3mm")	echo "/ Distance mm: 3674; 3. 9872"    > file_list.dat	;;
	    "cu7mm")	echo "/ Distance mm: 3672; 7. 9872"    > file_list.dat 	;;
	    "sn5mm")	echo "/ Distance mm: 3673; 5. 9855"    > file_list.dat	;;
	    "pb3mm")    echo "/ Distance mm: 3674; 3. 9788"    > file_list.dat  ;;
	    "ch80mm")   echo "/ Distance mm: 3700; 80. 9781"   > file_list.dat	;;
	    *)          echo "/ Distance mm: 3700; 80. 9781"   > file_list.dat  ;; #default value (Be)
	esac
	echo './Gener_dir/' >> file_list.dat
	if /bin/ls /tmp/hyp_runs_* ; then
	    for runs in $(/bin/ls /tmp/hyp_runs_*) ; do
		cat $runs >> file_list.dat
	    done
	    export THIS_THREAD_PATH=$(pwd)
	    printenv > env.sh
	    rm -f prog.sdv
	    ln -s $prog_sdv prog.sdv
	    echo "qsub -q ihep-medium $WD/analyse_1target.sh" > command
	    qsub -q ihep-medium $WD/analyse_1target.sh
	    #done
	else
	    echo "no data found. skipping "
	fi
    done
done
cd $WD
