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
#export PRODUCTION_DIR=/lustre/ihep.su/data/hyperon/HYPERON_MC/evdokimov/2008-11_Gener
export PRODUCTION_DIR=/lustre/ihep.su/data/hyperon/HYPERON_MC/evdokimov/reconvert_from_2008-11/2008-11
#export PRODUCTION_DIR=$ANDIR/sdv_MCruns/2008-11_Gener/
export AN_CONFIG_DIR=$ANDIR/2008-11_MC
#export HY_HBOOKS_DIR=$AN_CONFIG_DIR/hbooks
export HY_HBOOKS_DIR=$AN_CONFIG_DIR/hbooks_pi0eta_reconverted
rm -f Gener_dir
mkdir -p $HY_HBOOKS_DIR
export COMBINED_NAME
export WD=$(pwd)
#export prog_sdv=$ANDIR/24.01.2020_prog/calibr.x8664
#export prog_sdv=/afs/ihep.su/user/s/sevdokim/6gam_prog/calibr.x8664
export prog_sdv=/afs/ihep.su/user/s/sevdokim/24.01.2020_prog_2pi0/calibr.x8664

#====================================================
#     cycle over targets
#====================================================
for TGTPRFX in be79mm c78mm al35mm cu7mm sn5mm pb3mm ch80mm
#for TGTPRFX in be79mm
do
    for cond in s4eff
    do
	#for variation in "no_variation" #"dist+0.5percent" "dist-0.5percent" "enorm+0.5percent" "enorm-0.5percent"
	for variation in "dist+0.5percent" "dist-0.5percent" "enorm+0.5percent" "enorm-0.5percent" "dist+1.5percent" "dist-1.5percent" "enorm+1.5percent" "enorm-1.5percent" "dist+2percent" "dist-2percent" "enorm+2percent" "enorm-2percent"
        do
	    export TGT_PRFX=$TGTPRFX
	    MESON=K0
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
	    case "$variation" in
                # 1 percent
		"dist+1percent") percent_dist=1010; percent_enorm=1000;;
		"dist-1percent") percent_dist=990; percent_enorm=1000;;
		"enorm+1percent") percent_dist=1000; percent_enorm=1010;;
		"enorm-1percent") percent_dist=1000; percent_enorm=990;;
		# 2 persents
		"dist+2percent") percent_dist=1020; percent_enorm=1000;;
                "dist-2percent") percent_dist=980; percent_enorm=1000;;
                "enorm+2percent") percent_dist=1000; percent_enorm=1020;;
                "enorm-2percent") percent_dist=1000; percent_enorm=980;;
		# 1.5 percents
		"dist+1.5percent") percent_dist=1015; percent_enorm=1000;;
                "dist-1.5percent") percent_dist=985; percent_enorm=1000;;
                "enorm+1.5percent") percent_dist=1000; percent_enorm=1015;;
                "enorm-1.5percent") percent_dist=1000; percent_enorm=985;;
		# 0.5 percents
		"dist+0.5percent") percent_dist=1005; percent_enorm=1000;;
                "dist-0.5percent") percent_dist=995; percent_enorm=1000;;
                "enorm+0.5percent") percent_dist=1000; percent_enorm=1005;;
                "enorm-0.5percent") percent_dist=1000; percent_enorm=995;;
		# default - no variation
		*) percent_dist=1000; percent_enorm=1000;; #no variation
	    esac
	    COMBINED_NAME=${PRODUCTION_NAME}_${variation}
	    echo 'I combined name:' ${COMBINED_NAME}
	    cd $WD
	    mkdir -p MC_$COMBINED_NAME
	    cd MC_$COMBINED_NAME
	    n=0
	    #skip processing due to some condition
	    if [ -e file_list.dat ] ; then
		n=$(grep -c Run file_list.dat)
		#if [ $n = 10 ]; then  continue ; fi #all data are is analysed already.
		#so just skip it in order to save CPU time
	    fi
	    rm -f Gener_dir
	    ln -s $PRODUCTION_DIR Gener_dir
            #prepare filelist.dat
	    case "$TGT_PRFX" in
		"be79mm")	dist=3698; thick=79; enorm=10021;;
		"c78mm")	dist=3695; thick=78; enorm=10284;;
		"al35mm")       dist=3658; thick=35; enorm=9956;;
		"al17mm")	dist=3776; thick=17; enorm=9726;;
		"cu3mm")	dist=3674; thick=3; enorm=9746;;
		"cu7mm")	dist=3672; thick=7; enorm=9788;;
		"sn5mm")	dist=3673; thick=5; enorm=9792;;
		"pb3mm")        dist=3674; thick=3; enorm=9549;;
		"ch80mm")       dist=3700; thick=80; enorm=9980;;
		*)              dist=3698; thick=79; enorm=10021;; #default value (Be)
	    esac	    
	    dist=$(($dist * $percent_dist / 1000))
	    enorm=$(($enorm * $percent_enorm / 1000))
	    echo "/ Distance mm: ${dist}; ${thick}. ${enorm}"   > file_list.dat
	    echo './Gener_dir/' >> file_list.dat
	    cat /tmp/hyp_runs >> file_list.dat
	    export THIS_THREAD_PATH=$(pwd)
	    printenv > env.sh
	    rm -f prog.sdv
	    ln -s $prog_sdv prog.sdv
	    if [ $(grep -c Run file_list.dat) != 0 ] ; then
		#we have something to process, submit a job
		echo "qsub -q ihep-short $WD/analyse_1target.sh" > command
		$(cat command)
	    fi #else do not submit anything
	done # for variation
    done # for cond
done # for TGTPRFX
cd $WD
