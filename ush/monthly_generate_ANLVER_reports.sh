#!/bin/bash

set -xa

VDATE_YYYY=${1:-`date +%Y`}
VDATE_mm=${2:-`date +%m`}
VDATE_yy=${VDATE_yy:-`echo $VDATE_YYYY | cut -c3-4`}

###################################################
# COMPUTES MONTHLY SUMMARY OF S1 SCORES FROM      #
# VARIOUS MODELS                                  #
###################################################

########################################################
# COMPUTES MONTHLY SUMMARY OF S1 SCORES FOR MODELS:    #
#    GFS - 12HR FORECASTS                              #
#    GFL - 24HR FORECASTS                              #
########################################################
model_list=" GFL GFS "
for mfile in $model_list
 do
   MODEL=$DATA/${mfile}.${VDATE_mm}.in
   MAX=$DATA/s1max.${mfile}.${VDATE_mm}
   TRU=$DATA/s1tru.${mfile}.${VDATE_mm}
   #################
   #  input files  #
   #################
   export FORT10="arkv_verdate.in"
   export FORT12="$FIXgfs_legacy_verif_v25/anlver/arkv_sum${mfile}.in"
   export FORT50="$MODEL"
   #################
   # output files  #
   #################
   export FORT30="$MAX"
   export FORT40="$TRU"
   $EXECgfs_legacy_verif_v25/verf_s1sum  1> s1sum${mfile}.${VDATE_mm} 2>s2sum${VDATE_mfile}.err
   #export err=$?; err_chk
done

##############################################################
# COMPUTES MONTHLY SUMMARY OF WMO MSLP,HGT,TEMP SCORES AND   #
# COMPUTES MONTHLY SUMMARY OF WMO WIND SCORES FOR BOTH THE   #
# GFL AND GFL MODELS                                         #
##############################################################
cyc_list=" 00 12 "
for ct in $cyc_list
 do
   XMSLP=${VDATE_mm}.xmslp.in${ct}
   X850Z=${VDATE_mm}.x850z.in${ct}
   X850T=${VDATE_mm}.x850t.in${ct}
   X500Z=${VDATE_mm}.x500z.in${ct}
   X500T=${VDATE_mm}.x500t.in${ct}
   X250Z=${VDATE_mm}.x250z.in${ct}
   X250T=${VDATE_mm}.x250t.in${ct}
   V850=${VDATE_mm}.w850.in${ct}
   V500=${VDATE_mm}.w500.in${ct}
   V250=${VDATE_mm}.w250.in${ct}
   XYZ=xyzsum.${VDATE_mm}
   WND=windsum.${VDATE_mm}
   ################
   # input files  #
   ################
   export FORT10="arkv_verdate.in"
   export FORT21="$COMINverm_anlverYYYY_inputdata/$XMSLP"
   export FORT22="$COMINverm_anlverYYYY_inputdata/$X850Z"
   export FORT23="$COMINverm_anlverYYYY_inputdata/$X850T"
   export FORT24="$COMINverm_anlverYYYY_inputdata/$X500Z"
   export FORT25="$COMINverm_anlverYYYY_inputdata/$X500T"
   export FORT26="$COMINverm_anlverYYYY_inputdata/$X250Z"
   export FORT27="$COMINverm_anlverYYYY_inputdata/$X250T"
   export FORT28="$COMINverm_anlverYYYY_inputdata/$V850"
   export FORT29="$COMINverm_anlverYYYY_inputdata/$V500"
   export FORT30="$COMINverm_anlverYYYY_inputdata/$V250"
   #################
   # output files  #
   #################
   export FORT70="$XYZ"
   export FORT80="$WND"
   $EXECgfs_legacy_verif_v25/verf_xsum  1>xsum${ct}.${VDATE_mm} 2>xsum${ct}.${VDATE_mm}.err
   #export err=$?; err_chk
   $EXECgfs_legacy_verif_v25/verf_wsum  1>wsum${ct}.${VDATE_mm} 2>wsum${ct}.${VDATE_mm}.err
   #export err=$?; err_chk
   cat $XYZ $WND > vwmomon${ct}.${VDATE_mm}
done

################
# input files  #
################
export FORT20="vwmomon00.${VDATE_mm}"
export FORT30="vwmomon12.${VDATE_mm}"
#################
# output files  #
#################
export FORT40="vwmomon.${VDATE_mm}"
$EXECgfs_legacy_verif_v25/verf_combintbls
#Cexport err=$?; err_chk

##########################################################
# GENERATES MONTHLY VERIFICATION IN NON-STANDARD TABLES  #
# FOR NORTHERN AND SOUTHERN HEMISHPERE AND THE TROPICS   # 
##########################################################
VW=$DATA/vwmomon.${VDATE_mm}
NHX=$DATA/wmonhx${VDATE_mm}
SHX=$DATA/wmoshx${VDATE_mm}
TRX=$DATA/wmotrx${VDATE_mm}
NHW=$DATA/wmonhw${VDATE_mm}
SHW=$DATA/wmoshw${VDATE_mm}
TRW=$DATA/wmotrw${VDATE_mm}
NHM=$DATA/wmonhm${VDATE_mm}
SHM=$DATA/wmoshm${VDATE_mm}
TRP=$DATA/wmotrp${VDATE_mm}
################
# input file   #
################
export FORT40="$VW"
###########################################################
#  output files (temp -- will be appended to arch files   #
#  and deleted, see end of script)                        #
###########################################################
export FORT41="$NHX"
export FORT42="$SHX"
export FORT43="$TRX"
export FORT44="$NHW"
export FORT45="$SHW"
export FORT46="$TRW"
export FORT47="$NHM"
export FORT48="$SHM"
export FORT49="$TRP"
########################
# EXECUTING wmoptbls   #
########################
$EXECgfs_legacy_verif_v25/verf_wmoptbls 1>wmoptbls.${VDATE_mm} 2>wmoptbls.err
#export err=$?; err_chk
########################
# EXECUTING wmoptblw   #
########################
$EXECgfs_legacy_verif_v25/verf_wmoptblw 1>wmoptblw.${VDATE_mm} 2>wmoptblw.err
#export err=$?; err_chk
cp wmoptblw.${VDATE_mm} $DATA_sumac4YYYY_wmoreports/ryhwmx${VDATE_mm}
cat $NHM >> $DATA/wmonhm$VDATE_YYYY
cat $SHM >> $DATA/wmoshm$VDATE_YYYY
cat $TRP >> $DATA/wmotrp$VDATE_YYYY
cat $NHX >> $DATA/wmonhx$VDATE_YYYY
cat $SHX >> $DATA/wmoshx$VDATE_YYYY
cat $TRX >> $DATA/wmotrx$VDATE_YYYY
cat $NHW >> $DATA/wmonhw$VDATE_YYYY
cat $SHW >> $DATA/wmoshw$VDATE_YYYY
cat $TRW >> $DATA/wmotrw$VDATE_YYYY
####################################################################
# Appends current month's s1/s2 scores to archive for each model   #
####################################################################
if [ ${VDATE_mm} -lt '10' ]; then
   export msd=`echo ${VDATE_mm} | cut -c2`
   sed -e 's/MSL    ${msd}/MSL   0${msd}/g' s1max.GFS.${VDATE_mm} | sed -e 's/500MB  ${msd}/500MB 0${msd}/g' > h1
   sed -e 's/MSL    ${msd}/MSL   0${msd}/g' s1tru.GFS.${VDATE_mm} | sed -e 's/500MB  ${msd}/500MB 0${msd}/g' > j1
   sed -e 's/MSL    ${msd}/MSL   0${msd}/g' s1max.GFL.${VDATE_mm} | sed -e 's/500MB  ${msd}/500MB 0${msd}/g' > h4
   sed -e 's/MSL    ${msd}/MSL   0${msd}/g' s1tru.GFL.${VDATE_mm} | sed -e 's/500MB  ${msd}/500MB 0${msd}/g' > j4
else
   export msd=`echo ${VDATE_mm} | cut -c2`
   sed -e 's/MSL    ${msd}/MSL   ${msd}/g' s1max.GFS.${VDATE_mm} | sed -e 's/500MB  ${msd}/500MB ${msd}/g' > h1
   sed -e 's/MSL    ${msd}/MSL   ${msd}/g' s1tru.GFS.${VDATE_mm} | sed -e 's/500MB  ${msd}/500MB ${msd}/g' > j1
   sed -e 's/MSL    ${msd}/MSL   ${msd}/g' s1max.GFL.${VDATE_mm} | sed -e 's/500MB  ${msd}/500MB ${msd}/g' > h4
   sed -e 's/MSL    ${msd}/MSL   ${msd}/g' s1tru.GFL.${VDATE_mm} | sed -e 's/500MB  ${msd}/500MB ${msd}/g' > j4
fi
mv h1 $DATA_anlverYYYY_inputdata/s1max.GFS.${VDATE_mm}
mv j1 $DATA_anlverYYYY_inputdata/s1tru.GFS.${VDATE_mm}
mv h4 $DATA_anlverYYYY_inputdata/s1max.GFL.${VDATE_mm}
mv j4 $DATA_anlverYYYY_inputdata/s1tru.GFL.${VDATE_mm}
if [ ${VDATE_mm} -eq '01' ]; then
 echo 'MONYR NN GFSMAX MSL 12-72HR  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' | cat >> $DATA_anlverYYYY_wmoreports/s1GFSmsl
 echo 'MONYR NN GFSMAX 500 12-72HR  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' | cat >> $DATA_anlverYYYY_wmoreports/s1GFS500
 echo 'MONYR NN GFLMAX MSL 24-240HR (X24)  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33'|cat >> $DATA_anlverYYYY_wmoreports/s1GFLmsl
 echo 'MONYR NN GFLMAX 500 24-240HR (X24)  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' |cat >> $DATA_anlverYYYY_wmoreports/s1GFL500
 echo 'MONYR NN GFSTRU MSL 12-72HR  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' | cat >> $DATA_anlverYYYY_wmoreports/s2GFSmsl
 echo 'MONYR NN GFSTRU 500 12-72HR  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' | cat >> $DATA_anlverYYYY_wmoreports/s2GFS500
 echo 'MONYR NN GFLTRU MSL 24-240HR (X24)  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' |cat >> $DATA_anlverYYYY_wmoreports/s2GFLmsl
 echo 'MONYR NN GFLTRU 500 24-240HR (X24)  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' |cat >> $DATA_anlverYYYY_wmoreports/s2GFL500
else
 cp $COMINverm_anlverYYYY_wmoreports/s1GFSmsl $DATA_anlverYYYY_wmoreports
 cp $COMINverm_anlverYYYY_wmoreports/s1GFS500 $DATA_anlverYYYY_wmoreports
 cp $COMINverm_anlverYYYY_wmoreports/s1GFLmsl $DATA_anlverYYYY_wmoreports
 cp $COMINverm_anlverYYYY_wmoreports/s1GFL500 $DATA_anlverYYYY_wmoreports
 cp $COMINverm_anlverYYYY_wmoreports/s2GFSmsl $DATA_anlverYYYY_wmoreports
 cp $COMINverm_anlverYYYY_wmoreports/s2GFS500 $DATA_anlverYYYY_wmoreports
 cp $COMINverm_anlverYYYY_wmoreports/s2GFLmsl $DATA_anlverYYYY_wmoreports
 cp $COMINverm_anlverYYYY_wmoreports/s2GFL500 $DATA_anlverYYYY_wmoreports
fi
grep -v "MONYR" $DATA_anlverYYYY_wmoreports/s1GFSmsl | cat >> a1
head -6 $DATA_anlverYYYY_wmoreports/s1max.GFS.${VDATE_mm} >> a1
rm -f $DATA_anlverYYYY_wmoreports/s1GFSmsl
echo 'MONYR NN GFSMAX MSL 12-72HR  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' | cat >> a1
cat a1 >> $DATA_anlverYYYY_wmoreports/s1GFSmsl
grep -v "MONYR" $DATA_anlverYYYY_wmoreports/s1GFS500 |cat >> b1
tail -n -6 $DATA_anlverYYYY_wmoreports/s1max.GFS.${VDATE_mm} >> b1
rm -f $DATA_anlverYYYY_wmoreports/s1GFS500
echo 'MONYR NN GFSMAX 500 12-72HR  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' | cat >> b1
cat b1 >> $DATA_anlverYYYY_wmoreports/s1GFS500
grep -v "MONYR" $DATA_anlverYYYY_wmoreports/s1GFLmsl |cat >> e1
head -6 $DATA_anlverYYYY_wmoreports/s1max.GFL.${VDATE_mm} >> e1
rm -f $DATA_anlverYYYY_wmoreports/s1GFLmsl
echo 'MONYR NN GFLMAX MSL 24-240HR (X24)  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33'|cat >> e1
cat e1 >> $DATA_anlverYYYY_wmoreports/s1GFLmsl
grep -v "MONYR" $DATA_anlverYYYY_wmoreports/s1GFL500 |cat >> f1
tail -n -6 $DATA_anlverYYYY_wmoreports/s1max.GFL.${VDATE_mm} >> f1
rm -f $DATA_anlverYYYY_wmoreports/s1GFL500
echo 'MONYR NN GFLMAX 500 24-240HR (X24)  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' |cat >> f1
cat f1 >> $DATA_anlverYYYY_wmoreports/s1GFL500
grep -v "MONYR" $DATA_anlverYYYY_wmoreports/s2GFSmsl |cat >> a2
head -6 $DATA_anlverYYYY_wmoreports/s1tru.GFS.${VDATE_mm} >> a2
rm -f $DATA_anlverYYYY_wmoreports/s2GFSmsl
echo 'MONYR NN GFSTRU MSL 12-72HR  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' | cat >> a2
cat a2 >> $DATA_anlverYYYY_wmoreports/s2GFSmsl
grep -v "MONYR" $DATA_anlverYYYY_wmoreports/s2GFS500 |cat >> b2
tail -n -6 $DATA_anlverYYYY_wmoreports/s1tru.GFS.${VDATE_mm} >> b2
rm -f $DATA_anlverYYYY_wmoreports/s2GFS500
echo 'MONYR NN GFSTRU 500 12-72HR  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' | cat >> b2
cat b2 >> $DATA_anlverYYYY_wmoreports/s2GFS500
grep -v "MONYR" $DATA_anlverYYYY_wmoreports/s2GFLmsl |cat >> e2
head -6 $DATA_anlverYYYY_wmoreports/s1tru.GFL.${VDATE_mm} >> e2
rm -f $DATA_anlverYYYY_wmoreports/s2GFLmsl
echo 'MONYR NN GFLTRU MSL 24-240HR (X24)  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' |cat >> e2
cat  e2 >> $DATA_anlverYYYY_wmoreports/s2GFLmsl
grep -v "MONYR" $DATA_anlverYYYY_wmoreports/s2GFL500 |cat >> f2
tail -n -6 $DATA_anlverYYYY_wmoreports/s1tru.GFL.${VDATE_mm} >> f2
rm -f $DATA_anlverYYYY_wmoreports/s2GFL500
echo 'MONYR NN GFLTRU 500 24-240HR (X24)  GBL,59,W33,E33,49,RGNAR1,LL59,LLW33,LLE33' |cat >> f2
cat f2 >> $DATA_anlverYYYY_wmoreports/s2GFL500

##########################################################################################
# Saving the monthly s1max.GFS and s1max.NAM 36hr and 72hr S1 scores for skill's report  #
##########################################################################################
echo "GFS" | cat >> s1gfs
tail -n -4 $COMINverm_anlverYYYY_inputdata/s1max.GFS.${VDATE_mm} >> s1gfs
model_list=" s1gfs "
for mfile in $model_list
 do
   MODEL=${mfile}
   S136=${mfile}"36"
   S172=${mfile}"72"
   #################
   #  input files  #
   #################
   export FORT10="gfs"
   export FORT50="$MODEL"
   $EXECgfs_legacy_verif_v25/verf_gets1scores 1>output 2>gets1.err
   #export err=$?; err_chk
   if [ -f $COMINverm_s1YYYY/${mfile}"36500" ]; then
       cp $COMINverm_s1YYYY/${mfile}"36500" $DATA_s1YYYY
   fi
   cat $DATA/${mfile}"36" >> $DATA_s1YYYY/${mfile}"36500"
   if [ -f $COMINverm_s1YYYY/${mfile}"72500" ]; then
       cp $COMINverm_s1YYYY/${mfile}"72500" $DATA_s1YYYY
   fi
   cat $DATA/${mfile}"72" >> $DATA_s1YYYY/${mfile}"72500"
done

