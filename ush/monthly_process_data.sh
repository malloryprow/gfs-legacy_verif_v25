#!/bin/bash

set -xa

VDATE_YYYY=${1:-`date +%Y`}
VDATE_mm=${2:-`date +%m`}
VDATE_yy=${VDATE_yy:-`echo $VDATE_YYYY | cut -c3-4`}

###############################################################
# This utility script has two functions:  First, it generates #
# tables headers and labels.  Secondly, this utility script   #
# combines daily stats to generate monthly statistics for the #
# WMO Reports.                                                #
###############################################################

###################
# QCMON begins    #
###################
###############################
# Generates the header files  #
###############################
cp $FIXgfs_legacy_verif_v25/qcmon/acrtmp   acrdata
cp $FIXgfs_legacy_verif_v25/qcmon/airtmp   airdata
cp $FIXgfs_legacy_verif_v25/qcmon/asdtmp   asddata
cp $FIXgfs_legacy_verif_v25/qcmon/cqctmp   cqcdata
cp $FIXgfs_legacy_verif_v25/qcmon/rpttmp   rptdata
cp $FIXgfs_legacy_verif_v25/qcmon/shptmp   shpdata
cp $FIXgfs_legacy_verif_v25/qcmon/sxn.doc  sxn.doc
if [ "$VDATE_mm" = "01" ] || [ "$VDATE_mm" = "10" ]; then
  if [ "$VDATE_mm" = "01" ]; then
   MONTH="JANUARY"
   sed -e 's/xxxxxxxxx/  JANUARY/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  else
   MONTH="OCTOBER"
   sed -e 's/xxxxxxxxx/  OCTOBER/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  fi
  echo "ZZ$MONTH $VDATE_YYYY acrdata acrheadr" > acrtable
  echo "ZZ$MONTH $VDATE_YYYY airdata airheadr" > airtable
  echo "ZZ$MONTH $VDATE_YYYY asddata asdheadr" > asdtable
  echo "ZZ$MONTH $VDATE_YYYY cqcdata cqcheadr" > cqctable
  echo "ZZ$MONTH $VDATE_YYYY rptdata rptheadr" > rpttable
  echo "ZZ$MONTH $VDATE_YYYY shpdata shiphead0" > shipable
elif [ "$VDATE_mm" = "02" ]  || [ "$VDATE_mm" = "11" ] || [ "$VDATE_mm" = "12" ]; then
  if [ "$VDATE_mm" = "02" ]; then
    MONTH="FEBRUARY"
    sed -e 's/xxxxxxxxx/ FEBRUARY/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  elif [ "$VDATE_mm" = "11" ]; then
    MONTH="NOVEMBER"
    sed -e 's/xxxxxxxxx/ NOVEMBER/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  else
    MONTH="DECEMBER"
    sed -e 's/xxxxxxxxx/ DECEMBER/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  fi
  echo "Z$MONTH $VDATE_YYYY acrdata acrheadr" > acrtable
  echo "Z$MONTH $VDATE_YYYY airdata airheadr" > airtable
  echo "Z$MONTH $VDATE_YYYY asddata asdheadr" > asdtable
  echo "Z$MONTH $VDATE_YYYY cqcdata cqcheadr" > cqctable
  echo "Z$MONTH $VDATE_YYYY rptdata rptheadr" > rpttable
  echo "Z$MONTH $VDATE_YYYY shpdata shiphead0" > shipable
elif [ "$VDATE_mm" = "03" ]  || [ "$VDATE_mm" = "04" ]; then
  if [ "$VDATE_mm" = "03" ]; then
    MONTH="MARCH"
    sed -e 's/xxxxxxxxx/    MARCH/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  else
    MONTH="APRIL"
    sed -e 's/xxxxxxxxx/    APRIL/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  fi
  echo "ZZZZ$MONTH $VDATE_YYYY acrdata acrheadr" > acrtable
  echo "ZZZZ$MONTH $VDATE_YYYY airdata airheadr" > airtable
  echo "ZZZZ$MONTH $VDATE_YYYY asddata asdheadr" > asdtable
  echo "ZZZZ$MONTH $VDATE_YYYY cqcdata cqcheadr" > cqctable
  echo "ZZZZ$MONTH $VDATE_YYYY rptdata rptheadr" > rpttable
  echo "ZZZZ$MONTH $VDATE_YYYY shpdata shiphead0" > shipable
elif [ "$VDATE_mm" = "05" ]; then
  MONTH="MAY"
  sed -e 's/xxxxxxxxx/      MAY/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  echo "ZZZZZZ$MONTH $VDATE_YYYY acrdata acrheadr" > acrtable
  echo "ZZZZZZ$MONTH $VDATE_YYYY airdata airheadr" > airtable
  echo "ZZZZZZ$MONTH $VDATE_YYYY asddata asdheadr" > asdtable
  echo "ZZZZZZ$MONTH $VDATE_YYYY cqcdata cqcheadr" > cqctable
  echo "ZZZZZZ$MONTH $VDATE_YYYY rptdata rptheadr" > rpttable
  echo "ZZZZZZ$MONTH $VDATE_YYYY shpdata shiphead0" > shipable
elif [ "$VDATE_mm" = "06" ] || [ "$VDATE_mm" = "07" ]; then
  if [ "$VDATE_mm" = "06" ]; then
    MONTH="JUNE"
    sed -e 's/xxxxxxxxx/     JUNE/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  else
    MONTH="JULY"
    sed -e 's/xxxxxxxxx/     JULY/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  fi
  echo "ZZZZZ$MONTH $VDATE_YYYY acrdata acrheadr" > acrtable
  echo "ZZZZZ$MONTH $VDATE_YYYY airdata airheadr" > airtable
  echo "ZZZZZ$MONTH $VDATE_YYYY asddata asdheadr" > asdtable
  echo "ZZZZZ$MONTH $VDATE_YYYY cqcdata cqcheadr" > cqctable
  echo "ZZZZZ$MONTH $VDATE_YYYY rptdata rptheadr" > rpttable
  echo "ZZZZZ$MONTH $VDATE_YYYY shpdata shiphead0" > shipable
elif [ "$VDATE_mm" = "08" ]; then
  MONTH="AUGUST"
  sed -e 's/xxxxxxxxx/   AUGUST/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  echo "ZZZ$MONTH $VDATE_YYYY acrdata acrheadr" > acrtable
  echo "ZZZ$MONTH $VDATE_YYYY airdata airheadr" > airtable
  echo "ZZZ$MONTH $VDATE_YYYY asddata asdheadr" > asdtable
  echo "ZZZ$MONTH $VDATE_YYYY cqcdata cqcheadr" > cqctable
  echo "ZZZ$MONTH $VDATE_YYYY rptdata rptheadr" > rpttable
  echo "ZZZ$MONTH $VDATE_YYYY shpdata shiphead0" > shipable
elif [ "$VDATE_mm" = "09" ]; then
  MONTH="SEPTEMBER"
  sed -e 's/xxxxxxxxx/SEPTEMBER/g' sxn.doc > $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt"
  echo "$MONTH $VDATE_YYYY acrdata acrheadr" > acrtable
  echo "$MONTH $VDATE_YYYY airdata airheadr" > airtable
  echo "$MONTH $VDATE_YYYY asddata asdheadr" > asdtable
  echo "$MONTH $VDATE_YYYY cqcdata cqcheadr" > cqctable
  echo "$MONTH $VDATE_YYYY rptdata rptheadr" > rpttable
  echo "$MONTH $VDATE_YYYY shpdata shiphead0" > shipable
fi
$EXECgfs_legacy_verif_v25/verf_qcheadr < acrtable > headr_tmp
#export err=$?; err_chk
$EXECgfs_legacy_verif_v25/verf_qcheadr < airtable >> headr_tmp
#export err=$?; err_chk
$EXECgfs_legacy_verif_v25/verf_qcheadr < asdtable >> headr_tmp
#export err=$?; err_chk
$EXECgfs_legacy_verif_v25/verf_qcheadr < cqctable >> headr_tmp
#export err=$?; err_chk
$EXECgfs_legacy_verif_v25/verf_qcheadr < rpttable >> headr_tmp
#export err=$?; err_chk
$EXECgfs_legacy_verif_v25/verf_qcheadr < shipable >> headr_tmp
#export err=$?; err_chk
if [ "$VDATE_mm" = "01" ] || [ "$VDATE_mm" = "10" ]; then
  sed -e 's/ZZ/\ \ /g' headr_tmp > headr_file
elif [ "$VDATE_mm" = "02" ] || [ "$VDATE_mm" = "11" ] || [ "$VDATE_mm" = "12" ]; then
  sed -e 's/Z/\ /g' headr_tmp > headr_file
elif [ "$VDATE_mm" = "03" ] || [ "$VDATE_mm" = "04" ]; then
  sed -e 's/ZZZZ/\ \ \ \ /g' headr_tmp > headr_file
elif [ "$VDATE_mm" = "05" ]; then
  sed -e 's/ZZZZZZ/\ \ \ \ \ \ /g' headr_tmp > headr_file
elif [ "$VDATE_mm" = "06" ] || [ "$VDATE_mm" = "07" ]; then
  sed -e 's/ZZZZZ/\ \ \ \ \ /g' headr_tmp > headr_file
elif [ "$VDATE_mm" = "08" ]; then
  sed -e 's/ZZZ/\ \ \ /g' headr_tmp > headr_file
elif [ "$VDATE_mm" = "09" ]; then
  cat headr_tmp > headr_file
fi
chmod u+x headr_file
. headr_file
sed -e 's/00 U/12 U/g' shiphead0 > shiphead2
export VDATE_YYYYmm_ndays=$(cal $VDATE_mm $VDATE_YYYY | awk 'NF {DAYS = $NF}; END {print DAYS}')
echo "  "$VDATE_YYYY$VDATE_mm"0100  "$VDATE_YYYY$VDATE_mm$VDATE_YYYYmm_ndays"18           2           0" > options
echo "  "$VDATE_YYYY$VDATE_mm"0100  "$VDATE_YYYY$VDATE_mm$VDATE_YYYYmm_ndays"18           2           1" > optionrc
echo "    " > blank
cat blank >> blanks
cat blank >> blanks
cat blank >> blanks
cat blank >> blanks
cat blank >> blanks
cat blank >> blanks
cat blank >> blanks
############################
# Joins CQC  daily stats   #
############################
if [ 31 -eq $VDATE_YYYYmm_ndays ]
then
  days=" 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 "
elif [ 30 -eq $VDATE_YYYYmm_ndays ]
then
  days=" 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 "
elif [ 29 -eq $VDATE_YYYYmm_ndays ]
then
  days=" 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 "
else [ 28 -eq $VDATE_YYYYmm_ndays ]
  days=" 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 "
fi
for ndays in $days
do
cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/cqcht/cqc_$VDATE_YYYY$VDATE_mm*_events.fnl >> cqc_events.fnl
cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/cqcht/cqc_$VDATE_YYYY$VDATE_mm*_stncnt.fnl >> cqc_stncnt.fnl
cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/cqcht/cqc_$VDATE_YYYY$VDATE_mm*_stnlst.fnl >> cqc_stnlst.fnl
done
sed '1,$s/*/9/g' cqc_events.fnl > temp.sed
sed '1,$s/  R /0.0 /g' temp.sed > cqc_events.fnl
rm temp.sed
cp cqc_events.fnl $DATA_cqchtYYYY_mm_inputdata/cqc_events.fnl
cp cqc_stncnt.fnl $DATA_cqchtYYYY_mm_inputdata/cqc_stncnt.fnl
cp cqc_stnlst.fnl $DATA_cqchtYYYY_mm_inputdata/cqc_stnlst.fnl
####################################################################
#  Check for presence of cqc56 fix file.  May be RFC'ed into       #
#  prod at a later date.  If not there, simply touch the file in   #
#  $DATA so the sorc will have something to read.                  #
####################################################################
if test -f $FIXgfs_legacy_verif_v25/cqcht/cqc56
then
 cp $FIXgfs_legacy_verif_v25/cqcht/cqc56 .
else
 touch cqc56
fi
export FORT12="cqc_events.fnl"
export FORT15="cqc_stncnt.fnl"
export FORT16="cqc_stnlst.fnl"
export FORT17="montable$VDATE_mm$VDATE_yy".txt""
export FORT55="$FIXgfs_legacy_verif_v25/cqcht/index.all"
export FORT56="cqc56"
#############################################################
#  montable                montable$VDATE_mm$VDATE_yy.txt   #
#  stnlist.txt             stnlist$VDATE_mm$VDATE_yy.txt    #
#  cqcsum96.saf.txt        saf96$VDATE_mm$VDATE_yy.txt      #
#  cqcsum96.canada.txt     canada96$VDATE_mm$VDATE_yy.txt   #
#  cqcsum96.us.short.txt   us96$VDATE_mm$VDATE_yy.stn.txt   #
#  cqcsum96.us.txt         us96$VDATE_mm$VDATE_yy.txt       #
#  cqcsum96.all.txt        all96$VDATE_mm$VDATE_yy.txt      #
#  station_bias.fnl        baseline$VDATE_mm$VDATE_yy.txt   #
#############################################################
$EXECgfs_legacy_verif_v25/verf_cqc_sum96 1> $DATA_cqchtYYYY_mm_cqchtreports/all96.txt
#Cexport err=$?; err_chk
export FORT55="$FIXgfs_legacy_verif_v25/cqcht/index.us"
$EXECgfs_legacy_verif_v25/verf_cqc_sum96 1> us96$VDATE_mm$VDATE_yy.txt
#Cexport err=$?; err_chk
cp us96$VDATE_mm$VDATE_yy.txt $DATA_cqchtYYYY_mm_cqchtreports/us96cqcht.txt
export lines=`sed -n '/^ LIST OF/=' us96$VDATE_mm$VDATE_yy.txt`
export linesm1=`expr $lines - 1`
sed 1,${linesm1}d us96$VDATE_mm$VDATE_yy.txt > $DATA_cqchtYYYY_mm_cqchtreports/us96cqcht.stn.txt
export FORT55="$FIXgfs_legacy_verif_v25/cqcht/index.canada"
$EXECgfs_legacy_verif_v25/verf_cqc_sum96 1> $DATA_cqchtYYYY_mm_cqchtreports/canada96.txt
#Cexport err=$?; err_chk
export FORT55="$FIXgfs_legacy_verif_v25/cqcht/index.saf"
$EXECgfs_legacy_verif_v25/verf_cqc_sum96 1> $DATA_cqchtYYYY_mm_cqchtreports/saf96.txt
#Cexport err=$?; err_chk
export FORT16="cqc_stnlst.fnl"
$EXECgfs_legacy_verif_v25/verf_cqc_stnlist 1> $DATA_cqchtYYYY_mm_cqchtreports/stnlist.txt
#Cexport err=$?; err_chk
export FORT16="cqc_stnlst.fnl"
export FORT17="${FIXgfs_legacy_verif_v25:?}/sonde.land.tbl"
export FORT60="baseline$VDATE_mm$VDATE_yy".txt""
$EXECgfs_legacy_verif_v25/verf_cqc_bias < $FIXgfs_legacy_verif_v25/cqcht/bias.parm 1> $DATA_cqchtYYYY_mm_cqchtreports/bias_fnl.txt
#Cexport err=$?; err_chk
cp montable$VDATE_mm$VDATE_yy.txt $DATA_cqchtYYYY_mm_cqchtreports/montable$VDATE_mm$VDATE_yy
cp baseline$VDATE_mm$VDATE_yy.txt $DATA_cqchtYYYY_mm_cqchtreports/baseline.txt
#######################
# Joins daily stats   #
#######################
for ndays in $days
do
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/qcmon/asraob$VDATE_mm* >> $DATA_qcmonYYYY_inputdata/asraob$VDATE_mm
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/qcmon/qcacr$VDATE_mm* >> $DATA_qcmonYYYY_inputdata/qcacr$VDATE_mm
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/qcmon/qcair$VDATE_mm* >> $DATA_qcmonYYYY_inputdata/qcair$VDATE_mm
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/qcmon/qcasd$VDATE_mm* >> $DATA_qcmonYYYY_inputdata/qcasd$VDATE_mm
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/qcmon/qcraob$VDATE_mm* >> $DATA_qcmonYYYY_inputdata/qcraob$VDATE_mm
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/qcmon/qcsatw$VDATE_mm* >> $DATA_qcmonYYYY_inputdata/qcsatw$VDATE_mm
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/qcmon/qcship$VDATE_mm* >> $DATA_qcmonYYYY_inputdata/qcship$VDATE_mm
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/qcmon/shipout$VDATE_mm* >> $DATA_qcmonYYYY_inputdata/shipout$VDATE_mm
done
#######################################################
# Copying the Marine Platform Stats from Production   #
#######################################################
#cp $COM_MARINES_STATS/bracknell.stats $DATA_qcmonYYYY_wmoreports/nospc$VDATE_mm
#Cerr=$?; err_chk
##############################################
# Copying the Complex Quality Control Table  #
##############################################
cp montable$VDATE_mm$VDATE_yy.txt cqcfile
#Cerr=$?; err_chk
sed "1,2d" < cqcfile > cqcfile1
sed -e 's/,/ /g' cqcfile1 > cqcfile2
grep -v "   100     0     0     0     0     0 " cqcfile2 > cqcfile3
grep -v "   110     0     0     0     0     0 " cqcfile3 > $DATA_qcmonYYYY_wmoreports/cqc$VDATE_mm
###################
# QCMON ends      #
###################

####################
# SUMAC4 begins    #
####################
############################################################################
# Joins daily stats of dly*, qcadp_dat* and jet_dat* from production       #
# and put the output into its' approaiate directories to run the SUMAC4    #
# verification                                                             #
############################################################################
for ndays in $days
do
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/sumac4/dly$VDATE_yy$VDATE_mm.* >>  $DATA_sumac4YYYY_inputdata/dsum$VDATE_mm
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/sumac4/qcadp_dat$VDATE_yy$VDATE_mm.* >>  $DATA_sumac4YYYY_inputdata/qcadp$VDATE_mm
done
#################################################################
# Generating the header files and the appropriate data cards    #               
#################################################################
cp $FIXgfs_legacy_verif_v25/sumac4/sel4tmp sel4dat
cp $FIXgfs_legacy_verif_v25/sumac4/wmxctmp wmxcdat
cp $FIXgfs_legacy_verif_v25/sumac4/wgfstmp wgfsdat
echo "$VDATE_YYYY $VDATE_mm $VDATE_YYYYmm_ndays sel4dat sel4avg" >> sel4table
echo "$VDATE_YYYY $VDATE_mm $VDATE_YYYYmm_ndays wmxcdat wmxcards" >> wmxctable
echo "$VDATE_YYYY $VDATE_mm $VDATE_YYYYmm_ndays wgfsdat gfscards" >> gfsctable
$EXECgfs_legacy_verif_v25/verf_datefindr < sel4table >> modeldates
#export err=$?; err_chk
$EXECgfs_legacy_verif_v25/verf_datefindr < wmxctable >> modeldates
#export err=$?; err_chk
$EXECgfs_legacy_verif_v25/verf_datefindr < gfsctable >> modeldates
#export err=$?; err_chk
. modeldates
echo "  "$VDATE_YYYY$VDATE_mm"0100  "$VDATE_YYYY$VDATE_mm$VDATE_YYYYmm_ndays"12   0   0   0   1" > jetparm3
echo "  "$VDATE_YYYY$VDATE_mm"0100  "$VDATE_YYYY$VDATE_mm$VDATE_YYYYmm_ndays"12   0   0" > jetparm
####################
# SUMAC4 ends      #
####################

####################
# ANLVER begins    #
####################
############################################################
# Provides the dates in which the data is being verified   #
############################################################
echo "$VDATE_YYYY$VDATE_mm"0100 "$VDATE_YYYY$VDATE_mm$VDATE_YYYYmm_ndays"12"" > arkv_verdate.in
############################
# Saving the daily stats   #
############################

for ndays in $days
do
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/GFS.$VDATE_mm.in >> GFS.$VDATE_mm.in
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/GFL.$VDATE_mm.in >> GFL.$VDATE_mm.in
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.w250.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.w250.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.w250.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.w250.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.w500.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.w500.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.w500.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.w500.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.w850.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.w850.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.w850.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.w850.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x250t.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x250t.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x250t.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x250t.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x250z.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x250z.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x250z.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x250z.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x500t.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x500t.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x500t.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x500t.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x500z.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x500z.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x500z.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x500z.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x850t.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x850t.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x850t.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x850t.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x850z.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x850z.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.x850z.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.x850z.in12
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.xmslp.in00 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.xmslp.in00
 cat $COMINverf/verd.$VDATE_YYYY$VDATE_mm$ndays/anlver/daily_obs/$VDATE_mm.xmslp.in12 >> $DATA_anlverYYYY_inputdata/$VDATE_mm.xmslp.in12
done
######################################################################
echo 'checking the S1 data for rows of "100.00" --'
echo 'if they occur, you will need to vi the files and change'
echo 'them all to "0.00" '
echo '       '
######################################################################
sed 's/100\.00/  0.00/g' < GFL.$VDATE_mm.in > newgfl
cp newgfl $DATA_anlverYYYY_inputdata/GFL.$VDATE_mm.in
sed 's/100\.00/  0.00/g' < GFS.$VDATE_mm.in > newgfs
cp newgfs $DATA_anlverYYYY_inputdata/GFS.$VDATE_mm.in
####################
# ANLVER ends      #
####################
