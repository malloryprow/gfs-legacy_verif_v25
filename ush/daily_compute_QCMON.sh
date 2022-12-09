#!/bin/bash

set -xa

VDATE=${1:-`date +%Y%m%d`}
export VDATE_YYYY=`echo $VDATE | cut -c1-4`
export VDATE_yy=`echo $VDATE | cut -c3-4`
export VDATE_mm=`echo $VDATE | cut -c5-6`
export VDATE_dd=`echo $VDATE | cut -c7-8`

##########################################################
# SECTION 4. Daily statistics for the Monthly Quality    #
#            Monitoring Report - Input argument is       #
#            prepbufr file for a given cycle             #
#            This is known as the QCMON portion          #
##########################################################
###############################
# Looping on the Cycle time   #
###############################

for ct in 00 06 12 18
do
  if [ -f $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.prepbufr ]; then
    cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.prepbufr $DATA/bufr
    #export err=$?; err_chk
  else
    echo "Prepbufr file not found"
    err=1
    #export err=$?; err_chk
  fi
  ########################
  # upper air data type  #
  ########################
  data=`echo uair`
  echo ${data} > datatype
  #pgm="hrly_qcmon"
  #export pgm;. prep_step
  export FORT10="bufr"
  export FORT15="qcacr"
  #startmsg
  $EXECgfs_legacy_verif_v25/hrly_qcmon < datatype 1> qcaircar.out 2> qcaircar.err
  #export err=$?; err_chk
  cat qcacr >> $DATA_QCMON1/qcacr$VDATE_mm$VDATE_dd
  #pgm="hrly_qcmon"
  #export pgm;. prep_step
  export FORT10="bufr"
  export FORT20="qcair"
  export FORT25="qcasd"
  #startmsg
  $EXECgfs_legacy_verif_v25/hrly_qcmon < datatype 1> qcaircft.out 2> qcaircft.err
  #export err=$?; err_chk
  cat qcair >> $DATA/qcair$VDATE_dd
  cat qcasd >> $DATA/qcasd$VDATE_dd
  #pgm="hrly_qcmon"
  #export pgm;. prep_step
  export FORT10="bufr"
  export FORT30="qcsatw"
  #startmsg
  $EXECgfs_legacy_verif_v25/hrly_qcmon < datatype 1> qcsatwnd.out 2> qcsatwnd.err
  #export err=$?; err_chk
  cat qcsatw >> $DATA_QCMON1/qcsatw$VDATE_mm$VDATE_dd
  echo "Sort aircraft and asdar files to remove duplicates"
  sort +0 -6 qcair$VDATE_dd > sort1
  sort -um +1 -6 sort1 > sort2
  cat sort2 >> $DATA_QCMON1/qcair$VDATE_mm$VDATE_dd
  rm -f sort1 sort2 qcair$VDATE_dd
  sort +0 -6 qcasd$VDATE_dd > sort3
  sort -um +1 -6 sort3 > sort4
  cat sort4 >> $DATA_QCMON1/qcasd$VDATE_mm$VDATE_dd
  rm -f sort3 sort4 qcasd$VDATE_dd
  if [ $ct -eq 00 -o $ct -eq 12 ]
  then
    #pgm="hrly_qcmon"
    #export pgm;. prep_step
    export FORT10="bufr"
    export FORT34="qcraob$ct"
    #startmsg
    $EXECgfs_legacy_verif_v25/hrly_qcmon < datatype 1> asraob  2> qcraob.err
    #export err=$?; err_chk
    cat qcraob${ct}  >> $DATA_QCMON1/qcraob$VDATE_mm$VDATE_dd
    cat asraob >> $DATA_QCMON1/asraob$VDATE_mm$VDATE_dd
  fi
done

#############################################################################
# This generates U and V components of the wind base upon daily shipbase    #
# radiosondes observations minus the first guess increment at 00Z and 12Z   #
# for the monthly Quality Monitoring Report.                                #
#############################################################################
for ct in 00 12
do
  if [ -f $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.prepbufr ]; then
    cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.prepbufr $DATA/bufr
    #export err=$?; err_chk
  else
    echo "Prepbufr file not found"
    err=1
    #export err=$?; err_chk
  fi
  data=`echo ship`
  echo ${data} > datatype
  #pgm="hrly_qcmon"
  #export pgm;. prep_step
  export FORT12="bufr"
  export FORT24="shipraob${ct}"
  #startmsg
  $EXECgfs_legacy_verif_v25/hrly_qcmon < datatype 1> shipraob.out 2> shipraob.err
  #export err=$?; err_chk
  cat shipraob${ct} >> $DATA_QCMON1/qcship$VDATE_mm$VDATE_dd
  cat shipraob.out >> $DATA_QCMON1/shipout$VDATE_mm$VDATE_dd
done

###########################################################
# End of generating Upperair and Shipbase observations    #
# which makes up the Quality Monitoring Report            #
# End of SECTION 4.                                       #
###########################################################
