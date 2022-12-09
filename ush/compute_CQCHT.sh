#!/bin/bash

set -xa

VDATE=${1:-`date +%Y%m%d`}
export VDATE_YYYY=`echo $VDATE | cut -c1-4`
export VDATE_yy=`echo $VDATE | cut -c3-4`
export VDATE_mm=`echo $VDATE | cut -c5-6`
export VDATE_dd=`echo $VDATE | cut -c7-8`

#######################################################
# SECTION 5. Bill Collins' CQC  monthly verification  #
#            This script copies the following files:  #
#                gfs.t${cyc}z.cqc_events,             #
#                gfs.t${cyc}z.cqc_stncnt,             #
#                and gfs.t${cyc}z.cqc_ stnlst         #
#            files for 00z, 06z, 12z & 18z            #
#######################################################

###############################
# Looping on the Cycle time   #
###############################

for ct in 00 06 12 18
do
  cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.cqc_events  $DATA_cqcht/cqc_${VDATE_YYYY}${VDATE_mm}${VDATE_dd}${ct}_events.fnl
  cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.cqc_stncnt  $DATA_cqcht/cqc_${VDATE_YYYY}${VDATE_mm}${VDATE_dd}${ct}_stncnt.fnl
  cp $COMINcurfv3gfs/gfs.${VDATE}/gfs.t${ct}z.cqc_stnlst  $DATA_cqcht/cqc_${VDATE_YYYY}${VDATE_mm}${VDATE_dd}${ct}_stnlst.fnl
done

#################################################
# End of generating CQC monthly verification    #
# which provides 1 table for the Quality        #
# Monitoring Report                             #
# End of SECTION 5.                             #
#################################################
