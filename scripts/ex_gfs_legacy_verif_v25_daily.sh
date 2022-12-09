#!/bin/bash
###############################################################################
# Name of Script: ex_gfs_legacy_verif_v25_daily.sh 
# Purpose of Script: This script generates daily verification
# Log history:
###############################################################################

set -x

# Set up DATA directories
export DATAcurfv3gfs_VDATE=$DATA/curfv3gfs/gfs.${VDATE}
mkdir -p $DATAcurfv3gfs_VDATE
export DATAverd=$DATA/verf/verd.${VDATE}
mkdir -p $DATAverd
export DATA_GRIB=$DATAverd/grib
mkdir -p $DATA_GRIB
export DATA_SUMAC41=$DATAverd/sumac4
mkdir -p $DATA_SUMAC41
export DATA_QCMON1=$DATAverd/qcmon
mkdir -p $DATA_QCMON1
export DATA_ANLVER_DATA1=$DATAverd/anlver/daily_obs
export DATA_ANLVER_FCST1=$DATAverd/anlver/daily_forecast
mkdir -p $DATA_ANLVER_DATA1 $DATA_ANLVER_FCST1
export DATA_cqcht=$DATAverd/cqcht
mkdir -p $DATA_cqcht

# Grab data
cd $DATA
${USHgfs_legacy_verif_v25}/get_data.sh ${VDATE}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    mkdir -p $COMOUTcurfv3gfs/gfs.${VDATE}
    for cyc in 00 12; do
        cp -pv ${DATAcurfv3gfs_VDATE}/gfs.t${cyc}z.pgrb2.1p00.anl $COMOUTcurfv3gfs/gfs.${VDATE}
        cp -pv ${DATAcurfv3gfs_VDATE}/gfs.t${cyc}z.pgrb2.1p00.anl.idx $COMOUTcurfv3gfs/gfs.${VDATE}
        for fhr in 000 012 024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216 228 240; do
            cp -pv ${DATAcurfv3gfs_VDATE}/gfs.t${cyc}z.pgrb2.1p00.f${fhr} $COMOUTcurfv3gfs/gfs.${VDATE}
        done
    done
    for cyc in 00 06 12 18; do
        cp -pv ${DATAcurfv3gfs_VDATE}/gfs.t${cyc}z.prepbufr $COMOUTcurfv3gfs/gfs.${VDATE}
        cp -pv ${DATAcurfv3gfs_VDATE}/gfs.t${cyc}z.cqc_events $COMOUTcurfv3gfs/gfs.${VDATE}
        cp -pv ${DATAcurfv3gfs_VDATE}/gfs.t${cyc}z.cqc_stncnt $COMOUTcurfv3gfs/gfs.${VDATE}
        cp -pv ${DATAcurfv3gfs_VDATE}/gfs.t${cyc}z.cqc_stnlst $COMOUTcurfv3gfs/gfs.${VDATE}
    done
fi

# Trim data
cd $DATA
${USHgfs_legacy_verif_v25}/trim_data.sh ${VDATE}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    for cyc in 00 12; do
        cp -pv $DATA_GRIB/ver.gfs.t${cyc}z.pgrbanl $COMOUTverd_grib
        cp -pv ver.gfs.t${cyc}z.pgrbianl $COMOUTverd_grib
        for fhr in 000 012 024 036 048 060 072 084 096 108 120 132 144 156 168 180 192 204 216 228 240; do
            cp -pv $DATA_GRIB/ver.gfs.t${cyc}z.pgrbf${fhr} $COMOUTverd_grib
            cp -pv $DATA_GRIB/ver.gfs.t${cyc}z.pgrbif${fhr} $COMOUTverd_grib
        done
    done
fi

# Run daily verification
#########################################################################
# START FLOW OF CONTROL
# 1)    QCMON - GENERATES DAILY STATS FOR AIRCRAFT, SATWINDS, SHIPREPORTS
# 2)   ANLVER - GENERATES S1 SCORES FOR THE VARIOUS MODELS (00Z)(12Z)
# 3)   SUMAC4 - GENERATES DAILY STATS FOR DAILY SUMAC FILES
#########################################################################
echo "------------------------------------------------"
echo "  Verification - Daily Statistics"
echo "------------------------------------------------"
###############################################################
# Daily forecast files for various models are used to         #
# generate daily statistical scores for the monthly WMO       #
# Reports.                                                    #
###############################################################
export VDATE_YYYY=`echo $VDATE | cut -c1-4`
export VDATE_yy=`echo $VDATE | cut -c3-4`
export VDATE_mm=`echo $VDATE | cut -c5-6`
export VDATE_dd=`echo $VDATE | cut -c7-8`

######################################################################
# SECTION 1.  Computing QCADP                                        #
#             QCADP consists of qualities checks and restrictions    #
#             which are applied to Radiosonde data                   #
######################################################################
cd ${DATA}
${USHgfs_legacy_verif_v25}/compute_QCADP.sh ${VDATE}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA_SUMAC41/tosda_dat${VDATE_yy}${VDATE_mm}"."${VDATE_dd} $COMOUTverd_sumac4
    cp -pv $DATA_SUMAC41/qcadp_dat${VDATE_yy}${VDATE_mm}"."${VDATE_dd} $COMOUTverd_sumac4
fi
#############################
# End of generating QCADP   #
# End of SECTION 1.         #
#############################

#############################################################
# SECTION 2. VERIFICATION OF FORECAST AGAINST RADIOSONDE    #
#            This is known as the SUMAC4 portion which      #
#            uses the QCADP which was generated in          #
#            SECTION 1                                      #
#############################################################
cd ${DATA}
${USHgfs_legacy_verif_v25}/compute_SUMAC4.sh ${VDATE}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA_SUMAC41/dly${VDATE_yy}${VDATE_mm}.${VDATE_dd} $COMOUTverd_sumac4
    cp -pv $DATA_SUMAC41/accumjetstat${VDATE_yy}${VDATE_mm}.${VDATE_dd} $COMOUTverd_sumac4
fi
###########################################################
# End of generating SUMAC4 - Forecast against Radiosonde  #
# End of SECTION 2.                                       #
###########################################################

##########################################################
# SECTION 3. Verification of Forecast against Analyses   #
#            This is known as the ANLVER portion         #
##########################################################
cd ${DATA}
${USHgfs_legacy_verif_v25}/compute_ANLVER.sh ${VDATE}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    for cyc in 00 12; do
        cp -pv $DATA_ANLVER_FCST1/${VDATE}${cyc}.gfsm.out $COMINverd_anlver_forecast
        cp -pv $DATA_ANLVER_FCST1/${VDATE}${cyc}.gfs.out $COMINverd_anlver_forecast
        cp -pv $DATA_ANLVER_FCST1/s1gfs.${VDATE}${cyc} $COMINverd_anlver_forecast
        cp -pv $DATA_ANLVER_FCST1/s1gfsm.${VDATE}${cyc} $COMINverd_anlver_forecast
        cp -pv $DATA_ANLVER_FCST1/wgfs.${VDATE}${cyc} $COMINverd_anlver_forecast
        cp -pv $DATA_ANLVER_FCST1/xgfs.${VDATE}${cyc} $COMINverd_anlver_forecast
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.w250.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.w500.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.w850.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.x250t.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.x250z.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.x500t.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.x500z.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.x850t.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.x850z.in${cyc} $COMINverd_anlver_obs
        cp -pv $DATA_ANLVER_DATA1/${VDATE_mm}.xmslp.in${cyc} $COMINverd_anlver_obs
    done
    cp -pv $DATA_ANLVER_DATA1/GFL.${VDATE_mm}.in $COMINverd_anlver_obs
    cp -pv $DATA_ANLVER_DATA1/GFS.${VDATE_mm}.in $COMINverd_anlver_obs
fi
###########################################################
# End of generating ANLVER - Forecast against Analyses    #
# End of SECTION 3.                                       #
###########################################################

##########################################################
# SECTION 4. Daily statistics for the Monthly Quality    #
#            Monitoring Report - Input argument is       #
#            prepbufr file for a given cycle             #
#            This is known as the QCMON portion          #
##########################################################
cd ${DATA}
${USHgfs_legacy_verif_v25}/compute_QCMON.sh ${VDATE}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA_QCMON1/asraob${VDATE_mm}${VDATE_dd} $COMOUTverd_qcmon
    cp -pv $DATA_QCMON1/qcacr${VDATE_mm}${VDATE_dd} $COMOUTverd_qcmon
    cp -pv $DATA_QCMON1/qcair${VDATE_mm}${VDATE_dd} $COMOUTverd_qcmon
    cp -pv $DATA_QCMON1/qcasd${VDATE_mm}${VDATE_dd} $COMOUTverd_qcmon
    cp -pv $DATA_QCMON1/qcraob${VDATE_mm}${VDATE_dd} $COMOUTverd_qcmon
    cp -pv $DATA_QCMON1/qcsatw${VDATE_mm}${VDATE_dd} $COMOUTverd_qcmon
    cp -pv $DATA_QCMON1/qcship${VDATE_mm}${VDATE_dd} $COMOUTverd_qcmon
    cp -pv $DATA_QCMON1/shipout${VDATE_mm}${VDATE_dd} $COMOUTverd_qcmon
fi
###########################################################
# End of generating Upperair and Shipbase observations    #
# which makes up the Quality Monitoring Report            #
# End of SECTION 4.                                       #
###########################################################


#######################################################
# SECTION 5. Bill Collins' CQC  monthly verification  #
#            This script copies the following files:  #
#                gfs.t${cyc}z.cqc_events,             #
#                gfs.t${cyc}z.cqc_stncnt,             #
#                and gfs.t${cyc}z.cqc_ stnlst         #
#            files for 00z, 06z, 12z & 18z            #
#######################################################
cd ${DATA}
${USHgfs_legacy_verif_v25}/compute_CQCHT.sh ${VDATE}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    for cyc in 00 06 12 18; do
        cp -pv $DATA_cqcht/cqc_${VDATE_YYYY}${VDATE_mm}${VDATE_dd}${cyc}_events.fnl $COMOUTverd_cqcht
        cp -pv $DATA_cqcht/cqc_${VDATE_YYYY}${VDATE_mm}${VDATE_dd}${cyc}_stncnt.fnl $COMOUTverd_cqcht
        cp -pv $DATA_cqcht/cqc_${VDATE_YYYY}${VDATE_mm}${VDATE_dd}${cyc}_stnlst.fnl $COMOUTverd_cqcht
    done
fi
#################################################
# End of generating CQC monthly verification    #
# which provides 1 table for the Quality        #
# Monitoring Report                             #
# End of SECTION 5.                             #
#################################################
