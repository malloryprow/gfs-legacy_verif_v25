#!/bin/bash
###############################################################################
# Name of Script: ex_gfs_legacy_verif_v25_monthly.sh 
# Purpose of Script: This script generates monthly verification
# Log history:
###############################################################################

set -x

# Set up DATA directories
export DATA_emcdatabase=$DATA/emc_database
mkdir -p $DATA_emcdatabase
export DATA_emcdatabase_YYYYmm=$DATA_emcdatabase/${VDATE_YYYY}_${VDATE_mm}
mkdir -p $DATA_emcdatabase_YYYYmm
export DATA_anlverYYYY=$DATA/anlver_$VDATE_YYYY
mkdir -p $DATA_anlverYYYY
export DATA_anlverYYYY_inputdata=$DATA_anlverYYYY/input_data
mkdir -p $DATA_anlverYYYY_inputdata
export DATA_anlverYYYY_wmoreports=$DATA_anlverYYYY/wmo_reports
mkdir -p $DATA_anlverYYYY_wmoreports
export DATA_cqchtYYYY=$DATA/cqcht_$VDATE_YYYY
mkdir -p $DATA_cqchtYYYY
export DATA_cqchtYYYY_mm=$DATA_cqchtYYYY/cqcht_$VDATE_mm
mkdir -p $DATA_cqchtYYYY_mm
export DATA_cqchtYYYY_mm_inputdata=$DATA_cqchtYYYY_mm/input_data
mkdir -p $DATA_cqchtYYYY_mm_inputdata
export DATA_cqchtYYYY_mm_cqchtreports=$DATA_cqchtYYYY_mm/cqcht_reports
mkdir -p $DATA_cqchtYYYY_mm_cqchtreports
export DATA_qcmonYYYY=$DATA/qcmon_$VDATE_YYYY
mkdir -p $DATA_qcmonYYYY
export DATA_qcmonYYYY_inputdata=$DATA_qcmonYYYY/input_data
mkdir -p $DATA_qcmonYYYY_inputdata
export DATA_qcmonYYYY_wmoreports=$DATA_qcmonYYYY/wmo_reports
mkdir -p $DATA_qcmonYYYY_wmoreports
export DATA_s1YYYY=$DATA/s1_$VDATE_YYYY
mkdir -p $DATA_s1YYYY
export DATA_sumac4YYYY=$DATA/sumac4_$VDATE_YYYY
mkdir -p $DATA_sumac4YYYY
export DATA_sumac4YYYY_inputdata=$DATA_sumac4YYYY/input_data
mkdir $DATA_sumac4YYYY_inputdata
export DATA_sumac4YYYY_wmoreports=$DATA_sumac4YYYY/wmo_reports
mkdir -p $DATA_sumac4YYYY_wmoreports
export DATA_MONTHLY=$DATA/MONTHLY
mkdir -p $DATA_MONTHLY

# Clear out old COMOUT data????

# Get month name
case ${VDATE_mm} in

    01) MonWord=January   ;;

    02) MonWorg=February  ;;

    03) MonWord=March     ;;

    04) MonWord=April     ;;

    05) MonWord=May       ;;

    06) MonWord=June      ;;

    07) MonWord=July      ;;

    08) MonWord=August    ;;

    09) MonWord=September ;;

    10) MonWord=October   ;;

    11) MonWord=November  ;;

    12) MonWord=December  ;;

  esac

# Run monthly verification
#########################################################################
# START FLOW OF CONTROL
# 1)    QCMON - GENERATES DAILY STATS FOR AIRCRAFT, SATWINDS, SHIPREPORTS
# 2)   ANLVER - GENERATES S1 SCORES FOR THE VARIOUS MODELS (00Z)(12Z)
# 3)   SUMAC4 - GENERATES DAILY STATS FOR DAILY SUMAC FILES
#########################################################################

######################################################################
# Compiling daily SUMAC4, ANALVER and QCMON stats and generating     # 
# fix files and header for monthly WMO reports                       #
######################################################################

# Process monthly data
echo "------------------------------------------------"
echo "  Monthly Verification - Data Processing"
echo "------------------------------------------------"
cd $DATA
${USHgfs_legacy_verif_v25}/monthly_process_data.sh ${VDATE_YYYY} ${VDATE_mm}
#export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA_qcmonYYYY_wmoreports/sxn$VDATE_mm".txt" $COMOUTverm_qcmonYYYY_wmoreports
    cp -pv $DATA_cqchtYYYY_mm_inputdata/cqc_events.fnl $COMOUTverm_cqchtYYYY_mm_inputdata
    cp -pv $DATA_cqchtYYYY_mm_inputdata/cqc_stncnt.fnl $COMOUTverm_cqchtYYYY_mm_inputdata
    cp -pv $DATA_cqchtYYYY_mm_inputdata/cqc_stnlst.fnl $COMOUTverm_cqchtYYYY_mm_inputdata
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/all96.txt $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/us96cqcht.txt $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/us96cqcht.stn.txt $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/canada96.txt $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/saf96.txt $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/stnlist.txt $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/bias_fnl.txt $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/montable$VDATE_mm$VDATE_yy $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_cqchtYYYY_mm_cqchtreports/baseline.txt $COMOUTverm_cqchtYYYY_mm_cqchtreports
    cp -pv $DATA_qcmonYYYY_inputdata/asraob$VDATE_mm $COMOUTverm_qcmonYYYY_inputdata
    cp -pv $DATA_qcmonYYYY_inputdata/qcacr$VDATE_mm $COMOUTverm_qcmonYYYY_inputdata
    cp -pv $DATA_qcmonYYYY_inputdata/qcair$VDATE_mm $COMOUTverm_qcmonYYYY_inputdata
    cp -pv $DATA_qcmonYYYY_inputdata/qcasd$VDATE_mm $COMOUTverm_qcmonYYYY_inputdata
    cp -pv $DATA_qcmonYYYY_inputdata/qcraob$VDATE_mm $COMOUTverm_qcmonYYYY_inputdata
    cp -pv $DATA_qcmonYYYY_inputdata/qcsatw$VDATE_mm $COMOUTverm_qcmonYYYY_inputdata
    cp -pv $DATA_qcmonYYYY_inputdata/qcship$VDATE_mm $COMOUTverm_qcmonYYYY_inputdata
    cp -pv $DATA_qcmonYYYY_inputdata/shipout$VDATE_mm $COMOUTverm_qcmonYYYY_inputdata
    cp -pv $DATA_qcmonYYYY_wmoreports/cqc$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports
    cp -pv $DATA_sumac4YYYY_inputdata/dsum$VDATE_mm $COMOUTverm_sumac4YYYY_inputdata
    cp -pv $DATA_sumac4YYYY_inputdata/qcadp$VDATE_mm $COMOUTverm_sumac4YYYY_inputdata
    for cyc in 00 12; do
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.w250.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.w500.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.w850.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.x250t.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.x500t.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.x850t.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.x250z.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.x500z.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.x850z.in${cyc} $COMOUTverm_anlverYYYY_inputdata
        cp -pv $DATA_anlverYYYY_inputdata/$VDATE_mm.xmslp.in${cyc} $COMOUTverm_anlverYYYY_inputdata
    done
    cp -pv $DATA_anlverYYYY_inputdata/GFL.$VDATE_mm.in $COMOUTverm_anlverYYYY_inputdata
    cp -pv $DATA_anlverYYYY_inputdata/GFS.$VDATE_mm.in $COMOUTverm_anlverYYYY_inputdata
fi
if [ $SENDDBN = YES ]; then
       export dbnjob=S1_verf15
       #$DBNROOT/bin/dbn_alert TEXT VERF_CQCHT $dbnjob $COMOUTverm_cqchtYYYY_mm_cqchtreports/all96.txt
       #$DBNROOT/bin/dbn_alert TEXT VERF_CQCHT $dbnjob $COMOUTverm_cqchtYYYY_mm_cqchtreports/us96cqcht.txt
       #$DBNROOT/bin/dbn_alert TEXT VERF_CQCHT $dbnjob $COMOUTverm_cqchtYYYY_mm_cqchtreports/us96cqcht.stn.txt
       #$DBNROOT/bin/dbn_alert TEXT VERF_CQCHT $dbnjob $COMOUTverm_cqchtYYYY_mm_cqchtreports/canada96.txt
       #$DBNROOT/bin/dbn_alert TEXT VERF_CQCHT $dbnjob $COMOUTverm_cqchtYYYY_mm_cqchtreports/saf96.txt
       #$DBNROOT/bin/dbn_alert TEXT VERF_CQCHT $dbnjob $COMOUTverm_cqchtYYYY_mm_cqchtreports/stnlist.txt
       #$DBNROOT/bin/dbn_alert TEXT VERF_CQCHT $dbnjob $COMOUTverm_cqchtYYYY_mm_cqchtreports/baseline.txt
       export subject="CQCHT Verification Stats"
       #export recipients="NCEP.list.spa-helpdesk@noaa.gov"
       export recipients="NCEP.CQCHTstats@noaa.gov"
       export filelist="$COMOUTverm_cqchtYYYY_mm_cqchtreports/all96.txt -a $COMOUTverm_cqchtYYYY_mm_cqchtreports/us96cqcht.txt -a \
       $COMOUTverm_cqchtYYYY_mm_cqchtreports/us96cqcht.stn.txt -a $COMOUTverm_cqchtYYYY_mm_cqchtreports/canada96.txt -a \
       $COMOUTverm_cqchtYYYY_mm_cqchtreports/saf96.txt -a $COMOUTverm_cqchtYYYY_mm_cqchtreports/stnlist.txt -a \
       $COMOUTverm_cqchtYYYY_mm_cqchtreports/baseline.txt"
       echo "Attached are the $subject" | mail -s "$subject" -a $filelist "$recipients"
fi

# Generate QCMON reports
echo "------------------------------------------------"
echo "  Monthly Verification - QCMON Processing"
echo "------------------------------------------------"
cd $DATA
${USHgfs_legacy_verif_v25}/monthly_generate_QCMON_reports.sh ${VDATE_YYYY} ${VDATE_mm}
#export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA_qcmonYYYY_wmoreports/airsum$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports
    cp -pv $DATA_qcmonYYYY_wmoreports/asdsum$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports
    cp -pv $DATA_qcmonYYYY_wmoreports/acrsum$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports
    cp -pv $DATA_qcmonYYYY_wmoreports/shpsum$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports
    cp -pv $DATA_qcmonYYYY_wmoreports/vstats$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports
    cp -pv $DATA_qcmonYYYY_wmoreports/zstats$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports 
    cp -pv $DATA_qcmonYYYY_wmoreports/raobsm$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports
    cp -pv $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm $COMOUTverm_qcmonYYYY_wmoreports
fi
if [ $SENDDBN = YES ]; then
      export dbnjob=S1_verf15
      #$DBNROOT/bin/dbn_alert TEXT VERF_QCREPORT $dbnjob $COMOUTverm_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
      export subject="QC Report Stats"
      #export recipients="NCEP.list.spa-helpdesk@noaa.gov"
      export recipients="NCEP.QCReport@noaa.gov"
      export filelist="$COMOUTverm_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm"
      echo "Attached are the $subject" | mail -s "$subject" -a $filelist "$recipients"
fi

# Generate ANLVER reports
echo "------------------------------------------------"
echo "  Monthly Verification - ANLVER Processing"
echo "------------------------------------------------"
cd $DATA
${USHgfs_legacy_verif_v25}/monthly_generate_ANLVER_reports.sh ${VDATE_YYYY} ${VDATE_mm}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA_sumac4YYYY_wmoreports/ryhwmx${VDATE_mm} $COMOUTverm_sumac4YYYY_wmoreports
    cp -pv $DATA_anlverYYYY_inputdata/s1max.GFL.${VDATE_mm} $COMOUTverm_anlverYYYY_inputdata
    cp -pv $DATA_anlverYYYY_inputdata/s1tru.GFL.${VDATE_mm} $COMOUTverm_anlverYYYY_inputdata
    cp -pv $DATA_anlverYYYY_inputdata/s1max.GFS.${VDATE_mm} $COMOUTverm_anlverYYYY_inputdata
    cp -pv $DATA_anlverYYYY_inputdata/s1tru.GFS.${VDATE_mm} $COMOUTverm_anlverYYYY_inputdata
    cp -pv $DATA_anlverYYYY_wmoreports/s1GFL500 $COMOUTverm_anlverYYYY_wmoreports
    cp -pv $DATA_anlverYYYY_wmoreports/s1GFLmsl $COMOUTverm_anlverYYYY_wmoreports
    cp -pv $DATA_anlverYYYY_wmoreports/s1GFS500 $COMOUTverm_anlverYYYY_wmoreports
    cp -pv $DATA_anlverYYYY_wmoreports/s1GFSmsl $COMOUTverm_anlverYYYY_wmoreports
    cp -pv $DATA_anlverYYYY_wmoreports/s2GFL500 $COMOUTverm_anlverYYYY_wmoreports
    cp -pv $DATA_anlverYYYY_wmoreports/s2GFLmsl $COMOUTverm_anlverYYYY_wmoreports
    cp -pv $DATA_anlverYYYY_wmoreports/s2GFS500 $COMOUTverm_anlverYYYY_wmoreports
    cp -pv $DATA_anlverYYYY_wmoreports/s2GFSmsl $COMOUTverm_anlverYYYY_wmoreports
    cp -pv $DATA_s1YYYY/s1gfs36500 $COMOUTverm_s1YYYY
    cp -pv $DATA_s1YYYY/s1gfs72500 $COMOUTverm_s1YYYY
fi

# Generate SUMAC4 reports
echo "------------------------------------------------"
echo "  Monthly Verification - SUMAC4 Processing"
echo "------------------------------------------------"
cd $DATA
${USHgfs_legacy_verif_v25}/monthly_generate_SUMAC4_reports.sh ${VDATE_YYYY} ${VDATE_mm}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA_sumac4YYYY_wmoreports/wmxgfs$VDATE_mm $COMOUTverm_sumac4YYYY_wmoreports
    cp -pv $DATA_sumac4YYYY_wmoreports/wmxrpt$VDATE_mm $COMOUTverm_sumac4YYYY_wmoreports
    cp -pv $DATA_emcdatabase_YYYYmm/gfs_${VDATE_YYYY}${VDATE_mm}.vsdb $COMOUTverm_emcdatabase_YYYYmm
    cp -pv $DATA_emcdatabase_YYYYmm/ukm_${VDATE_YYYY}${VDATE_mm}.vsdb $COMOUTverm_emcdatabase_YYYYmm
fi
if [ $SENDDBN = YES ]; then
       export dbnjob=S1_verf15
       #$DBNROOT/bin/dbn_alert TEXT VERF_NAM $job $COMOUTverm_sumac4YYYY_wmoreports/wmxnam$VDATE_mm
       #$DBNROOT/bin/dbn_alert TEXT VERF_GFS $job $COMOUTverm_sumac4YYYY_wmoreports/wmxrpt$VDATE_mm
       export subject="NAM Verification Stats"
       #export recipients="NCEP.list.spa-helpdesk@noaa.gov"
       export recipients="NCEP.NAMStats@noaa.gov"
       export filelist="$COMOUTverm_sumac4YYYY_wmoreports/wmxnam$VDATE_mm"
       echo "Attached are the $subject" | mail -s "$subject" -a $filelist "$recipients"
       export subject="GFS Verification Stats"
       export recipients="NCEP.GFSstats@noaa.gov"
       #export recipients="NCEP.list.spa-helpdesk@noaa.gov"
       export filelist="$COMOUTverm_sumac4YYYY_wmoreports/wmxrpt$VDATE_mm"
       echo "Attached are the $subject" | mail -s "$subject" -a $filelist "$recipients"
fi

# Update Monthly Archive
echo "------------------------------------------------"
echo "  Monthly Verification - Update Historic Archive"
echo "------------------------------------------------"
cd $DATA
python ${USHgfs_legacy_verif_v25}/monthly_update_historic_archive.py ${VDATE_YYYY} ${VDATE_mm}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    cp -pv $DATA/s1_historic_archive_monthly_rawdata.txt $COMOUThistoricarch
fi

# Run Monthly Final Processing
echo "------------------------------------------------"
echo "  Monthly Verification - Final Processing"
echo "------------------------------------------------"
cd $DATA
python ${USHgfs_legacy_verif_v25}/monthly_run_final_processes.py ${VDATE_YYYY} ${VDATE_mm}
export err=$?; err_chk
