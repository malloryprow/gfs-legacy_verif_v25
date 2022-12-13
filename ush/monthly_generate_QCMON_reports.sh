#!/bin/bash

set -xa

VDATE_YYYY=${1:-`date +%Y`}
VDATE_mm=${2:-`date +%m`}
VDATE_yy=${VDATE_yy:-`echo $VDATE_YYYY | cut -c3-4`}

###############################################################################
# This utility script performs the following functions:                       #
#   1. Generates the header files and the appropriate data cards              #
#   2. Joins daily stats from qcmond.s and then stores the output             #
#      data in the approaiate directory                                       #
#   3. Generates monthly summaries reports of aircraft, AMDAT/ASDAR           #
#      and ACARS data                                                         #
#   4. Generates monthly summaries reports of radiosonde observation          #
#      of geopotential height and wind                                        #
#   5. Writes ship-based monthly summary table for ship-based radiosonde      #
#      observations                                                           #
#   6. Reads monthly summary statistics written by raobsum and writes         #
#      table of wind statistics for stations having one level exceeding a     #
#      certain normalized threshhold                                          #
#   7. Gemerates the WMO Monthly Reports                                      #
#   8. Archiving the monthly data                                             #
#   9. Cleaning up the fix files and compressing the data                     #
###############################################################################

################################################################################
# SECTION 1.  QCAIRSUM.s makes separate monthly summaries for aircraft,        #
# AMDAT/ASDAR, and ACARS.  Writes tables giving identification characters,     #
# and the following wind and temperature statistics: mean difference           #
# (OBS - FG), RMS difference, number of obs, number of "gross" differences,    #
# and number of rejections by quality-marker.  In additions, mean speed        #
# difference, RMS vector difference, mean temperature difference, and RMS      #
# temperature difference are computed for all observations within each         #
# five-degree latitude-longitude "square".                                     #
################################################################################
export FORT7="options"
export FORT11="$COMINverm_qcmonYYYY_inputdata/qcair$VDATE_mm"
export FORT12="$COMINverm_qcmonYYYY_inputdata/qcasd$VDATE_mm"
export FORT13="$COMINverm_qcmonYYYY_inputdata/qcacr$VDATE_mm"
export FORT21="airbox$VDATE_mm"
export FORT22="asdbox$VDATE_mm"
export FORT23="acrbox$VDATE_mm"
export FORT31="airsumxx"
export FORT32="asdsumxx"
export FORT33="acrsumxx"
$EXECgfs_legacy_verif_v25/verf_qcairsum 1>qcairsum.out 2>qcairsum.err
#export err=$?; err_chk
############################################
# sort table by descending order of rmsv   #
# sorting aircraft by carrier table ...    #
############################################
tail -n +4 airsumxx > unsorted
  sort -o sorted -r -n -k 3,3 unsorted
    cat sorted >> $DATA_qcmonYYYY_wmoreports/airsum$VDATE_mm
tail -n +4 asdsumxx > unsorted
  sort -o sorted -r -n -k 3,3 unsorted
    cat sorted >> $DATA_qcmonYYYY_wmoreports/asdsum$VDATE_mm
tail -n +4 acrsumxx > unsorted
  sort -o sorted -r -n -k 3,3 unsorted
    cat sorted >> $DATA_qcmonYYYY_wmoreports/acrsum$VDATE_mm
#######################
# SECTION 1. FINISH   #
#######################

###########################################################################
# SECTION 2. RAOBSUM.s generates raw monthly statistics of radiosonde     #
# observation of geopotential height and wind compared to first           #
# guess values at the same location and cycle time.  The number of        #
# reports, the mean and RMS differences of ( OBS - FG ), and number       #
# of "gross" differences are computed separately for 00Z and 12Z          #
# cycles, for each mandatory level.                                       #
###########################################################################
export FORT7="options"
export FORT10="$COMINverm_qcmonYYYY_inputdata/qcraob$VDATE_mm"
export FORT20="raw$VDATE_mm"
$EXECgfs_legacy_verif_v25/verf_raobsum  1>raobsum.out 2>raobsum.err
#export err=$?; err_chk
################################################################
# Raw Statistics of radiosonde and gross differences ...done   #
################################################################
cp raobsum.out raw$VDATE_mm
#######################
# SECTION 2. FINISH   #
#######################

################################################################################
# SECTION 3. SHIPSUM.s writes ship-based monthly summary table for ship-based  #
# radiosonde observations versus concurrent and collocated first guess values  #
# at each mandatory level.  The table provides the ship call sign, pressure    #
# level, number of geopotential height observations, number of "gross"         #
# ( vector obs - FG ) differences, mean of ( UOBS - FG) and ( VOBS - FG )      #
# differences, and RMSV differences.   These statistics are provided separately#
# for 00Z and 12Z observations.  The table is manually edited to delete ship   #
# summaries for which there were generally fewer than 10 observations per      #
# pressure level for each cycle ( 00Z and 12Z ).                               #
################################################################################
export FORT7="options"
export FORT10="$COMINverm_qcmonYYYY_inputdata/qcship$VDATE_mm"
export FORT20="shipsum$VDATE_mm"
$EXECgfs_legacy_verif_v25/verf_shipsum  1>shipsum.out 2>shipsum.err
#export err=$?; err_chk
########################################################
# 'Ship - based radiosonde ...done                     #
# The following puts the correct title                 #
# into shpsum$VDATE_mm                                 #
########################################################
if [ "$VDATE_mm" = "01" ]; then
 sed -e 's/XXXXXXXXX/  JANUARY/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "02" ]; then
  sed -e 's/XXXXXXXXX/ FEBRUARY/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "03" ]; then
  sed -e 's/XXXXXXXXX/    MARCH/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "04" ]; then
  sed -e 's/XXXXXXXXX/    APRIL/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "05" ]; then
  sed -e 's/XXXXXXXXX/      MAY/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "06" ]; then
  sed -e 's/XXXXXXXXX/     JUNE/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "07" ]; then
  sed -e 's/XXXXXXXXX/     JULY/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "08" ]; then
  sed -e 's/XXXXXXXXX/   AUGUST/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "09" ]; then
  sed -e 's/XXXXXXXXX/SEPTEMBER/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "10" ]; then
  sed -e 's/XXXXXXXXX/  OCTOBER/g' shipsum.out > shpsum1
elif [ "$VDATE_mm" = "11" ]; then
  sed -e 's/XXXXXXXXX/ NOVEMBER/g' shipsum.out > shpsum1
else
  sed -e 's/XXXXXXXXX/ DECEMBER/g' shipsum.out > shpsum1
fi
#########################################################
# Removing the old shpsumXX which has the wrong title   #
# Them making shpsumXX with the right title             #
#########################################################
$EXECgfs_legacy_verif_v25/verf_rmships < shpsum1 > shpsum2
#export err=$?; err_chk
grep -v "1 SHIP" shpsum2 > $DATA_qcmonYYYY_wmoreports/shpsum$VDATE_mm
#######################
# SECTION 3. FINISH   #
#######################

################################################################################
# SECTION 4. VSTATS.s and ZSTATS.s read initial monthly summary statistics     #
# written by raobsum.f and writes table of wind statistics for stations having #
# at one level for which the RMSV difference between observation and first 
# guess exceeds a certain normalized ( by pressure level ) threshhold.   Only  #
# the statistics for the lowest such pressure level ( if multiple ) is printed,#
# but the number of offending pressure levels is noted in a separate column.   #
# Other statistics printed are the station block and id number, the cycle ( 00Z#
# or 12Z ), the mean and RMSV (OB - FG) difference, the number of observations #
# ( minium of 10 ), and the number of "gross" differences.                     #
################################################################################
export FORT7="raw$VDATE_mm"
$EXECgfs_legacy_verif_v25/verf_vstats  1>vstats.out 2>vstats.err
#export err=$?; err_chk
#############################################################
#  sort the vstatx file; create vstats (finished product)   #
#############################################################
#head -8 vstats.out > $DATA_qcmonYYYY_wmoreports/vstats$VDATE_mm
tail -n +9 vstats.out > unsorted
  sort -o vsort  -n +0 -1 unsorted
cat vsort >> $DATA_qcmonYYYY_wmoreports/vstats$VDATE_mm
##############################################################################
#    ZSTATS.s reads initial monthly summary statistics written by raobsum.f  #
#    and writes table of wind statistics for stations having at one level    #
#    for which the RMSV difference between observation and first guess       #
#    exceeds a certain normalized ( by pressure level ) threshhold.   Only   #
#    the statistics for the lowest such pressure level ( if multiple ) is    #
#    printed, but the number of offending pressure levels is noted in a      #
#    separate column.   Other statistics printed are the station block and   #
#    id number, the cycle ( 00Z or 12Z ), the mean and RMSV (OB - FG)        #
#    difference, the number of observations ( minium of 10 ), and the number #
#    of "gross" differences.                                                 #
##############################################################################
export FORT7="raw$VDATE_mm"
$EXECgfs_legacy_verif_v25/verf_zstats  1>zstats.out 2>zstats.err
#export err=$?; err_chk
##############################################################################
# Write table of Wind statistics ...created:                                 #
# ZREPORT.s fetchs zstatx file and then sort it creating final zstats file.  #
# script for fetching zstats from rawstats                                   #
##############################################################################
#head -8 zstats.out > $DATA_qcmonYYYY_wmoreports/zstats$VDATE_mm
tail -n +9 zstats.out > unsorted
sort -o zsort  -n +0 -1 unsorted
cat zsort >> $DATA_qcmonYYYY_wmoreports/zstats$VDATE_mm
cat $DATA_qcmonYYYY_wmoreports/zstats$VDATE_mm >> $DATA_qcmonYYYY_wmoreports/raobsm$VDATE_mm
cat blank | cat >> $DATA_qcmonYYYY_wmoreports/raobsm$VDATE_mm
cat $DATA_qcmonYYYY_wmoreports/vstats$VDATE_mm >> $DATA_qcmonYYYY_wmoreports/raobsm$VDATE_mm
#######################
# SECTION 4. FINISH   #
#######################

##########################################
# SECTION 5. Concatenating the reports   #
##########################################
cat rptheadr >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat cqcheadr >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat $COMINverm_qcmonYYYY_wmoreports/cqc$VDATE_mm   >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat blanks   >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat $DATA_qcmonYYYY_wmoreports/raobsm$VDATE_mm >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat blanks   >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat airheadr >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat $DATA_qcmonYYYY_wmoreports/airsum$VDATE_mm >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat blanks   >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat asdheadr >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat $DATA_qcmonYYYY_wmoreports/asdsum$VDATE_mm >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat blanks   >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat acrheadr >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat $DATA_qcmonYYYY_wmoreports/acrsum$VDATE_mm >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat blanks   >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat $DATA_qcmonYYYY_wmoreports/nospc$VDATE_mm  >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat blanks   >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
cat $DATA_qcmonYYYY_wmoreports/shpsum$VDATE_mm >> $DATA_qcmonYYYY_wmoreports/qcmrpt$VDATE_mm
#######################
# SECTION 5. FINISH   #
#######################
