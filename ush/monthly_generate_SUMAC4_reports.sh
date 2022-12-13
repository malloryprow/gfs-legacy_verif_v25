#!/bin/bash

set -xa

VDATE_YYYY=${1:-`date +%Y`}
VDATE_mm=${2:-`date +%m`}
VDATE_yy=${VDATE_yy:-`echo $VDATE_YYYY | cut -c3-4`}

#######################################################################
# SECTION 1. Generating the Monthly Average Verification Statistics   #
#######################################################################
export FORT10="$COMINverm_sumac4YYYY_inputdata/dsum$VDATE_mm"
export FORT15="input"
$EXECgfs_legacy_verif_v25/verf_selecsum < sel4avg 1>selecsum.out 2>selecsum.err
#export err=$?; err_chk
#########################################################################
# NOTE: monavg.$mm is printout for diagnostic purposes only.  File      #
# contains the actual monthly average verification statistics (binary)  #
# Makes ascii copy of binary monthly stats                              #
#########################################################################
cat <<\parmEOF >parmcard
  MODEL=           AREA=          FD= 1970010100  LD= 2999123112 P=4 C=0
                                                                 P=0 C=9
parmEOF
export FORT10="input"
$EXECgfs_legacy_verif_v25/verf_selecsum <parmcard 1>arcx 2>$DATA/selecsum.err
#export err=$?; err_chk
####################################################################
# Makestime-series files for WMO using daily stats generated from  #
# the GFS model                                                    #
####################################################################
models="gfs"
for model in ${models}
do
  cp $model"cards" parmcard
  export FORT10="$COMINverm_sumac4YYYY_inputdata/dsum$VDATE_mm"
  export FORT15="sumac15"
  export FORT20="w$model"time""
  export FORT25="sumac25"
  export FORT30="sumac30"
  $EXECgfs_legacy_verif_v25/verf_selecsum <parmcard 1>selecsum.out 2>$DATA/selecsum.err
  #export err=$?; err_chk
  ###############################################################
  # Calculates monthly average for GFS stats for WMO            #
  # from the time_series and prints them                        #
  ###############################################################
  cp $FIXgfs_legacy_verif_v25/sumac4/$model"4wmx" parmcard
  export FORT10="w$model"time""
  $EXECgfs_legacy_verif_v25/verf_wmogfs <parmcard 1>wmomod.out 2>wmomod.err
  #export err=$?; err_chk
  cat wmomod.out | cut -c2-80 > wmx$model
done
cp wmxgfs $DATA_sumac4YYYY_wmoreports/wmxgfs$VDATE_mm
cat $COMINverm_sumac4YYYY_wmoreports/ryhwmx$VDATE_mm >> $DATA_sumac4YYYY_wmoreports/wmxrpt$VDATE_mm
cat $DATA_sumac4YYYY_wmoreports/wmxgfs$VDATE_mm >>$DATA_sumac4YYYY_wmoreports/wmxrpt$VDATE_mm
#################################################
# generating files for EMC (VLCEK)  directories #
#################################################
mnames='LRG GFS UKM'
# Variables to change for monthly stats....
M=$VDATE_mm
D="01"
H="00"
m=$VDATE_mm
d=$(cal $VDATE_mm $VDATE_YYYY | awk 'NF {DAYS = $NF}; END {print DAYS}')
h="12"
for mx in $mnames
do
cat >parmcard.$mx <<EOF
  MODEL=$mx        AREA=          FD= $YYYY$M$D$H  LD= $YYYY$m$d$h P=4 C=0
                                                                 P=1 C=9
EOF
 # run selecsum
 export FORT10="input"
 export FORT15="input.c"
 export FORT60="vsdb"
 $EXECgfs_legacy_verif_v25/verf_emcsum <parmcard.$mx 1>selecsum.$mx.out 2>selecsum.$mx.err
 #export err=$?; err_chk
 cp *.vsdb $DATA_emcdatabase_YYYYmm
done
