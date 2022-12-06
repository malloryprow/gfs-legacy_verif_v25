#!/bin/bash
###############################################################################
# Name of Script: ex_gfs_legacy_verif_v25_daily.sh 
# Purpose of Script: This script generates daily verification
# Log history:
###############################################################################

set -x

# Grab data
${USHgfs_legacy_verif_v25}/get_data.sh ${VDATE}
export err=$?; err_chk
if [ $SENDCOM = YES ]; then
    mkdir -p $COMOUT/output/curfv3gfs/gfs.${VDATE}
    for curfv3gfs_file in ${DATA}/output/curfv3gfs/gfs.${VDATE}/*; do
        cp -pv $curfv3gfs_file $COMOUT/output/curfv3gfs/gfs.${VDATE}/.
    done
fi
